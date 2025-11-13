months_per_quarter <- 3L

label_horizon <- function(step) {
  dplyr::case_when(
    step == 1L ~ "1-step ahead",
    step == 4L ~ "1-year ahead",
    TRUE ~ "Other"
  )
}

coverage_label <- function(extra_months) {
  dplyr::case_when(
    extra_months == 0L ~ "Cutoff only",
    extra_months == 1L ~ "Cutoff +1m",
    extra_months == 2L ~ "Cutoff +2m",
    extra_months == 3L ~ "Cutoff +3m",
    TRUE ~ sprintf("Cutoff +%dm", extra_months)
  )
}

add_months <- function(year_month, n) {
  stopifnot(length(year_month) == 2L)
  total_months <- as.integer(year_month[1]) * 12L + (as.integer(year_month[2]) - 1L) + as.integer(n)
  if (total_months < 0L) {
    stop("Month arithmetic produced a negative index; check the inputs.")
  }
  new_year <- total_months %/% 12L
  new_month <- (total_months %% 12L) + 1L
  c(new_year, new_month)
}

collect_forecast_tbl <- function(target_vars, horizon_steps, model_label, generator_fn, warn_context = NULL, warn_on_all_na = TRUE) {
  purrr::map_dfr(target_vars, function(var) {
    preds <- generator_fn(var)
    if (!length(preds)) {
      preds <- rep(NA_real_, length(horizon_steps))
    }
    if (length(preds) != length(horizon_steps)) {
      warning(sprintf("%s for %s returned %d values (expected %d)%s",
        model_label,
        var,
        length(preds),
        length(horizon_steps),
        if (!is.null(warn_context)) paste0(" ", warn_context) else ""
      ), call. = FALSE)
      preds <- rep(NA_real_, length(horizon_steps))
    }
    if (warn_on_all_na && all(is.na(preds))) {
      warning(sprintf("%s for %s returned all NAs%s",
        model_label,
        var,
        if (!is.null(warn_context)) paste0(" ", warn_context) else ""
      ), call. = FALSE)
    }
    tibble::tibble(
      variable = var,
      step_ahead = horizon_steps,
      prediction = preds,
      model = model_label
    )
  })
}

run_forecaster_with_timing <- function(target_vars, horizon_steps, model_label, generator_fn, warn_context = NULL, warn_on_all_na = TRUE) {
  timed <- measure_elapsed({
    collect_forecast_tbl(
      target_vars = target_vars,
      horizon_steps = horizon_steps,
      model_label = model_label,
      generator_fn = generator_fn,
      warn_context = warn_context,
      warn_on_all_na = warn_on_all_na
    )
  })
  list(predictions = timed$result, elapsed = timed$elapsed)
}

forecast_mfvar <- function(
    q_train_adj,
    monthly_train,
    transforms,
    n_lags,
    horizon_quarters,
    target_vars,
    seed = 123L,
    return_model = FALSE,
    extract_states = FALSE) {
  Y_train <- build_Y(q_train_adj, monthly_train)

  mod <- tryCatch(
    estimate_mfvar_model(
      Y_train,
      n_lags,
      n_fcst = horizon_quarters * months_per_quarter,
      seed = seed
    ),
    error = function(err) {
      warning(sprintf("MF-VAR estimation failed: %s", conditionMessage(err)), call. = FALSE)
      NULL
    }
  )

  if (is.null(mod)) {
    fallback <- purrr::map_dfr(target_vars, function(var) {
      tibble::tibble(variable = var, step_ahead = seq_len(horizon_quarters), prediction = NA_real_)
    })
    result <- list(predictions = fallback, latent_states = NULL)
    if (return_model) result$model <- NULL
    return(result)
  }

  fc <- predict(mod, aggregate_fcst = TRUE, pred_bands = 0.8) |>
    dplyr::filter(variable %in% target_vars) |>
    dplyr::arrange(variable, time) |>
    dplyr::group_by(variable) |>
    dplyr::mutate(step_ahead = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::filter(step_ahead <= horizon_quarters) |>
    dplyr::mutate(
      prediction = restore_series_values(
        median,
        variable,
        compute_time_index(nrow(q_train_adj), step_ahead),
        transforms
      )
    ) |>
    dplyr::select(variable, step_ahead, prediction)

  latent_states <- NULL
  if (isTRUE(extract_states)) {
    latent_states <- tryCatch(
      extract_latent_states(mod, summary = "mean"),
      error = function(err) {
        warning(sprintf("Latent state extraction failed: %s", conditionMessage(err)), call. = FALSE)
        NULL
      }
    )
  }

  result <- list(predictions = fc, latent_states = latent_states)
  if (return_model) result$model <- mod
  result
}

make_ar_generator <- function(q_train_adj, transforms, time_indices, horizon_steps, base_context) {
  function(var) {
    horizon_len <- length(horizon_steps)
    preds_adj <- predict_ar2(
      q_train_adj[[var]],
      horizon_len,
      var_label = var,
      context = base_context
    )
    if (!length(preds_adj) || length(preds_adj) != horizon_len) {
      return(rep(NA_real_, horizon_len))
    }
    restore_series_values(
      preds_adj,
      rep(var, horizon_len),
      time_indices,
      transforms
    )
  }
}

forecast_midas_series <- function(y_series, train_rows, x_train_full, x_future_full, horizon, include_trend) {
  y_train <- stats::window(y_series, end = stats::time(y_series)[train_rows])

  if (length(y_train) < 3) {
    warning(sprintf("MIDAS: Insufficient y_train data (n=%d)", length(y_train)), call. = FALSE)
    return(rep(NA_real_, horizon))
  }
  if (length(x_train_full) < 6) {
    warning(sprintf("MIDAS: Insufficient x_train data (n=%d)", length(x_train_full)), call. = FALSE)
    return(rep(NA_real_, horizon))
  }
  if (length(x_future_full) != horizon * months_per_quarter) {
    warning(sprintf("MIDAS: x_future_full length mismatch (expected %d, got %d)",
      horizon * months_per_quarter, length(x_future_full)
    ), call. = FALSE)
    return(rep(NA_real_, horizon))
  }

  trend_train <- seq_len(length(y_train))
  data_list <- list(y = y_train, x = x_train_full)
  formula_obj <- stats::as.formula("y ~ mls(y, k = 2, m = 1) + fmls(x, k = 2, m = 3)")

  if (isTRUE(include_trend)) {
    data_list$trend <- trend_train
    formula_obj <- stats::as.formula("y ~ trend + mls(y, k = 2, m = 1) + fmls(x, k = 2, m = 3)")
  }

  fit <- try(
    midasr::midas_r(formula_obj, data = data_list, start = list(x = rep(0, 3))),
    silent = TRUE
  )

  if (inherits(fit, "try-error")) {
    warning(sprintf("MIDAS fit error: %s", as.character(fit)), call. = FALSE)
    return(rep(NA_real_, horizon))
  }

  newdata <- list(x = x_future_full)
  if (isTRUE(include_trend)) {
    newdata$trend <- trend_train[length(trend_train)] + seq_len(horizon)
  }

  fc <- try(
    midasr::forecast(fit, newdata = newdata, h = horizon, method = "dynamic"),
    silent = TRUE
  )

  if (inherits(fc, "try-error")) {
    warning(sprintf("MIDAS forecast error: %s", as.character(fc)), call. = FALSE)
    return(rep(NA_real_, horizon))
  }

  result <- as.numeric(fc$mean)
  if (length(result) != horizon) {
    warning(sprintf("MIDAS forecast length mismatch (expected %d, got %d)",
      horizon, length(result)
    ), call. = FALSE)
    return(rep(NA_real_, horizon))
  }

  result
}

forecast_midas_latent <- function(y_series, train_rows, latent_states_df, variable_name, horizon, include_trend) {
  if (is.null(latent_states_df) || !nrow(latent_states_df)) {
    warning("MIDAS-Latent: No latent states provided", call. = FALSE)
    return(rep(NA_real_, horizon))
  }

  if (!variable_name %in% names(latent_states_df)) {
    warning(sprintf("MIDAS-Latent: Variable %s not found in latent states", variable_name), call. = FALSE)
    return(rep(NA_real_, horizon))
  }

  y_train <- stats::window(y_series, end = stats::time(y_series)[train_rows])
  if (length(y_train) < 3) {
    warning(sprintf("MIDAS-Latent: Insufficient y_train data (n=%d)", length(y_train)), call. = FALSE)
    return(rep(NA_real_, horizon))
  }

  latent_vec <- latent_states_df[[variable_name]]
  if (length(latent_vec) < train_rows * months_per_quarter) {
    warning(sprintf("MIDAS-Latent: Insufficient latent state data (n=%d, need %d)",
      length(latent_vec), train_rows * months_per_quarter
    ), call. = FALSE)
    return(rep(NA_real_, horizon))
  }

  x_train_full <- stats::ts(latent_vec[seq_len(train_rows * months_per_quarter)], frequency = 12)

  x_future_vec <- rep(latent_vec[train_rows * months_per_quarter], horizon * months_per_quarter)
  x_future_full <- stats::ts(x_future_vec, frequency = 12)

  if (length(x_train_full) < 6) {
    warning(sprintf("MIDAS-Latent: Insufficient x_train data (n=%d)", length(x_train_full)), call. = FALSE)
    return(rep(NA_real_, horizon))
  }

  trend_train <- seq_len(length(y_train))
  data_list <- list(y = y_train, x = x_train_full)
  formula_obj <- stats::as.formula("y ~ mls(y, k = 2, m = 1) + fmls(x, k = 2, m = 3)")

  if (isTRUE(include_trend)) {
    data_list$trend <- trend_train
    formula_obj <- stats::as.formula("y ~ trend + mls(y, k = 2, m = 1) + fmls(x, k = 2, m = 3)")
  }

  fit <- try(
    midasr::midas_r(formula_obj, data = data_list, start = list(x = rep(0, 3))),
    silent = TRUE
  )

  if (inherits(fit, "try-error")) {
    warning(sprintf("MIDAS-Latent fit error: %s", as.character(fit)), call. = FALSE)
    return(rep(NA_real_, horizon))
  }

  newdata <- list(x = x_future_full)
  if (isTRUE(include_trend)) {
    newdata$trend <- trend_train[length(trend_train)] + seq_len(horizon)
  }

  fc <- try(
    midasr::forecast(fit, newdata = newdata, h = horizon, method = "dynamic"),
    silent = TRUE
  )

  if (inherits(fc, "try-error")) {
    warning(sprintf("MIDAS-Latent forecast error: %s", as.character(fc)), call. = FALSE)
    return(rep(NA_real_, horizon))
  }

  result <- as.numeric(fc$mean)
  if (length(result) != horizon) {
    warning(sprintf("MIDAS-Latent forecast length mismatch (expected %d, got %d)",
      horizon, length(result)
    ), call. = FALSE)
    return(rep(NA_real_, horizon))
  }

  result
}

assemble_extra_month_predictions <- function(
    extra_months,
    q_train_adj,
    q_train_orig,
    monthly_series_list,
    transforms,
    n_lags,
    horizon_max,
    horizon_steps,
    target_vars,
    y_ts_list,
    x_train_full,
    x_future_actual_vec,
    future_start,
    horizon_months,
    idx,
    cutoff_label,
    forecast_quarters,
    quarter_end_dates,
    ar_base,
    train_rows) {
  warn_suffix <- sprintf("at fold %s (%s)", cutoff_label, coverage_label(extra_months))

  train_last_qtr_cv <- q_train_orig$qtr[nrow(q_train_orig)]
  baro_end <- add_months(quarter_to_month_end(train_last_qtr_cv), extra_months)
  monthly_train_trimmed <- lapply(monthly_series_list, function(ts_obj) {
    stats::window(ts_obj, end = baro_end)
  })

  mfvar_eval <- measure_elapsed({
    forecast_mfvar(
      q_train_adj,
      monthly_train_trimmed,
      transforms,
      n_lags,
      horizon_max,
      target_vars,
      seed = 1000L + idx * 10L + extra_months,
      return_model = FALSE,
      extract_states = TRUE
    )
  })
  mfvar_result <- mfvar_eval$result
  mfvar_pred <- mfvar_result$predictions |>
    tidyr::complete(
      variable = target_vars,
      step_ahead = horizon_steps,
      fill = list(prediction = NA_real_)
    ) |>
    dplyr::mutate(model = "MF-VAR")
  latent_states_fold <- mfvar_result$latent_states

  fill_value <- if (length(x_train_full)) utils::tail(x_train_full, 1) else 0
  x_future_vec <- rep(fill_value, horizon_months)
  observed_months <- min(extra_months, length(x_future_actual_vec))
  if (observed_months > 0) {
    x_future_vec[seq_len(observed_months)] <- x_future_actual_vec[seq_len(observed_months)]
  }
  x_future_ts <- stats::ts(x_future_vec, start = future_start, frequency = 12)

  midas_trend_eval <- run_forecaster_with_timing(
    target_vars = target_vars,
    horizon_steps = horizon_steps,
    model_label = "MIDAS (trend)",
    generator_fn = function(var) {
      forecast_midas_series(
        y_series = y_ts_list[[var]],
        train_rows = train_rows,
        x_train_full = x_train_full,
        x_future_full = x_future_ts,
        horizon = horizon_max,
        include_trend = TRUE
      )
    },
    warn_context = warn_suffix
  )

  midas_simple_eval <- run_forecaster_with_timing(
    target_vars = target_vars,
    horizon_steps = horizon_steps,
    model_label = "MIDAS",
    generator_fn = function(var) {
      forecast_midas_series(
        y_series = y_ts_list[[var]],
        train_rows = train_rows,
        x_train_full = x_train_full,
        x_future_full = x_future_ts,
        horizon = horizon_max,
        include_trend = FALSE
      )
    },
    warn_context = warn_suffix
  )

  midas_latent_trend_eval <- run_forecaster_with_timing(
    target_vars = target_vars,
    horizon_steps = horizon_steps,
    model_label = "MIDAS-Latent (trend)",
    generator_fn = function(var) {
      forecast_midas_latent(
        y_series = y_ts_list[[var]],
        train_rows = train_rows,
        latent_states_df = latent_states_fold,
        variable_name = var,
        horizon = horizon_max,
        include_trend = TRUE
      )
    },
    warn_context = warn_suffix
  )

  midas_latent_simple_eval <- run_forecaster_with_timing(
    target_vars = target_vars,
    horizon_steps = horizon_steps,
    model_label = "MIDAS-Latent",
    generator_fn = function(var) {
      forecast_midas_latent(
        y_series = y_ts_list[[var]],
        train_rows = train_rows,
        latent_states_df = latent_states_fold,
        variable_name = var,
        horizon = horizon_max,
        include_trend = FALSE
      )
    },
    warn_context = warn_suffix
  )

  combined_predictions <- dplyr::bind_rows(
    mfvar_pred,
    midas_trend_eval$predictions,
    midas_simple_eval$predictions,
    midas_latent_trend_eval$predictions,
    midas_latent_simple_eval$predictions,
    ar_base
  ) |>
    dplyr::mutate(
      extra_months = extra_months,
      fold_index = idx,
      cutoff_quarter = cutoff_label,
      forecast_quarter = as.character(forecast_quarters[step_ahead]),
      quarter_end = quarter_end_dates[step_ahead],
      horizon = label_horizon(step_ahead)
    )

  list(
    predictions = combined_predictions,
    timings = list(
      mfvar = mfvar_eval$elapsed,
      midas_kof = midas_trend_eval$elapsed + midas_simple_eval$elapsed,
      midas_latent = midas_latent_trend_eval$elapsed + midas_latent_simple_eval$elapsed
    )
  )
}

summarise_metrics <- function(tbl) {
  tbl |>
    dplyr::summarise(
      rmse = {
        valid <- stats::na.omit(error)
        if (length(valid)) sqrt(mean(valid^2)) else NA_real_
      },
      mae = {
        valid <- stats::na.omit(error)
        if (length(valid)) mean(abs(valid)) else NA_real_
      },
      observations = sum(!is.na(error)),
      .groups = "drop"
    )
}

table_to_markdown <- function(df, headers) {
  if (!is.data.frame(df) || !nrow(df) || !ncol(df)) return(character())
  result_df <- df
  for (col in names(result_df)) {
    result_df[[col]] <- as.character(result_df[[col]])
  }
  if (length(headers)) {
    colnames(result_df) <- headers
  } else {
    headers <- colnames(result_df)
  }
  header_line <- paste0("| ", paste(headers, collapse = " | "), " |")
  separator_line <- paste0("| ", paste(rep("---", length(headers)), collapse = " | "), " |")
  body_lines <- vapply(seq_len(nrow(result_df)), function(i) {
    row_values <- unlist(result_df[i, , drop = FALSE], use.names = FALSE)
    paste0("| ", paste(row_values, collapse = " | "), " |")
  }, character(1))
  c(header_line, separator_line, body_lines, "")
}

measure_elapsed <- function(expr) {
  expr_call <- substitute(expr)
  parent_env <- parent.frame()
  start <- proc.time()
  result <- eval(expr_call, envir = parent_env)
  elapsed <- (proc.time() - start)[["elapsed"]]
  list(result = result, elapsed = elapsed)
}
