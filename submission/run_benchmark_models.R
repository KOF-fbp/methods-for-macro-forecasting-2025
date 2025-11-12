#!/usr/bin/env Rscript

if (!interactive()) {
  args_all <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args_all, value = TRUE)
  if (length(file_arg)) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE)
    setwd(dirname(script_path))
  }
}

source(file.path("R", "setup.R"))
source(file.path("R", "data_processing.R"))
source(file.path("R", "evaluation.R"))

stage_status <- local({
  stage_env <- new.env(parent = emptyenv())
  stage_env$idx <- 0L
  stage_env$current <- NULL
  function(name = NULL, status = c("start", "done", "skip", "error")) {
    status <- match.arg(status)
    if (identical(status, "start")) {
      if (is.null(name)) stop("Stage name required when status = 'start'")
      stage_env$idx <- stage_env$idx + 1L
      stage_env$current <- name
      message(sprintf("\n[Stage %d] %s ...", stage_env$idx, name))
    } else {
      stage_name <- if (is.null(name)) stage_env$current else name
      if (is.null(stage_name)) stage_name <- "<unnamed stage>"
      label <- switch(status,
        done = "completed",
        skip = "skipped",
        error = "failed",
        status
      )
      message(sprintf("[Stage %d] %s %s.", stage_env$idx, stage_name, label))
      if (status %in% c("done", "skip", "error")) {
        stage_env$current <- NULL
      }
    }
  }
})

stage_status("Project setup", "start")
activate_project()

all_pkgs <- unique(c(required_pkgs, "midasr", "forecast", "purrr", "future", "future.apply"))
load_required_packages(all_pkgs)
stage_status(status = "done")

DATA_DIR <- file.path(".", "data")
OUT_DIR <- file.path(".", "output")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

forecast_steps <- c(1L, 4L)
history_quarters <- 4L
n_lags <- 5

quarter_start_month <- function(q) {
  q_date <- zoo::as.Date(q, frac = 0)
  c(lubridate::year(q_date), lubridate::month(q_date))
}

label_horizon <- function(step) {
  dplyr::case_when(
    step == 1L ~ "1-step ahead",
    step == 4L ~ "1-year ahead",
    TRUE ~ "Other"
  )
}

convert_for_plot <- function(value, var) {
  if (identical(var, "exch_rate")) exp(value) else value
}

label_for_var <- function(var) {
  switch(
    var,
    "gdp_growth" = "Annualised percentage",
    "inflation" = "Annualised percentage",
    "exch_rate" = "CHF per EUR",
    "Value"
  )
}

months_per_quarter <- 3L

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

coverage_label <- function(extra_months) {
  dplyr::case_when(
    extra_months == 0L ~ "Cutoff only",
    extra_months == 1L ~ "Cutoff +1m",
    extra_months == 2L ~ "Cutoff +2m",
    extra_months == 3L ~ "Cutoff +3m",
    TRUE ~ sprintf("Cutoff +%dm", extra_months)
  )
}

forecast_mfvar <- function(q_train_adj, baro_train, transforms, n_lags, horizon_quarters, target_vars, seed = 123L, return_model = FALSE) {
  Y_train <- build_Y(q_train_adj, baro_train)
  mod <- tryCatch(
    estimate_mfvar_model(Y_train, n_lags, n_fcst = horizon_quarters * 3L, seed = seed),
    error = function(err) {
      warning(sprintf("MF-VAR estimation failed: %s", conditionMessage(err)), call. = FALSE)
      NULL
    }
  )

  if (is.null(mod)) {
    fallback <- purrr::map_dfr(target_vars, function(var) {
      tibble::tibble(variable = var, step_ahead = seq_len(horizon_quarters), prediction = NA_real_)
    })
    result <- list(predictions = fallback)
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

  result <- list(predictions = fc)
  if (return_model) result$model <- mod
  result
}

forecast_midas_series <- function(y_series, train_rows, x_train, x_future, horizon, include_trend) {
  y_train <- stats::window(y_series, end = stats::time(y_series)[train_rows])
  trend_train <- seq_len(length(y_train))

  data_list <- list(y = y_train, x = x_train)
  formula_obj <- stats::as.formula("y ~ mls(y, k = 1, m = 1) + fmls(x, k = 2, m = 3)")

  if (isTRUE(include_trend)) {
    data_list$trend <- trend_train
    formula_obj <- stats::as.formula("y ~ trend + mls(y, k = 1, m = 1) + fmls(x, k = 2, m = 3)")
  }

  fit <- try(
    midasr::midas_r(formula_obj, data = data_list, start = list(x = rep(0, 3))),
    silent = TRUE
  )

  if (inherits(fit, "try-error")) {
    warning(as.character(fit), call. = FALSE)
    return(rep(NA_real_, horizon))
  }

  newdata <- list(x = x_future)
  if (isTRUE(include_trend)) {
    newdata$trend <- trend_train[length(trend_train)] + seq_len(horizon)
  }

  fc <- try(
    midasr::forecast(fit, newdata = newdata, h = horizon, method = "dynamic"),
    silent = TRUE
  )

  if (inherits(fc, "try-error")) {
    warning(as.character(fc), call. = FALSE)
    return(rep(NA_real_, horizon))
  }

  as.numeric(fc$mean)
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
  if (!nrow(df)) return(character())
  header_line <- paste0("| ", paste(headers, collapse = " | "), " |")
  separator_line <- paste0("|", paste(rep("---", length(headers)), collapse = "|"), "|")
  body_lines <- apply(df, 1, function(row) paste0("| ", paste(row, collapse = " | "), " |"))
  c(header_line, separator_line, body_lines)
}

run_benchmark_cross_validation <- function(
    qdat_adj,
    qdat_orig,
    transforms,
    baro_ts,
    baro_diff_series,
    y_ts_list,
    target_vars,
    forecast_steps,
    n_lags,
    extra_months_options = 0:2,
    max_folds = Inf,
    initial_train_quarter = zoo::as.yearqtr("2015 Q4"),
    progress = TRUE) {

  horizon_max <- max(forecast_steps)
  horizon_steps <- seq_len(horizon_max)
  horizon_months <- horizon_max * months_per_quarter
  n_obs <- nrow(qdat_adj)

  if (!inherits(initial_train_quarter, "yearqtr")) {
    initial_train_quarter <- zoo::as.yearqtr(initial_train_quarter)
  }

  initial_index <- match(initial_train_quarter, qdat_orig$qtr)
  if (is.na(initial_index)) {
    stop("Initial training quarter ", initial_train_quarter, " not found in the quarterly data.")
  }

  cv_start_idx <- initial_index + 1L
  cv_end_idx <- n_obs - horizon_max + 1L
  if (cv_end_idx < cv_start_idx) {
    warning("Not enough observations to run cross-validation with the requested horizon.")
    return(list(
      results = tibble::tibble(),
      metrics_by_horizon = tibble::tibble(),
      metrics_overall = tibble::tibble(),
      folds = tibble::tibble(),
      fold_count = 0L,
      extra_values = extra_months_options
    ))
  }

  cv_indices <- seq.int(cv_start_idx, cv_end_idx)
  if (is.finite(max_folds)) {
    max_folds <- as.integer(max_folds)
    if (max_folds > 0L && length(cv_indices) > max_folds) {
      cv_indices <- tail(cv_indices, max_folds)
    }
  }
  total_folds <- length(cv_indices)
  if (total_folds == 0L) {
    warning("Cross-validation skipped: no folds selected after applying limits.")
    return(list(
      results = tibble::tibble(),
      metrics_by_horizon = tibble::tibble(),
      metrics_overall = tibble::tibble(),
      folds = tibble::tibble(),
      fold_count = 0L,
      extra_values = extra_months_options
    ))
  }

  show_fold_progress <- isTRUE(progress)
  cores_available <- parallel::detectCores(logical = TRUE)
  default_workers <- if (is.null(cores_available) || !is.finite(cores_available)) 1L else max(1L, cores_available - 1L)
  desired_workers <- getOption("mfvar.cv_workers", default_workers)
  if (!is.numeric(desired_workers) || !is.finite(desired_workers)) desired_workers <- default_workers
  desired_workers <- as.integer(desired_workers)
  desired_workers <- max(1L, min(desired_workers, total_folds))
  use_parallel <- total_folds > 1L && desired_workers > 1L

  if (use_parallel && show_fold_progress) {
    message(sprintf("  • Running %d CV folds using %d worker(s)...", total_folds, desired_workers))
  }

  run_single_fold <- function(fold_pos) {
    idx <- cv_indices[fold_pos]
    train_rows <- idx - 1L
    if (train_rows <= n_lags) {
      return(NULL)
    }

    q_train_adj <- dplyr::slice_head(qdat_adj, n = train_rows)
    q_train_orig <- dplyr::slice_head(qdat_orig, n = train_rows)
    q_eval_orig <- dplyr::slice(qdat_orig, idx:(idx + horizon_max - 1L))

    cutoff_quarter <- q_train_orig$qtr[nrow(q_train_orig)]
    cutoff_label <- as.character(cutoff_quarter)
    forecast_quarters <- q_eval_orig$qtr
    quarter_end_dates <- zoo::as.Date(forecast_quarters, frac = 1)

    if (show_fold_progress && !use_parallel) {
      message(sprintf("  • CV fold %02d/%02d | training through %s", fold_pos, total_folds, cutoff_label))
    }

    actual_fold <- q_eval_orig |>
      dplyr::mutate(step_ahead = dplyr::row_number()) |>
      dplyr::select(step_ahead, tidyselect::all_of(target_vars)) |>
      tidyr::pivot_longer(
        cols = -step_ahead,
        names_to = "variable",
        values_to = "actual"
      ) |>
      dplyr::mutate(
        quarter_end = quarter_end_dates[step_ahead],
        horizon = label_horizon(step_ahead),
        fold_index = idx,
        cutoff_quarter = cutoff_label,
        forecast_quarter = as.character(forecast_quarters[step_ahead])
      )

    time_indices <- compute_time_index(train_rows, horizon_steps)

    ar_base <- purrr::map_dfr(target_vars, function(var) {
      preds_adj <- predict_ar2(q_train_adj[[var]], horizon_max, var_label = var, context = sprintf("CV fold ending %s", cutoff_label))
      preds_orig <- restore_series_values(
        preds_adj,
        rep(var, horizon_max),
        time_indices,
        transforms
      )
      tibble::tibble(
        variable = var,
        step_ahead = horizon_steps,
        prediction = preds_orig,
        model = "AR(2)"
      )
    })

    rw_base <- purrr::map_dfr(target_vars, function(var) {
      preds <- predict_rw_trend(q_train_orig[[var]], horizon_max, var_label = var, context = sprintf("CV fold ending %s", cutoff_label))
      tibble::tibble(
        variable = var,
        step_ahead = horizon_steps,
        prediction = preds,
        model = "RW-trend"
      )
    })

    train_last_month <- quarter_to_month_end(cutoff_quarter)
    x_train_full <- stats::window(baro_diff_series, end = train_last_month)

    if (length(x_train_full) != train_rows * months_per_quarter) {
      warning(sprintf("Skipping fold %s: monthly regressor length mismatch.", cutoff_label), call. = FALSE)
      return(NULL)
    }

    future_start <- add_months(train_last_month, 1L)
    future_end <- add_months(train_last_month, horizon_months)
    x_future_actual <- try(stats::window(baro_diff_series, start = future_start, end = future_end), silent = TRUE)
    x_future_actual_vec <- if (inherits(x_future_actual, "try-error") || length(x_future_actual) == 0L) {
      numeric(0)
    } else {
      as.numeric(x_future_actual)
    }

    fold_predictions <- vector("list", length(extra_months_options))

    for (extra_idx in seq_along(extra_months_options)) {
      extra_months <- extra_months_options[extra_idx]
      baro_end <- add_months(train_last_month, extra_months)
      baro_train <- stats::window(baro_ts, end = baro_end)

      mfvar_pred <- forecast_mfvar(
        q_train_adj,
        baro_train,
        transforms,
        n_lags,
        horizon_max,
        target_vars,
        seed = 1000L + idx * 10L + extra_months,
        return_model = FALSE
      )$predictions |>
        tidyr::complete(
          variable = target_vars,
          step_ahead = horizon_steps,
          fill = list(prediction = NA_real_)
        ) |>
        dplyr::mutate(model = "MF-VAR")

      fill_value <- if (length(x_train_full)) utils::tail(x_train_full, 1) else 0
      x_future_vec <- rep(fill_value, horizon_months)
      observed_months <- min(extra_months, length(x_future_actual_vec))
      if (observed_months > 0) {
        x_future_vec[seq_len(observed_months)] <- x_future_actual_vec[seq_len(observed_months)]
      }
      x_future_ts <- stats::ts(x_future_vec, start = future_start, frequency = 12)

      midas_trend_pred <- purrr::map_dfr(target_vars, function(var) {
        preds <- forecast_midas_series(
          y_series = y_ts_list[[var]],
          train_rows = train_rows,
          x_train = x_train_full,
          x_future = x_future_ts,
          horizon = horizon_max,
          include_trend = TRUE
        )
        tibble::tibble(variable = var, step_ahead = horizon_steps, prediction = preds, model = "MIDAS (trend)")
      })

      midas_simple_pred <- purrr::map_dfr(target_vars, function(var) {
        preds <- forecast_midas_series(
          y_series = y_ts_list[[var]],
          train_rows = train_rows,
          x_train = x_train_full,
          x_future = x_future_ts,
          horizon = horizon_max,
          include_trend = FALSE
        )
        tibble::tibble(variable = var, step_ahead = horizon_steps, prediction = preds, model = "MIDAS")
      })

      fold_predictions[[extra_idx]] <- dplyr::bind_rows(
        mfvar_pred,
        midas_trend_pred,
        midas_simple_pred,
        ar_base,
        rw_base
      ) |>
        dplyr::mutate(
          extra_months = extra_months,
          fold_index = idx,
          cutoff_quarter = cutoff_label,
          forecast_quarter = as.character(forecast_quarters[step_ahead]),
          quarter_end = quarter_end_dates[step_ahead],
          horizon = label_horizon(step_ahead)
        )
    }

    combined_predictions <- dplyr::bind_rows(fold_predictions)
    if (!nrow(combined_predictions)) {
      return(NULL)
    }

    list(
      predictions = combined_predictions,
      actual = actual_fold
    )
  }

  if (use_parallel) {
    oplan <- future::plan()
    on.exit(future::plan(oplan), add = TRUE)
    future::plan(future::multisession, workers = desired_workers)
    fold_results <- future.apply::future_lapply(seq_along(cv_indices), run_single_fold, future.seed = TRUE)
  } else {
    fold_results <- lapply(seq_along(cv_indices), run_single_fold)
  }

  fold_results <- purrr::compact(fold_results)

  if (length(fold_results)) {
    predictions_tbl <- dplyr::bind_rows(lapply(fold_results, `[[`, "predictions"))
    actual_tbl <- dplyr::bind_rows(lapply(fold_results, `[[`, "actual"))
  } else {
    predictions_tbl <- tibble::tibble()
    actual_tbl <- tibble::tibble()
  }

  if (!nrow(predictions_tbl) || !nrow(actual_tbl)) {
    warning("Cross-validation produced no predictions after filtering.")
    return(list(
      results = tibble::tibble(),
      metrics_by_horizon = tibble::tibble(),
      metrics_overall = tibble::tibble(),
      folds = tibble::tibble(),
      fold_count = 0L,
      extra_values = extra_months_options
    ))
  }

  cv_results <- predictions_tbl |>
    dplyr::left_join(
      actual_tbl,
      by = c(
        "variable", "step_ahead", "quarter_end", "horizon",
        "fold_index", "cutoff_quarter", "forecast_quarter"
      )
    ) |>
    dplyr::mutate(error = prediction - actual)

  metrics_by_horizon <- cv_results |>
    dplyr::filter(step_ahead %in% forecast_steps) |>
    dplyr::group_by(extra_months, model, horizon) |>
    summarise_metrics() |>
    dplyr::arrange(extra_months, model, horizon)

  metrics_overall <- cv_results |>
    dplyr::filter(step_ahead %in% forecast_steps) |>
    dplyr::group_by(extra_months, model) |>
    summarise_metrics() |>
    dplyr::arrange(extra_months, model)

  folds_info <- cv_results |>
    dplyr::distinct(fold_index, cutoff_quarter, forecast_quarter) |>
    dplyr::arrange(fold_index, forecast_quarter)

  list(
    results = cv_results,
    metrics_by_horizon = metrics_by_horizon,
    metrics_overall = metrics_overall,
    folds = folds_info,
    fold_count = dplyr::n_distinct(cv_results$fold_index),
    extra_values = sort(unique(cv_results$extra_months))
  )
}

qdat_raw <- read_quarterly_data(DATA_DIR)
baro_raw <- fetch_kof_barometer()
trimmed <- trim_to_overlap(qdat_raw, baro_raw)
stationary <- stationarise_quarterly(trimmed$qdat)
qdat_orig <- trimmed$qdat
qdat_adj <- stationary$data
transforms <- stationary$transforms
baro_ts <- window_baro(trimmed$baro_ts, qdat_orig)

baro_diff_series <- local({
  first_quarter_date <- zoo::as.Date(qdat_orig$qtr[1], frac = 0)
  prev_year <- lubridate::year(first_quarter_date)
  prev_month <- (lubridate::quarter(first_quarter_date) - 1L) * months_per_quarter
  if (prev_month == 0L) {
    prev_month <- 12L
    prev_year <- prev_year - 1L
  }

  diff_source <- stats::window(baro_ts, start = c(prev_year, prev_month))
  diff_series <- base::diff(diff_source)
  first_month_of_quarter <- (lubridate::quarter(first_quarter_date) - 1L) * months_per_quarter + 1L
  stats::window(
    diff_series,
    start = c(lubridate::year(first_quarter_date), first_month_of_quarter)
  )
})

stage_status("Data preparation", "start")

target_vars <- target_variables
y_ts_list <- build_q_ts(qdat_orig)

max_holdout <- nrow(qdat_adj) - (n_lags + 1)
if (max_holdout < 0) max_holdout <- 0
eval_horizon <- min(max(forecast_steps), max_holdout)

if (eval_horizon < max(forecast_steps)) {
  stop("Need at least ", max(forecast_steps), " holdout quarters to evaluate 1-year-ahead errors. Reduce lag length or extend the sample.")
}

train_rows <- nrow(qdat_adj) - eval_horizon
q_train_adj <- qdat_adj |> dplyr::slice_head(n = train_rows)
q_train_orig <- qdat_orig |> dplyr::slice_head(n = train_rows)
q_test_orig <- qdat_orig |> dplyr::slice_tail(n = eval_horizon)
forecast_quarters <- q_test_orig$qtr
forecast_dates <- zoo::as.Date(forecast_quarters, frac = 1)

stage_status(status = "done")

stage_status("Holdout evaluation", "start")

train_last_qtr <- q_train_orig$qtr[nrow(q_train_orig)]
train_last_month <- quarter_to_month_end(train_last_qtr)
x_train_full <- stats::window(baro_diff_series, end = train_last_month)

if (length(x_train_full) != train_rows * months_per_quarter) {
  stop("Monthly regressor length does not match the training sample.")
}

test_start_month <- quarter_start_month(forecast_quarters[1])
test_end_month <- quarter_to_month_end(forecast_quarters[eval_horizon])
x_future_full <- stats::window(baro_diff_series, start = test_start_month, end = test_end_month)

if (length(x_future_full) != eval_horizon * months_per_quarter) {
  stop("Monthly regressor length does not cover the holdout horizon.")
}

baro_train_end <- quarter_to_month_end(q_train_orig$qtr[nrow(q_train_orig)])
baro_train <- stats::window(baro_ts, end = baro_train_end)
mfvar_holdout <- forecast_mfvar(
  q_train_adj,
  baro_train,
  transforms,
  n_lags,
  eval_horizon,
  target_vars,
  seed = 123L,
  return_model = FALSE
)$predictions |>
  dplyr::mutate(model = "MF-VAR")

ar_holdout <- purrr::map_dfr(target_vars, function(var) {
  preds_adj <- predict_ar2(q_train_adj[[var]], eval_horizon, var_label = var, context = "holdout")
  tibble::tibble(
    variable = var,
    step_ahead = seq_len(eval_horizon),
    prediction = restore_series_values(
      preds_adj,
      rep(var, eval_horizon),
      compute_time_index(train_rows, seq_len(eval_horizon)),
      transforms
    ),
    model = "AR(2)"
  )
})

rw_holdout <- purrr::map_dfr(target_vars, function(var) {
  tibble::tibble(
    variable = var,
    step_ahead = seq_len(eval_horizon),
    prediction = predict_rw_trend(q_train_orig[[var]], eval_horizon, var_label = var, context = "holdout"),
    model = "RW-trend"
  )
})

midas_trend_holdout <- purrr::map_dfr(target_vars, function(var) {
  preds <- forecast_midas_series(
    y_ts_list[[var]],
    train_rows,
    x_train_full,
    x_future_full,
    eval_horizon,
    include_trend = TRUE
  )
  tibble::tibble(variable = var, step_ahead = seq_len(eval_horizon), prediction = preds, model = "MIDAS (trend)")
})

midas_simple_holdout <- purrr::map_dfr(target_vars, function(var) {
  preds <- forecast_midas_series(
    y_ts_list[[var]],
    train_rows,
    x_train_full,
    x_future_full,
    eval_horizon,
    include_trend = FALSE
  )
  tibble::tibble(variable = var, step_ahead = seq_len(eval_horizon), prediction = preds, model = "MIDAS")
})

# --- Gather predictions ----------------------------------------------------
predictions_tbl <- dplyr::bind_rows(
  mfvar_holdout,
  ar_holdout,
  rw_holdout,
  midas_trend_holdout,
  midas_simple_holdout
) |>
  dplyr::mutate(
    quarter_end = forecast_dates[step_ahead],
    horizon = label_horizon(step_ahead)
  )

actual_tbl <- q_test_orig |>
  dplyr::mutate(step_ahead = dplyr::row_number()) |>
  dplyr::select(step_ahead, tidyselect::all_of(target_vars)) |>
  tidyr::pivot_longer(cols = -step_ahead, names_to = "variable", values_to = "actual") |>
  dplyr::mutate(
    quarter_end = forecast_dates[step_ahead],
    horizon = label_horizon(step_ahead)
  )

# --- Metrics for specified horizons ----------------------------------------
metric_inputs <- predictions_tbl |>
  dplyr::inner_join(actual_tbl, by = c("variable", "step_ahead", "quarter_end", "horizon")) |>
  dplyr::mutate(error = prediction - actual)

holdout_metrics_detailed <- metric_inputs |>
  dplyr::group_by(variable, model, step_ahead, horizon) |>
  summarise_metrics()

metrics_tbl <- metric_inputs |>
  dplyr::filter(step_ahead %in% forecast_steps) |>
  dplyr::group_by(model, horizon) |>
  summarise_metrics()

# --- Forecast table (wide) -------------------------------------------------
forecast_wide <- predictions_tbl |>
  tidyr::pivot_wider(
    id_cols = c(variable, step_ahead, horizon, quarter_end),
    names_from = model,
    values_from = prediction
  ) |>
  dplyr::left_join(actual_tbl |> dplyr::select(variable, step_ahead, horizon, quarter_end, actual),
                   by = c("variable", "step_ahead", "horizon", "quarter_end")) |>
  dplyr::arrange(variable, step_ahead)

stage_status(status = "done")

stage_status("Cross-validation evaluation", "start")

cv_initial_quarter <- zoo::as.yearqtr("2015 Q4")
cv_extra_months <- 0:2
cv_max_folds <- 28L
cv_output <- run_benchmark_cross_validation(
  qdat_adj = qdat_adj,
  qdat_orig = qdat_orig,
  transforms = transforms,
  baro_ts = baro_ts,
  baro_diff_series = baro_diff_series,
  y_ts_list = y_ts_list,
  target_vars = target_vars,
  forecast_steps = forecast_steps,
  n_lags = n_lags,
  extra_months_options = cv_extra_months,
  max_folds = cv_max_folds,
  initial_train_quarter = cv_initial_quarter,
  progress = TRUE
)

cv_results <- cv_output$results
cv_metrics_tbl <- cv_output$metrics_by_horizon
cv_metrics_overall <- cv_output$metrics_overall
cv_folds_tbl <- cv_output$folds

if (!nrow(cv_results)) {
  stage_status(status = "skip")
} else {
  stage_status(status = "done")
  message(sprintf(
    "Cross-validation processed %d folds across %d coverage settings.",
    cv_output$fold_count,
    length(cv_output$extra_values)
  ))
}

# --- Output summaries and plots --------------------------------------------
stage_status("Output generation", "start")
summary_horizon_tbl <- metrics_tbl |>
  dplyr::mutate(
    RMSE = sprintf("%.4f", rmse),
    MAE = sprintf("%.4f", mae),
    Observations = as.character(observations)
  ) |>
  dplyr::select(model, horizon, Observations, RMSE, MAE)

summary_overall_tbl <- metric_inputs |>
  dplyr::group_by(model) |>
  summarise_metrics() |>
  dplyr::mutate(
    RMSE = sprintf("%.4f", rmse),
    MAE = sprintf("%.4f", mae),
    Observations = as.character(observations)
  ) |>
  dplyr::select(model, Observations, RMSE, MAE)

summary_cv_tbl <- cv_metrics_tbl |>
  dplyr::arrange(extra_months, model, horizon) |>
  dplyr::mutate(
    `Monthly data` = coverage_label(extra_months),
    RMSE = sprintf("%.4f", rmse),
    MAE = sprintf("%.4f", mae),
    Observations = as.character(observations)
  ) |>
  dplyr::select(`Monthly data`, model, horizon, Observations, RMSE, MAE)

summary_path <- file.path(OUT_DIR, "model_benchmark_summary.md")
summary_lines <- c(
  "# Benchmark Error Summary",
  "",
  "## Holdout RMSE and MAE by Horizon (aggregated across variables)",
  table_to_markdown(summary_horizon_tbl, c("Model", "Horizon", "Observations", "RMSE", "MAE")),
  "",
  "## Holdout Overall Average Errors",
  table_to_markdown(summary_overall_tbl, c("Model", "Observations", "RMSE", "MAE")),
  "",
  "## Rolling Cross-Validation RMSE and MAE by Monthly Coverage",
  table_to_markdown(summary_cv_tbl, c("Monthly data", "Model", "Horizon", "Observations", "RMSE", "MAE"))
)
readr::write_lines(summary_lines, summary_path)

plot_models <- c("Actual", "MF-VAR", "MIDAS (trend)", "MIDAS", "AR(2)", "RW-trend")
colour_map <- c(
  "Actual" = "#000000",
  "MF-VAR" = "#1b9e77",
  "MIDAS (trend)" = "#7570b3",
  "MIDAS" = "#d95f02",
  "AR(2)" = "#e7298a",
  "RW-trend" = "#66a61e"
)
linetype_map <- c(
  "Actual" = "solid",
  "MF-VAR" = "solid",
  "MIDAS (trend)" = "dashed",
  "MIDAS" = "dashed",
  "AR(2)" = "dotted",
  "RW-trend" = "dotdash"
)

plot_paths <- purrr::map_chr(target_vars, function(var) {
  history_values <- tail(q_train_orig[[var]], history_quarters)
  history_dates <- zoo::as.Date(tail(q_train_orig$qtr, history_quarters), frac = 1)
  history_df <- tibble::tibble(
    quarter_end = history_dates,
    model = factor("Actual", levels = plot_models),
    display_value = convert_for_plot(history_values, var)
  )

  future_actual_df <- actual_tbl |>
    dplyr::filter(variable == var, step_ahead <= max(forecast_steps)) |>
    dplyr::mutate(
      model = factor("Actual", levels = plot_models),
      display_value = convert_for_plot(actual, var)
    ) |>
    dplyr::select(quarter_end, model, display_value)

  actual_series <- dplyr::bind_rows(history_df, future_actual_df) |>
    dplyr::arrange(quarter_end)

  forecast_series <- predictions_tbl |>
    dplyr::filter(variable == var, step_ahead <= max(forecast_steps)) |>
    dplyr::mutate(
      model = factor(model, levels = plot_models),
      display_value = convert_for_plot(prediction, var)
    ) |>
    dplyr::select(quarter_end, model, display_value)

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = actual_series,
      ggplot2::aes(x = quarter_end, y = display_value, colour = model, linetype = model),
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = actual_series,
      ggplot2::aes(x = quarter_end, y = display_value, colour = model),
      size = 2
    ) +
    ggplot2::geom_line(
      data = forecast_series,
      ggplot2::aes(x = quarter_end, y = display_value, colour = model, linetype = model, group = model),
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = forecast_series,
      ggplot2::aes(x = quarter_end, y = display_value, colour = model),
      size = 2
    ) +
    ggplot2::scale_colour_manual(values = colour_map, drop = FALSE) +
    ggplot2::scale_linetype_manual(values = linetype_map, drop = FALSE) +
    ggplot2::labs(
      title = paste("Forecast comparison:", var),
      subtitle = "History (last 4 quarters) with 1-step to 1-year-ahead forecasts",
      x = "Quarter end",
      y = label_for_var(var),
      colour = NULL,
      linetype = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  plot_path <- file.path(OUT_DIR, paste0("model_benchmark_plot_", var, ".png"))
  ggplot2::ggsave(plot_path, plot = p, width = 8, height = 5, dpi = 150)
  plot_path
})

cv_metrics_export <- cv_metrics_tbl |>
  dplyr::mutate(monthly_coverage = coverage_label(extra_months)) |>
  dplyr::relocate(monthly_coverage, .before = model)

cv_results_export <- cv_results |>
  dplyr::mutate(monthly_coverage = coverage_label(extra_months)) |>
  dplyr::rename(fold = fold_index) |>
  dplyr::relocate(monthly_coverage, extra_months, fold, cutoff_quarter, forecast_quarter, .before = variable) |>
  dplyr::arrange(extra_months, fold, variable, step_ahead)

readr::write_csv(metrics_tbl, file.path(OUT_DIR, "model_benchmark_metrics.csv"))
readr::write_csv(holdout_metrics_detailed, file.path(OUT_DIR, "model_benchmark_holdout_detailed.csv"))
readr::write_csv(forecast_wide, file.path(OUT_DIR, "model_benchmark_forecasts.csv"))
readr::write_csv(cv_metrics_export, file.path(OUT_DIR, "model_benchmark_cv_metrics.csv"))
readr::write_csv(cv_results_export, file.path(OUT_DIR, "model_benchmark_cv_predictions.csv"))

cat(
  "Benchmark comparison complete.\n",
  "  - output/model_benchmark_metrics.csv\n",
  "  - output/model_benchmark_holdout_detailed.csv\n",
  "  - output/model_benchmark_forecasts.csv\n",
  "  - output/model_benchmark_cv_metrics.csv\n",
  "  - output/model_benchmark_cv_predictions.csv\n",
  "  - output/model_benchmark_summary.md\n",
  paste0("  - ", plot_paths, collapse = "\n"),
  "\n",
  sep = ""
)

stage_status(status = "done")
message("\nBenchmark pipeline complete.")
