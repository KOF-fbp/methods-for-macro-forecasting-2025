# Forecast evaluation utilities

utils::globalVariables(c(
  "variable", "step_ahead", "mfvar", "actual", "ar2", "fold_index",
  "model", "rw_trend", "time_index"
))

variable <- step_ahead <- mfvar <- actual <- ar2 <- fold_index <- rw_trend <- time_index <- NULL

run_holdout_evaluation <- function(qdat_adj, qdat_orig, monthly_inputs, n_lags, target_vars, transforms, out_dir) {
  eval_table <- NULL
  evaluation_path <- NULL
  eval_horizon <- {
    max_holdout <- nrow(qdat_adj) - (n_lags + 1)
    if (max_holdout < 0) max_holdout <- 0
    min(4, max_holdout)
  }

  if (eval_horizon >= 1) {
    q_train_adj <- qdat_adj |> dplyr::slice_head(n = nrow(qdat_adj) - eval_horizon)
    q_train_orig <- qdat_orig |> dplyr::slice_head(n = nrow(qdat_orig) - eval_horizon)
    q_eval_orig <- qdat_orig |> dplyr::slice_tail(n = eval_horizon)

    baro_train_end <- quarter_to_month_end(q_train_orig$qtr[nrow(q_train_orig)])
    monthly_train <- if (is.list(monthly_inputs)) {
      window_monthly_series(monthly_inputs, q_train_orig)
    } else {
      stats::window(monthly_inputs, end = baro_train_end)
    }
    Y_train <- build_Y(q_train_adj, monthly_train)

    mod_eval <- estimate_mfvar_model(Y_train, n_lags, n_fcst = eval_horizon, seed = 123)
    fc_eval_raw <- predict(mod_eval, aggregate_fcst = TRUE, pred_bands = 0.8)

    fc_eval <- fc_eval_raw |>
      dplyr::filter(variable %in% target_vars) |>
      dplyr::arrange(variable, time) |>
      dplyr::group_by(variable) |>
      dplyr::mutate(step_ahead = dplyr::row_number()) |>
      dplyr::filter(step_ahead <= eval_horizon) |>
      dplyr::ungroup() |>
      dplyr::select(variable, step_ahead, mfvar = median)

    index_map <- tibble::tibble(
      step_ahead = seq_len(eval_horizon),
      time_index = compute_time_index(nrow(q_train_adj), seq_len(eval_horizon))
    )

    mfvar_eval <- fc_eval |>
      dplyr::left_join(index_map, by = "step_ahead") |>
      dplyr::mutate(
        mfvar = restore_series_values(mfvar, variable, time_index, transforms)
      ) |>
      dplyr::select(variable, step_ahead, mfvar)

    actual_eval <- q_eval_orig |>
      dplyr::mutate(step_ahead = dplyr::row_number()) |>
      dplyr::select(step_ahead, tidyselect::all_of(target_vars)) |>
      tidyr::pivot_longer(
        cols = -tidyselect::any_of("step_ahead"),
        names_to = "variable",
        values_to = "actual"
      )

    mfvar_metrics <- mfvar_eval |>
      dplyr::left_join(actual_eval, by = c("variable", "step_ahead")) |>
      dplyr::group_by(variable) |>
      dplyr::summarise(
        model = "MF-VAR",
        rmse = safe_rmse(mfvar, actual),
        mae = safe_mae(mfvar, actual),
        .groups = "drop"
      )

    ar_preds_list <- lapply(target_vars, function(var) {
      series_adj <- q_train_adj[[var]]
      preds_adj <- predict_ar2(series_adj, eval_horizon, var_label = var, context = "holdout")
      tibble::tibble(variable = var, step_ahead = seq_len(eval_horizon), ar2 = preds_adj)
    })

    ar_eval <- dplyr::bind_rows(ar_preds_list) |>
      dplyr::left_join(index_map, by = "step_ahead") |>
      dplyr::mutate(
        ar2 = restore_series_values(ar2, variable, time_index, transforms)
      ) |>
      dplyr::select(variable, step_ahead, ar2) |>
      dplyr::left_join(actual_eval, by = c("variable", "step_ahead"))

    ar_metrics <- ar_eval |>
      dplyr::group_by(variable) |>
      dplyr::summarise(
        model = "AR(2)",
        rmse = safe_rmse(ar2, actual),
        mae = safe_mae(ar2, actual),
        .groups = "drop"
      )

    rw_preds_list <- lapply(target_vars, function(var) {
      series_orig <- q_train_orig[[var]]
      preds_rw <- predict_rw_trend(series_orig, eval_horizon, var_label = var, context = "holdout")
      tibble::tibble(variable = var, step_ahead = seq_len(eval_horizon), rw_trend = preds_rw)
    })

    rw_eval <- dplyr::bind_rows(rw_preds_list) |>
      dplyr::left_join(actual_eval, by = c("variable", "step_ahead"))

    rw_metrics <- rw_eval |>
      dplyr::group_by(variable) |>
      dplyr::summarise(
        model = "RW-trend",
        rmse = safe_rmse(rw_trend, actual),
        mae = safe_mae(rw_trend, actual),
        .groups = "drop"
      )

    eval_table <- dplyr::bind_rows(mfvar_metrics, ar_metrics, rw_metrics) |>
      dplyr::arrange(variable, model)

    evaluation_path <- file.path(out_dir, "forecast_evaluation.csv")
    readr::write_csv(eval_table, evaluation_path)
  } else {
    message("Evaluation skipped: not enough observations left after reserving lags.")
  }

  list(
    table = eval_table,
    horizon = eval_horizon,
    path = evaluation_path
  )
}

run_cross_validation <- function(qdat_adj, qdat_orig, monthly_inputs, n_lags, target_vars, transforms, out_dir, max_folds = getOption("mfvar.cv_max_folds", Inf)) {
  # Expanding window cross-validation: training window starts from beginning
  # and expands to include more observations as we move forward in time
  cv_table <- NULL
  cv_path <- NULL
  folds_path <- NULL
  cv_folds <- 0
  max_cv <- nrow(qdat_adj) - (n_lags + 2)
  if (max_cv < 0) max_cv <- 0
  cv_horizon <- max_cv
  if (is.finite(max_folds)) {
    cv_horizon <- min(cv_horizon, max(0, as.integer(max_folds)))
  }

  if (cv_horizon >= 1) {
    cv_indices <- seq.int(nrow(qdat_adj) - cv_horizon + 1, nrow(qdat_adj))
    cv_records <- vector("list", length(cv_indices))

    for (i in seq_along(cv_indices)) {
      idx <- cv_indices[i]
      train_rows <- idx - 1
      if (train_rows <= n_lags) {
        cv_records[[i]] <- NULL
        next
      }

      # Expanding window: always train from row 1 to train_rows
      # As idx increases, train_rows increases, expanding the training set
      q_train_adj <- qdat_adj |> dplyr::slice_head(n = train_rows)
      q_train_orig <- qdat_orig |> dplyr::slice_head(n = train_rows)
      q_test_orig <- qdat_orig |> dplyr::slice(idx)

      baro_train_end <- quarter_to_month_end(q_train_orig$qtr[nrow(q_train_orig)])
      monthly_train <- if (is.list(monthly_inputs)) {
        window_monthly_series(monthly_inputs, q_train_orig)
      } else {
        stats::window(monthly_inputs, end = baro_train_end)
      }
      Y_cv <- build_Y(q_train_adj, monthly_train)

      mod_cv <- try(estimate_mfvar_model(Y_cv, n_lags, n_fcst = 1, seed = 200 + idx), silent = TRUE)
      if (inherits(mod_cv, "try-error")) {
        warning(sprintf("MF-VAR cross-validation fold %d failed: %s", idx, mod_cv))
        cv_records[[i]] <- NULL
        next
      }

      fc_cv_raw <- try(predict(mod_cv, aggregate_fcst = TRUE, pred_bands = 0.8), silent = TRUE)
      if (inherits(fc_cv_raw, "try-error")) {
        warning(sprintf("MF-VAR prediction failed for fold %d: %s", idx, fc_cv_raw))
        cv_records[[i]] <- NULL
        next
      }

      mfvar_fold <- fc_cv_raw |>
        dplyr::filter(variable %in% target_vars) |>
        dplyr::select(variable, mfvar = median) |>
        dplyr::mutate(time_index = compute_time_index(train_rows, 1L)) |>
        dplyr::mutate(mfvar = restore_series_values(mfvar, variable, time_index, transforms)) |>
        dplyr::select(variable, mfvar)

      actual_fold <- q_test_orig |>
        dplyr::select(tidyselect::all_of(target_vars)) |>
        tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "variable", values_to = "actual")

      ar_fold <- tibble::tibble(
        variable = target_vars,
        ar2 = vapply(target_vars, function(var) {
          series_adj <- q_train_adj[[var]]
          preds_adj <- predict_ar2(series_adj, 1, var_label = var, context = sprintf("CV fold %d", idx))
          restored <- restore_series_values(preds_adj, var, compute_time_index(train_rows, 1L), transforms)
          restored[1]
        }, numeric(1))
      )

      rw_fold <- tibble::tibble(
        variable = target_vars,
        rw_trend = vapply(target_vars, function(var) {
          series_orig <- q_train_orig[[var]]
          preds_rw <- predict_rw_trend(series_orig, 1, var_label = var, context = sprintf("CV fold %d", idx))
          preds_rw[1]
        }, numeric(1))
      )

      cv_records[[i]] <- actual_fold |>
        dplyr::left_join(mfvar_fold, by = "variable") |>
        dplyr::left_join(ar_fold, by = "variable") |>
        dplyr::left_join(rw_fold, by = "variable") |>
        dplyr::mutate(fold_index = idx)
    }

    cv_results <- dplyr::bind_rows(cv_records)
    if (nrow(cv_results)) {
      cv_folds <- dplyr::n_distinct(cv_results$fold_index)

      mfvar_cv <- cv_results |>
        dplyr::group_by(variable) |>
        dplyr::summarise(
          model = "MF-VAR",
          rmse = safe_rmse(mfvar, actual),
          mae = safe_mae(mfvar, actual),
          .groups = "drop"
        )

      ar_cv <- cv_results |>
        dplyr::group_by(variable) |>
        dplyr::summarise(
          model = "AR(2)",
          rmse = safe_rmse(ar2, actual),
          mae = safe_mae(ar2, actual),
          .groups = "drop"
        )

      rw_cv <- cv_results |>
        dplyr::group_by(variable) |>
        dplyr::summarise(
          model = "RW-trend",
          rmse = safe_rmse(rw_trend, actual),
          mae = safe_mae(rw_trend, actual),
          .groups = "drop"
        )

      cv_table <- dplyr::bind_rows(mfvar_cv, ar_cv, rw_cv) |>
        dplyr::arrange(variable, model)

      cv_path <- file.path(out_dir, "forecast_cross_validation.csv")
      readr::write_csv(cv_table, cv_path)

      folds_path <- file.path(out_dir, "forecast_cross_validation_folds.csv")
      readr::write_csv(cv_results, folds_path)
    } else {
      message("Cross-validation skipped: no valid folds produced.")
    }
  } else {
    message("Cross-validation skipped: not enough data for folds beyond lag length.")
  }

  list(
    table = cv_table,
    folds = cv_folds,
    horizon = cv_horizon,
    path = cv_path,
    folds_path = folds_path
  )
}
