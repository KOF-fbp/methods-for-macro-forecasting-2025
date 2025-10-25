# Forecast evaluation utilities

variable <- step_ahead <- mfvar <- actual <- ar2 <- fold_index <- NULL

run_holdout_evaluation <- function(qdat, baro_ts, n_lags, target_vars, out_dir) {
  eval_table <- NULL
  evaluation_path <- NULL
  eval_horizon <- {
    max_holdout <- nrow(qdat) - (n_lags + 1)
    if (max_holdout < 0) max_holdout <- 0
    min(4, max_holdout)
  }

  if (eval_horizon >= 1) {
    q_train <- qdat |> dplyr::slice_head(n = nrow(qdat) - eval_horizon)
    q_eval <- qdat |> dplyr::slice_tail(n = eval_horizon)

    baro_train_end <- quarter_to_month_end(q_train$qtr[nrow(q_train)])
    baro_train <- stats::window(baro_ts, end = baro_train_end)
    Y_train <- build_Y(q_train, baro_train)

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

    actual_eval <- q_eval |>
      dplyr::mutate(step_ahead = dplyr::row_number()) |>
      dplyr::select(step_ahead, tidyselect::all_of(target_vars)) |>
  tidyr::pivot_longer(cols = -tidyselect::any_of("step_ahead"), names_to = "variable", values_to = "actual")

    mfvar_eval <- dplyr::inner_join(fc_eval, actual_eval, by = c("variable", "step_ahead"))
    mfvar_metrics <- mfvar_eval |>
      dplyr::group_by(variable) |>
      dplyr::summarise(
        model = "MF-VAR",
        rmse = safe_rmse(mfvar, actual),
        mae = safe_mae(mfvar, actual),
        .groups = "drop"
      )

    ar_preds_list <- lapply(target_vars, function(var) {
      series <- q_train[[var]]
      preds <- tryCatch({
        fit <- stats::arima(series, order = c(2, 0, 0))
        as.numeric(stats::predict(fit, n.ahead = eval_horizon)$pred)
      }, warning = function(w) {
        warning(sprintf("AR(2) warning for %s: %s", var, w$message))
        fit <- stats::arima(series, order = c(2, 0, 0))
        as.numeric(stats::predict(fit, n.ahead = eval_horizon)$pred)
      }, error = function(e) {
        warning(sprintf("AR(2) benchmark failed for %s: %s", var, e$message))
        rep(NA_real_, eval_horizon)
      })
      tibble::tibble(variable = var, step_ahead = seq_len(eval_horizon), ar2 = preds)
    })

    ar_eval <- dplyr::bind_rows(ar_preds_list) |>
      dplyr::inner_join(actual_eval, by = c("variable", "step_ahead"))

    ar_metrics <- ar_eval |>
      dplyr::group_by(variable) |>
      dplyr::summarise(
        model = "AR(2)",
        rmse = safe_rmse(ar2, actual),
        mae = safe_mae(ar2, actual),
        .groups = "drop"
      )

    eval_table <- dplyr::bind_rows(mfvar_metrics, ar_metrics) |>
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

run_cross_validation <- function(qdat, baro_ts, n_lags, target_vars, out_dir) {
  cv_table <- NULL
  cv_path <- NULL
  cv_folds <- 0
  cv_horizon <- {
    max_cv <- nrow(qdat) - (n_lags + 2)
    if (max_cv < 0) max_cv <- 0
    min(8, max_cv)
  }

  if (cv_horizon >= 1) {
    cv_indices <- seq.int(nrow(qdat) - cv_horizon + 1, nrow(qdat))
    cv_records <- vector("list", length(cv_indices))

    for (i in seq_along(cv_indices)) {
      idx <- cv_indices[i]
      train_rows <- idx - 1
      if (train_rows <= n_lags) {
        cv_records[[i]] <- NULL
        next
      }

      q_train <- qdat |> dplyr::slice_head(n = train_rows)
      q_test <- qdat |> dplyr::slice(idx)

      baro_train_end <- quarter_to_month_end(q_train$qtr[nrow(q_train)])
      baro_train <- stats::window(baro_ts, end = baro_train_end)
      Y_cv <- build_Y(q_train, baro_train)

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
        dplyr::select(variable, mfvar = median)

      actual_fold <- q_test |>
        dplyr::select(tidyselect::all_of(target_vars)) |>
        tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "variable", values_to = "actual")

      ar_fold <- tibble::tibble(
        variable = target_vars,
        ar2 = vapply(target_vars, function(var) {
          series <- q_train[[var]]
          tryCatch({
            fit <- withCallingHandlers(
              stats::arima(series, order = c(2, 0, 0)),
              warning = function(w) {
                warning(sprintf("AR(2) warning in fold %d for %s: %s", idx, var, w$message))
                invokeRestart("muffleWarning")
              }
            )
            as.numeric(stats::predict(fit, n.ahead = 1)$pred[1])
          }, error = function(e) {
            warning(sprintf("AR(2) benchmark failed in fold %d for %s: %s", idx, var, e$message))
            NA_real_
          })
        }, numeric(1))
      )

      cv_records[[i]] <- actual_fold |>
        dplyr::left_join(mfvar_fold, by = "variable") |>
        dplyr::left_join(ar_fold, by = "variable") |>
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

      cv_table <- dplyr::bind_rows(mfvar_cv, ar_cv) |>
        dplyr::arrange(variable, model)

      cv_path <- file.path(out_dir, "forecast_cross_validation.csv")
      readr::write_csv(cv_results, cv_path)
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
    path = cv_path
  )
}
