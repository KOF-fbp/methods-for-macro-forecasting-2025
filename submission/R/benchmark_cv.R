prepare_cv_plan <- function(qdat_adj, qdat_orig, forecast_steps, n_lags, extra_months_options, max_folds, initial_train_quarter) {
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
    return(list(valid = FALSE, reason = "Not enough observations to run cross-validation.", extra_months = extra_months_options))
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
    return(list(valid = FALSE, reason = "Cross-validation skipped: no folds selected after applying limits.", extra_months = extra_months_options))
  }

  list(
    valid = TRUE,
    horizon_max = horizon_max,
    horizon_steps = horizon_steps,
    horizon_months = horizon_months,
    cv_indices = cv_indices,
    total_folds = total_folds,
    extra_months = extra_months_options,
    n_obs = n_obs
  )
}

run_cv_fold <- function(idx,
                        horizon_max,
                        horizon_steps,
                        horizon_months,
                        qdat_adj,
                        qdat_orig,
                        transforms,
                        monthly_series_list,
                        baro_diff_series,
                        y_ts_list,
                        target_vars,
                        extra_months_options,
                        n_lags,
                        progress,
                        position,
                        total_folds) {
  train_rows <- idx - 1L
  if (train_rows <= n_lags) {
    if (progress) {
      message(sprintf("  • CV fold %02d/%02d | skipped (too few quarters)", position, total_folds))
    }
    return(NULL)
  }

  q_train_adj <- dplyr::slice_head(qdat_adj, n = train_rows)
  q_train_orig <- dplyr::slice_head(qdat_orig, n = train_rows)
  q_eval_orig <- dplyr::slice(qdat_orig, idx:(idx + horizon_max - 1L))

  cutoff_quarter <- q_train_orig$qtr[nrow(q_train_orig)]
  cutoff_label <- as.character(cutoff_quarter)
  forecast_quarters <- q_eval_orig$qtr
  quarter_end_dates <- zoo::as.Date(forecast_quarters, frac = 1)

  if (progress) {
    message(sprintf("  • CV fold %02d/%02d | training through %s", position, total_folds, cutoff_label))
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
  ar_generator <- make_ar_generator(
    q_train_adj = q_train_adj,
    transforms = transforms,
    time_indices = time_indices,
    horizon_steps = horizon_steps,
    base_context = sprintf("CV fold %d", idx)
  )
  ar_eval <- run_forecaster_with_timing(
    target_vars = target_vars,
    horizon_steps = horizon_steps,
    model_label = "AR(2)",
    generator_fn = ar_generator,
    warn_context = sprintf("for fold %s", cutoff_label),
    warn_on_all_na = FALSE
  )
  ar_base <- ar_eval$predictions

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

  mfvar_elapsed <- 0
  midas_kof_elapsed <- 0
  midas_latent_elapsed <- 0

  fold_predictions <- vector("list", length(extra_months_options))
  for (extra_idx in seq_along(extra_months_options)) {
    extra_months <- extra_months_options[extra_idx]
    extra_result <- assemble_extra_month_predictions(
      extra_months = extra_months,
      q_train_adj = q_train_adj,
      q_train_orig = q_train_orig,
      monthly_series_list = monthly_series_list,
      transforms = transforms,
      n_lags = n_lags,
      horizon_max = horizon_max,
      horizon_steps = horizon_steps,
      target_vars = target_vars,
      y_ts_list = y_ts_list,
      x_train_full = x_train_full,
      x_future_actual_vec = x_future_actual_vec,
      future_start = future_start,
      horizon_months = horizon_months,
      idx = idx,
      cutoff_label = cutoff_label,
      forecast_quarters = forecast_quarters,
      quarter_end_dates = quarter_end_dates,
      ar_base = ar_base,
      train_rows = train_rows
    )

    fold_predictions[[extra_idx]] <- extra_result$predictions
    mfvar_elapsed <- mfvar_elapsed + extra_result$timings$mfvar
    midas_kof_elapsed <- midas_kof_elapsed + extra_result$timings$midas_kof
    midas_latent_elapsed <- midas_latent_elapsed + extra_result$timings$midas_latent
  }

  combined_predictions <- dplyr::bind_rows(fold_predictions)
  if (!nrow(combined_predictions)) {
    return(NULL)
  }

  list(
    predictions = combined_predictions,
    actual = actual_fold,
    timings = list(
      fold_index = idx,
      mfvar = mfvar_elapsed,
      midas_kof = midas_kof_elapsed,
      midas_latent = midas_latent_elapsed,
      ar = ar_eval$elapsed,
      total = mfvar_elapsed + midas_kof_elapsed + midas_latent_elapsed + ar_eval$elapsed
    )
  )
}

execute_cv_folds <- function(plan,
                             qdat_adj,
                             qdat_orig,
                             transforms,
                             monthly_series_list,
                             baro_diff_series,
                             y_ts_list,
                             target_vars,
                             n_lags,
                             progress = TRUE) {
  predictions_list <- list()
  actual_list <- list()
  timings_info <- list()

  for (pos in seq_along(plan$cv_indices)) {
    idx <- plan$cv_indices[pos]
    fold_result <- run_cv_fold(
      idx = idx,
      horizon_max = plan$horizon_max,
      horizon_steps = plan$horizon_steps,
      horizon_months = plan$horizon_months,
      qdat_adj = qdat_adj,
      qdat_orig = qdat_orig,
      transforms = transforms,
      monthly_series_list = monthly_series_list,
      baro_diff_series = baro_diff_series,
      y_ts_list = y_ts_list,
      target_vars = target_vars,
      extra_months_options = plan$extra_months,
      n_lags = n_lags,
      progress = progress,
      position = pos,
      total_folds = plan$total_folds
    )

    if (!is.null(fold_result)) {
      predictions_list[[length(predictions_list) + 1L]] <- fold_result$predictions
      actual_list[[length(actual_list) + 1L]] <- fold_result$actual
      timings_info[[length(timings_info) + 1L]] <- fold_result$timings
    }
  }

  predictions_tbl <- if (length(predictions_list)) dplyr::bind_rows(predictions_list) else tibble::tibble()
  actual_tbl <- if (length(actual_list)) dplyr::bind_rows(actual_list) else tibble::tibble()
  timings_tbl <- if (length(timings_info)) {
    purrr::map_dfr(timings_info, function(tm) {
      tibble::tibble(
        fold_index = tm$fold_index,
        mfvar_seconds = tm$mfvar,
        midas_seconds = tm$midas_kof,
        midas_latent_seconds = tm$midas_latent,
        ar_seconds = tm$ar,
        total_seconds = tm$total
      )
    })
  } else {
    tibble::tibble()
  }

  list(
    predictions = predictions_tbl,
    actual = actual_tbl,
    timings = timings_tbl,
    processed_folds = if (nrow(actual_tbl)) dplyr::n_distinct(actual_tbl$fold_index) else 0L
  )
}

summarise_cv_results <- function(predictions_tbl, actual_tbl, forecast_steps) {
  if (!nrow(predictions_tbl) || !nrow(actual_tbl)) {
    return(list(
      results = tibble::tibble(),
      metrics_by_horizon = tibble::tibble(),
      metrics_overall = tibble::tibble(),
      folds = tibble::tibble(),
      fold_count = 0L,
      extra_values = integer()
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

run_benchmark_cross_validation <- function(
  qdat_adj,
  qdat_orig,
  transforms,
  monthly_series_list,
  baro_diff_series,
    y_ts_list,
    target_vars,
    forecast_steps,
    n_lags,
    extra_months_options = 0:2,
    max_folds = Inf,
    initial_train_quarter = zoo::as.yearqtr("2015 Q4"),
    progress = TRUE) {
  plan <- prepare_cv_plan(
    qdat_adj = qdat_adj,
    qdat_orig = qdat_orig,
    forecast_steps = forecast_steps,
    n_lags = n_lags,
    extra_months_options = extra_months_options,
    max_folds = max_folds,
    initial_train_quarter = initial_train_quarter
  )

  if (!isTRUE(plan$valid)) {
    if (!is.null(plan$reason)) warning(plan$reason, call. = FALSE)
    return(list(
      results = tibble::tibble(),
      metrics_by_horizon = tibble::tibble(),
      metrics_overall = tibble::tibble(),
      folds = tibble::tibble(),
      fold_count = 0L,
      extra_values = plan$extra_months,
      timings = list(per_fold = tibble::tibble(), totals = numeric())
    ))
  }

  fold_outputs <- execute_cv_folds(
    plan = plan,
    qdat_adj = qdat_adj,
    qdat_orig = qdat_orig,
    transforms = transforms,
    monthly_series_list = monthly_series_list,
    baro_diff_series = baro_diff_series,
    y_ts_list = y_ts_list,
    target_vars = target_vars,
    n_lags = n_lags,
    progress = progress
  )

  summaries <- summarise_cv_results(
    predictions_tbl = fold_outputs$predictions,
    actual_tbl = fold_outputs$actual,
    forecast_steps = forecast_steps
  )

  timing_totals <- if (nrow(fold_outputs$timings)) {
    colSums(dplyr::select(fold_outputs$timings, -fold_index))
  } else {
    numeric()
  }

  list(
    results = summaries$results,
    metrics_by_horizon = summaries$metrics_by_horizon,
    metrics_overall = summaries$metrics_overall,
    folds = summaries$folds,
    fold_count = summaries$fold_count,
    extra_values = summaries$extra_values,
    timings = list(per_fold = fold_outputs$timings, totals = timing_totals)
  )
}
