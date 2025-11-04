#!/usr/bin/env Rscript

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

all_pkgs <- unique(c(required_pkgs, "midasr", "forecast", "purrr"))
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

qdat_raw <- read_quarterly_data(DATA_DIR)
baro_raw <- fetch_kof_barometer()
trimmed <- trim_to_overlap(qdat_raw, baro_raw)
stationary <- stationarise_quarterly(trimmed$qdat)
qdat_orig <- trimmed$qdat
qdat_adj <- stationary$data
transforms <- stationary$transforms
baro_ts <- window_baro(trimmed$baro_ts, qdat_orig)

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

prev_year <- lubridate::year(zoo::as.Date(qdat_orig$qtr[1], frac = 0))
prev_month <- ((lubridate::quarter(zoo::as.Date(qdat_orig$qtr[1], frac = 0)) - 1L) * 3L)
if (prev_month == 0L) {
  prev_month <- 12L
  prev_year <- prev_year - 1L
}

xx0 <- stats::window(baro_ts, start = c(prev_year, prev_month))
x_series <- base::diff(xx0)
first_month_of_quarter <- ((lubridate::quarter(zoo::as.Date(qdat_orig$qtr[1], frac = 0)) - 1L) * 3L) + 1L
x_series <- stats::window(x_series, start = c(lubridate::year(zoo::as.Date(qdat_orig$qtr[1], frac = 0)), first_month_of_quarter))

train_last_qtr <- q_train_orig$qtr[nrow(q_train_orig)]
train_last_month <- quarter_to_month_end(train_last_qtr)
x_train_full <- stats::window(x_series, end = train_last_month)

if (length(x_train_full) != train_rows * 3L) {
  stop("Monthly regressor length does not match the training sample.")
}

test_start_month <- quarter_start_month(forecast_quarters[1])
test_end_month <- quarter_to_month_end(forecast_quarters[eval_horizon])
x_future_full <- stats::window(x_series, start = test_start_month, end = test_end_month)

if (length(x_future_full) != eval_horizon * 3L) {
  stop("Monthly regressor length does not cover the holdout horizon.")
}

stage_status(status = "done")

stage_status("Holdout evaluation", "start")

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

prev_year <- lubridate::year(zoo::as.Date(qdat_orig$qtr[1], frac = 0))
prev_month <- ((lubridate::quarter(zoo::as.Date(qdat_orig$qtr[1], frac = 0)) - 1L) * 3L)
if (prev_month == 0L) {
  prev_month <- 12L
  prev_year <- prev_year - 1L
}

xx0 <- stats::window(baro_ts, start = c(prev_year, prev_month))
x_series <- base::diff(xx0)
first_month_of_quarter <- ((lubridate::quarter(zoo::as.Date(qdat_orig$qtr[1], frac = 0)) - 1L) * 3L) + 1L
x_series <- stats::window(x_series, start = c(lubridate::year(zoo::as.Date(qdat_orig$qtr[1], frac = 0)), first_month_of_quarter))

train_last_qtr <- q_train_orig$qtr[nrow(q_train_orig)]
train_last_month <- quarter_to_month_end(train_last_qtr)
x_train_full <- stats::window(x_series, end = train_last_month)

if (length(x_train_full) != train_rows * 3L) {
  stop("Monthly regressor length does not match the training sample.")
}

test_start_month <- quarter_start_month(forecast_quarters[1])
test_end_month <- quarter_to_month_end(forecast_quarters[eval_horizon])
x_future_full <- stats::window(x_series, start = test_start_month, end = test_end_month)

if (length(x_future_full) != eval_horizon * 3L) {
  stop("Monthly regressor length does not cover the holdout horizon.")
}

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
# Cross-validation temporarily disabled; producing empty results.
cv_results <- tibble::tibble(
  variable = character(),
  step_ahead = integer(),
  prediction = numeric(),
  model = character(),
  actual = numeric(),
  fold = integer(),
  quarter_end = as.Date(character()),
  horizon = character(),
  error = numeric()
)
cv_metrics_tbl <- tibble::tibble(
  model = character(),
  horizon = character(),
  rmse = numeric(),
  mae = numeric(),
  observations = integer()
)
stage_status(status = "skip")

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
  dplyr::mutate(
    RMSE = sprintf("%.4f", rmse),
    MAE = sprintf("%.4f", mae),
    Observations = as.character(observations)
  ) |>
  dplyr::select(model, horizon, Observations, RMSE, MAE)

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
  "## Rolling 1-step Cross-Validation RMSE and MAE",
  table_to_markdown(summary_cv_tbl, c("Model", "Horizon", "Observations", "RMSE", "MAE"))
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

readr::write_csv(metrics_tbl, file.path(OUT_DIR, "model_benchmark_metrics.csv"))
readr::write_csv(holdout_metrics_detailed, file.path(OUT_DIR, "model_benchmark_holdout_detailed.csv"))
readr::write_csv(forecast_wide, file.path(OUT_DIR, "model_benchmark_forecasts.csv"))
readr::write_csv(cv_metrics_tbl, file.path(OUT_DIR, "model_benchmark_cv_metrics.csv"))
readr::write_csv(cv_results, file.path(OUT_DIR, "model_benchmark_cv_predictions.csv"))

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
