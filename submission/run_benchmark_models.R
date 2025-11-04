#!/usr/bin/env Rscript

source(file.path("R", "setup.R"))
source(file.path("R", "data_processing.R"))
source(file.path("R", "evaluation.R"))

activate_project()

all_pkgs <- unique(c(required_pkgs, "midasr", "forecast", "purrr"))
load_required_packages(all_pkgs)

DATA_DIR <- file.path(".", "data")
OUT_DIR <- file.path(".", "output")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

target_vars <- target_variables
forecast_steps <- c(1L, 4L)
n_lags <- 5

quarter_start_month <- function(q) {
  q_date <- zoo::as.Date(q, frac = 0)
  c(lubridate::year(q_date), lubridate::month(q_date))
}

qdat_raw <- read_quarterly_data(DATA_DIR)
baro_raw <- fetch_kof_barometer()
trimmed <- trim_to_overlap(qdat_raw, baro_raw)
stationary <- stationarise_quarterly(trimmed$qdat)
qdat_orig <- trimmed$qdat
qdat_adj <- stationary$data
transforms <- stationary$transforms
baro_ts <- window_baro(trimmed$baro_ts, qdat_orig)

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
forecast_dates <- zoo::as.Date(q_test_orig$qtr, frac = 1)

baro_train_end <- quarter_to_month_end(q_train_orig$qtr[nrow(q_train_orig)])
baro_train <- stats::window(baro_ts, end = baro_train_end)
Y_train <- build_Y(q_train_adj, baro_train)

# --- MF-VAR forecasts ------------------------------------------------------
mod_mfvar <- estimate_mfvar_model(Y_train, n_lags, n_fcst = eval_horizon * 3, seed = 123)

mfvar_fc <- predict(mod_mfvar, aggregate_fcst = TRUE, pred_bands = 0.8) |>
  dplyr::filter(variable %in% target_vars) |>
  dplyr::arrange(variable, time) |>
  dplyr::group_by(variable) |>
  dplyr::mutate(step_ahead = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::filter(step_ahead <= eval_horizon)

mfvar_preds <- tibble::tibble(
  variable = mfvar_fc$variable,
  step_ahead = mfvar_fc$step_ahead,
  model = "MF-VAR",
  prediction = restore_series_values(
    mfvar_fc$median,
    mfvar_fc$variable,
    compute_time_index(nrow(q_train_adj), mfvar_fc$step_ahead),
    transforms
  )
)

# --- Baseline forecasts ----------------------------------------------------
ar_preds <- purrr::map_dfr(target_vars, function(var) {
  preds_adj <- predict_ar2(q_train_adj[[var]], eval_horizon, var_label = var, context = "holdout benchmark")
  tibble::tibble(
    variable = var,
    step_ahead = seq_len(eval_horizon),
    prediction = restore_series_values(
      preds_adj,
      rep(var, eval_horizon),
      compute_time_index(nrow(q_train_adj), seq_len(eval_horizon)),
      transforms
    )
  )
}) |>
  dplyr::mutate(model = "AR(2)")

rw_preds <- purrr::map_dfr(target_vars, function(var) {
  tibble::tibble(
    variable = var,
    step_ahead = seq_len(eval_horizon),
    prediction = predict_rw_trend(q_train_orig[[var]], eval_horizon, var_label = var, context = "holdout benchmark")
  )
}) |>
  dplyr::mutate(model = "RW-trend")

# --- MIDAS forecasts -------------------------------------------------------
y_start_date <- zoo::as.Date(qdat_orig$qtr[1], frac = 1)
y_start_vec <- c(lubridate::year(y_start_date), lubridate::quarter(y_start_date))
y_start_quarter <- y_start_vec[2]
first_month_of_quarter <- (y_start_quarter - 1L) * 3L + 1L
prev_month <- first_month_of_quarter - 1L
if (prev_month == 0L) {
  prev_month <- 12L
  prev_year <- y_start_vec[1] - 1L
} else {
  prev_year <- y_start_vec[1]
}

xx0 <- stats::window(baro_ts, start = c(prev_year, prev_month))
x_series <- base::diff(xx0)
x_series <- stats::window(x_series, start = c(y_start_vec[1], first_month_of_quarter))

train_last_qtr <- q_train_orig$qtr[nrow(q_train_orig)]
train_last_month <- quarter_to_month_end(train_last_qtr)
x_train <- stats::window(x_series, end = train_last_month)

if (length(x_train) != train_rows * 3L) {
  stop("Monthly regressor length does not match the training sample.")
}

test_first_qtr <- q_test_orig$qtr[1]
test_last_qtr <- q_test_orig$qtr[nrow(q_test_orig)]
test_start_month <- quarter_start_month(test_first_qtr)
test_end_month <- quarter_to_month_end(test_last_qtr)
x_future <- stats::window(x_series, start = test_start_month, end = test_end_month)

if (length(x_future) != eval_horizon * 3L) {
  stop("Monthly regressor length does not cover the holdout horizon.")
}

fit_midas_model <- function(var, include_trend) {
  y_series <- stats::ts(qdat_orig[[var]], start = y_start_vec, frequency = 4)
  y_train <- stats::window(y_series, end = stats::time(y_series)[train_rows])
  trend_train <- seq_len(length(y_train))
  trend_future <- trend_train[length(trend_train)] + seq_len(eval_horizon)

  env <- new.env(parent = globalenv())
  env$y <- y_train
  env$x <- x_train
  env$trend <- trend_train

  base_formula <- stats::as.formula("y ~ mls(y, k = 1, m = 1) + fmls(x, k = 2, m = 3)")
  environment(base_formula) <- env

  trend_formula <- stats::as.formula("y ~ trend + mls(y, k = 1, m = 1) + fmls(x, k = 2, m = 3)")
  environment(trend_formula) <- env

  formula_to_use <- if (isTRUE(include_trend)) trend_formula else base_formula
  start_list <- list(x = rep(0, 3))

  fit <- try(midasr::midas_r(formula_to_use, start = start_list), silent = TRUE)
  if (inherits(fit, "try-error")) {
    warning(as.character(fit), call. = FALSE)
    return(rep(NA_real_, eval_horizon))
  }

  newdata <- list(x = x_future)
  if (isTRUE(include_trend)) {
    newdata$trend <- trend_future
  }

  fc <- try(midasr::forecast(fit, newdata = newdata, h = eval_horizon, method = "dynamic"), silent = TRUE)
  if (inherits(fc, "try-error")) {
    warning(as.character(fc), call. = FALSE)
    return(rep(NA_real_, eval_horizon))
  }

  as.numeric(fc$mean)
}

midas_results <- purrr::map_dfr(target_vars, function(var) {
  preds_trend <- fit_midas_model(var, include_trend = TRUE)
  preds_simple <- fit_midas_model(var, include_trend = FALSE)

  dplyr::bind_rows(
    tibble::tibble(variable = var, step_ahead = seq_len(eval_horizon), model = "MIDAS (trend)", prediction = preds_trend),
    tibble::tibble(variable = var, step_ahead = seq_len(eval_horizon), model = "MIDAS", prediction = preds_simple)
  )
})

# --- Gather predictions ----------------------------------------------------
predictions_tbl <- dplyr::bind_rows(
  mfvar_preds,
  midas_results,
  ar_preds,
  rw_preds
) |>
  dplyr::mutate(
    quarter_end = forecast_dates[step_ahead],
    horizon = dplyr::case_when(
      step_ahead == 1L ~ "1-step ahead",
      step_ahead == 4L ~ "1-year ahead",
      TRUE ~ "Other"
    )
  )

actual_tbl <- q_test_orig |>
  dplyr::mutate(step_ahead = dplyr::row_number()) |>
  dplyr::select(step_ahead, tidyselect::all_of(target_vars)) |>
  tidyr::pivot_longer(cols = -step_ahead, names_to = "variable", values_to = "actual") |>
  dplyr::mutate(
    quarter_end = forecast_dates[step_ahead],
    horizon = dplyr::case_when(
      step_ahead == 1L ~ "1-step ahead",
      step_ahead == 4L ~ "1-year ahead",
      TRUE ~ "Other"
    )
  )

# --- Metrics for specified horizons ----------------------------------------
metrics_tbl <- predictions_tbl |>
  dplyr::inner_join(actual_tbl, by = c("variable", "step_ahead", "quarter_end", "horizon")) |>
  dplyr::filter(step_ahead %in% forecast_steps) |>
  dplyr::mutate(error = prediction - actual) |>
  dplyr::group_by(variable, model, horizon) |>
  dplyr::summarise(
    rmse = sqrt(mean(error^2, na.rm = TRUE)),
    mae = mean(abs(error), na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(variable, horizon, model)

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

# --- Plot ------------------------------------------------------------------
convert_for_plot <- function(value, var) {
  ifelse(var == "exch_rate", exp(value), value)
}

plot_predictions <- predictions_tbl |>
  dplyr::filter(step_ahead %in% forecast_steps) |>
  dplyr::mutate(display_value = convert_for_plot(prediction, variable))

plot_actual <- actual_tbl |>
  dplyr::filter(step_ahead %in% forecast_steps) |>
  dplyr::mutate(model = "Actual", display_value = convert_for_plot(actual, variable))

plot_tbl <- dplyr::bind_rows(
  plot_predictions |> dplyr::mutate(model = model),
  plot_actual |> dplyr::select(variable, step_ahead, quarter_end, horizon, model, display_value)
)

plot_tbl$model <- factor(plot_tbl$model, levels = c("Actual", "MF-VAR", "MIDAS (trend)", "MIDAS", "AR(2)", "RW-trend"))

plot_path <- file.path(OUT_DIR, "model_benchmark_plot.png")
ggplot2::ggplot(plot_tbl, ggplot2::aes(x = quarter_end, y = display_value, colour = model)) +
  ggplot2::geom_line(ggplot2::aes(linetype = model)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~variable, scales = "free_y") +
  ggplot2::scale_colour_manual(values = c(
    "Actual" = "#000000",
    "MF-VAR" = "#1b9e77",
    "MIDAS (trend)" = "#7570b3",
    "MIDAS" = "#d95f02",
    "AR(2)" = "#e7298a",
    "RW-trend" = "#66a61e"
  )) +
  ggplot2::scale_linetype_manual(values = c(
    "Actual" = "solid",
    "MF-VAR" = "solid",
    "MIDAS (trend)" = "dashed",
    "MIDAS" = "dashed",
    "AR(2)" = "dotted",
    "RW-trend" = "dotdash"
  )) +
  ggplot2::labs(
    title = "Holdout forecasts by model",
    subtitle = "1-step and 1-year ahead horizons",
    x = "Quarter end",
    y = "Value",
    colour = NULL,
    linetype = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(plot_path, width = 10, height = 6, dpi = 150)

# --- Persist results --------------------------------------------------------
readr::write_csv(metrics_tbl, file.path(OUT_DIR, "model_benchmark_metrics.csv"))
readr::write_csv(forecast_wide, file.path(OUT_DIR, "model_benchmark_forecasts.csv"))

cat("Benchmark comparison complete. Metrics written to output/model_benchmark_metrics.csv, forecasts to output/model_benchmark_forecasts.csv, and plot to output/model_benchmark_plot.png.\n")
