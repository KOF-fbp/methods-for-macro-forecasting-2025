#!/usr/bin/env Rscript

source(file.path("R", "setup.R"))
source(file.path("R", "data_processing.R"))
source(file.path("R", "evaluation.R"))

activate_project()

all_pkgs <- unique(c(required_pkgs, "midasr", "forecast"))
load_required_packages(all_pkgs)

DATA_DIR <- file.path(".", "data")
OUT_DIR <- file.path(".", "output")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

target_var <- "gdp_growth"
n_lags <- 5

trim_qtr_start <- function(q) {
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
eval_horizon <- min(4L, max_holdout)

if (eval_horizon < 1) {
  stop("Not enough observations left after reserving lags to run the benchmark comparison.")
}

train_rows <- nrow(qdat_adj) - eval_horizon
q_train_adj <- qdat_adj |> dplyr::slice_head(n = train_rows)
q_train_orig <- qdat_orig |> dplyr::slice_head(n = train_rows)
q_test_orig <- qdat_orig |> dplyr::slice_tail(n = eval_horizon)

baro_train_end <- quarter_to_month_end(q_train_orig$qtr[nrow(q_train_orig)])
baro_train <- stats::window(baro_ts, end = baro_train_end)
Y_train <- build_Y(q_train_adj, baro_train)

mod_mfvar <- estimate_mfvar_model(Y_train, n_lags, n_fcst = eval_horizon, seed = 123)

mfvar_fc <- predict(mod_mfvar, aggregate_fcst = TRUE, pred_bands = 0.8) |>
  dplyr::filter(variable == target_var) |>
  dplyr::arrange(time) |>
  dplyr::mutate(step_ahead = dplyr::row_number()) |>
  dplyr::filter(step_ahead <= eval_horizon)

mfvar_indices <- compute_time_index(nrow(q_train_adj), seq_len(eval_horizon))
mfvar_preds <- restore_series_values(
  mfvar_fc$median,
  rep(target_var, eval_horizon),
  mfvar_indices,
  transforms
)

ar_preds_adj <- predict_ar2(q_train_adj[[target_var]], eval_horizon, var_label = target_var, context = "holdout benchmark")
ar_preds <- restore_series_values(
  ar_preds_adj,
  rep(target_var, eval_horizon),
  mfvar_indices,
  transforms
)

rw_preds <- predict_rw_trend(q_train_orig[[target_var]], eval_horizon, var_label = target_var, context = "holdout benchmark")

y_start_date <- zoo::as.Date(qdat_orig$qtr[1], frac = 1)
y_start_vec <- c(lubridate::year(y_start_date), lubridate::quarter(y_start_date))
y_series <- stats::ts(qdat_orig[[target_var]], start = y_start_vec, frequency = 4)
y_train_ts <- stats::window(y_series, end = stats::time(y_series)[train_rows])
trend_train <- seq_len(length(y_train_ts))
trend_future <- trend_train[length(trend_train)] + seq_len(eval_horizon)

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
x_series <- stats::diff(xx0)
x_series <- stats::window(x_series, start = c(y_start_vec[1], first_month_of_quarter))

train_last_qtr <- q_train_orig$qtr[nrow(q_train_orig)]
train_last_month <- quarter_to_month_end(train_last_qtr)

x_train <- stats::window(x_series, end = train_last_month)

if (length(x_train) != length(y_train_ts) * 3L) {
  stop("Monthly regressor length does not match the training sample.")
}

test_first_qtr <- q_test_orig$qtr[1]
test_last_qtr <- q_test_orig$qtr[nrow(q_test_orig)]
test_start_month <- trim_qtr_start(test_first_qtr)
test_end_month <- quarter_to_month_end(test_last_qtr)

x_future <- stats::window(x_series, start = test_start_month, end = test_end_month)

if (length(x_future) != eval_horizon * 3L) {
  stop("Monthly regressor length does not cover the holdout horizon.")
}

fit_midas_safe <- function(formula_call, start_list, newdata, horizon) {
  fit <- try(do.call(midasr::midas_r, formula_call), silent = TRUE)
  if (inherits(fit, "try-error")) {
    warning(conditionMessage(attr(fit, "condition")))
    return(rep(NA_real_, horizon))
  }
  fc <- try(midasr::forecast(fit, newdata = newdata, h = horizon, method = "dynamic"), silent = TRUE)
  if (inherits(fc, "try-error")) {
    warning(conditionMessage(attr(fc, "condition")))
    return(rep(NA_real_, horizon))
  }
  as.numeric(fc$mean)
}

midas_formula_base <- list(
  formula = y_train_ts ~ midasr::mls(y_train_ts, k = 1, m = 1) + midasr::fmls(x_train, k = 2, m = 3),
  start = list(x_train = rep(0, 3))
)

midas_trend_preds <- fit_midas_safe(
  modifyList(midas_formula_base, list(formula = y_train_ts ~ trend_train + midasr::mls(y_train_ts, k = 1, m = 1) + midasr::fmls(x_train, k = 2, m = 3))),
  start_list = list(x_train = rep(0, 3)),
  newdata = list(x_train = x_future, trend_train = trend_future),
  horizon = eval_horizon
)

midas_simple_preds <- fit_midas_safe(
  midas_formula_base,
  start_list = list(x_train = rep(0, 3)),
  newdata = list(x_train = x_future, trend_train = trend_future),
  horizon = eval_horizon
)

actual_vals <- q_test_orig[[target_var]]

metrics_tbl <- tibble::tibble(
  model = c("MF-VAR", "MIDAS (trend)", "MIDAS", "AR(2)", "RW-trend"),
  rmse = c(
    safe_rmse(mfvar_preds, actual_vals),
    safe_rmse(midas_trend_preds, actual_vals),
    safe_rmse(midas_simple_preds, actual_vals),
    safe_rmse(ar_preds, actual_vals),
    safe_rmse(rw_preds, actual_vals)
  ),
  mae = c(
    safe_mae(mfvar_preds, actual_vals),
    safe_mae(midas_trend_preds, actual_vals),
    safe_mae(midas_simple_preds, actual_vals),
    safe_mae(ar_preds, actual_vals),
    safe_mae(rw_preds, actual_vals)
  )
)

comparison_tbl <- tibble::tibble(
  quarter = q_test_orig$qtr,
  actual = actual_vals,
  mfvar = mfvar_preds,
  midas_trend = midas_trend_preds,
  midas_simple = midas_simple_preds,
  ar2 = ar_preds,
  rw_trend = rw_preds
)

readr::write_csv(metrics_tbl, file.path(OUT_DIR, "model_benchmark_metrics.csv"))
readr::write_csv(comparison_tbl, file.path(OUT_DIR, "model_benchmark_forecasts.csv"))

cat("Benchmark comparison complete. Metrics written to output/model_benchmark_metrics.csv and forecasts to output/model_benchmark_forecasts.csv.\n")
