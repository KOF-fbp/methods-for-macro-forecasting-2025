#!/usr/bin/env Rscript

# MIDAS Regression for Swiss Macro Forecasting
# ---------------------------------------------------------------
# Estimates MIDAS models using quarterly GDP/CPI/FX data with
# the monthly KOF Barometer. Produces forecasts and evaluation
# metrics for comparison with MF-VAR and other benchmarks.
# ---------------------------------------------------------------

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

activate_project()
required_midas <- c(required_pkgs, "midasr", "forecast")
load_required_packages(required_midas)

# --- Configuration -----------------------------------------------------------
DATA_DIR <- file.path(".", "data")
OUT_DIR  <- file.path(".", "output", "forecasts")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

n_lags <- 5  # For consistency with MF-VAR
forecast_horizons <- c(1L, 4L)  # 1-step and 1-year ahead

# --- Data Preparation --------------------------------------------------------
message("Loading and preparing data...")

qdat_raw <- read_quarterly_data(DATA_DIR)
baro_raw <- fetch_kof_barometer()
trimmed <- trim_to_overlap(qdat_raw, baro_raw)
qdat_orig <- trimmed$qdat
baro_ts <- window_baro(trimmed$baro_ts, qdat_orig)

# Prepare monthly differenced barometer
first_qtr_date <- zoo::as.Date(qdat_orig$qtr[1], frac = 0)
prev_year <- lubridate::year(first_qtr_date)
prev_month <- (lubridate::quarter(first_qtr_date) - 1) * 3

if (prev_month == 0) {
  prev_month <- 12
  prev_year <- prev_year - 1
}

baro_extended <- stats::window(baro_ts, start = c(prev_year, prev_month))
baro_diff <- base::diff(baro_extended)
first_month <- (lubridate::quarter(first_qtr_date) - 1) * 3 + 1
baro_diff <- stats::window(baro_diff, start = c(lubridate::year(first_qtr_date), first_month))

# Convert quarterly data to ts objects
target_vars <- target_variables
y_ts_list <- lapply(target_vars, function(var) {
  stats::ts(qdat_orig[[var]], 
            start = c(lubridate::year(first_qtr_date), lubridate::quarter(first_qtr_date)),
            frequency = 4)
})
names(y_ts_list) <- target_vars

# --- Estimation & Forecasting ------------------------------------------------
message("Estimating MIDAS models...")

n_obs <- nrow(qdat_orig)
train_size <- n_obs - max(forecast_horizons)

if (train_size <= n_lags) {
  stop("Insufficient data for MIDAS estimation with requested horizons.")
}

train_end_qtr <- qdat_orig$qtr[train_size]
train_end_month <- c(lubridate::year(zoo::as.Date(train_end_qtr, frac = 1)),
                     lubridate::quarter(zoo::as.Date(train_end_qtr, frac = 1)) * 3)

test_start_month <- c(lubridate::year(zoo::as.Date(train_end_qtr, frac = 1)),
                      (lubridate::quarter(zoo::as.Date(train_end_qtr, frac = 1))) * 3 + 1)
test_end_month <- c(lubridate::year(zoo::as.Date(qdat_orig$qtr[n_obs], frac = 1)),
                    lubridate::quarter(zoo::as.Date(qdat_orig$qtr[n_obs], frac = 1)) * 3)

x_train <- stats::window(baro_diff, end = train_end_month)
x_test <- stats::window(baro_diff, start = test_start_month, end = test_end_month)

# Fit MIDAS models for each target variable
results <- list()

for (var in target_vars) {
  message(sprintf("  • Fitting MIDAS for %s...", var))
  
  y_series <- y_ts_list[[var]]
  y_train <- stats::window(y_series, end = stats::time(y_series)[train_size])
  y_test <- stats::window(y_series, start = stats::time(y_series)[train_size + 1])
  
  trend_train <- seq_len(length(y_train))
  trend_test <- (length(y_train) + 1):(length(y_train) + length(y_test))
  
  # MIDAS with trend
  fit_trend <- tryCatch({
    midasr::midas_r(
      y_train ~ trend_train + mls(y_train, k = 1, m = 1) + fmls(x_train, k = 2, m = 3),
      start = list(x_train = rep(0, 3))
    )
  }, error = function(e) {
    warning(sprintf("MIDAS (trend) failed for %s: %s", var, conditionMessage(e)))
    NULL
  })
  
  # MIDAS without trend
  fit_simple <- tryCatch({
    midasr::midas_r(
      y_train ~ mls(y_train, k = 1, m = 1) + fmls(x_train, k = 2, m = 3),
      start = list(x_train = rep(0, 3))
    )
  }, error = function(e) {
    warning(sprintf("MIDAS (simple) failed for %s: %s", var, conditionMessage(e)))
    NULL
  })
  
  # Generate forecasts
  fc_trend <- if (!is.null(fit_trend)) {
    tryCatch({
      midasr::forecast(fit_trend, 
                      newdata = list(x_train = x_test, trend_train = trend_test),
                      h = length(y_test),
                      method = "dynamic")$mean
    }, error = function(e) {
      warning(sprintf("MIDAS (trend) forecast failed for %s", var))
      rep(NA_real_, length(y_test))
    })
  } else {
    rep(NA_real_, length(y_test))
  }
  
  fc_simple <- if (!is.null(fit_simple)) {
    tryCatch({
      midasr::forecast(fit_simple,
                      newdata = list(x_train = x_test),
                      h = length(y_test),
                      method = "dynamic")$mean
    }, error = function(e) {
      warning(sprintf("MIDAS (simple) forecast failed for %s", var))
      rep(NA_real_, length(y_test))
    })
  } else {
    rep(NA_real_, length(y_test))
  }
  
  results[[var]] <- list(
    fit_trend = fit_trend,
    fit_simple = fit_simple,
    forecast_trend = fc_trend,
    forecast_simple = fc_simple,
    actual = as.numeric(y_test),
    forecast_quarters = qdat_orig$qtr[(train_size + 1):n_obs]
  )
}

# --- Output Forecasts --------------------------------------------------------
message("Generating output files...")

forecast_df <- purrr::map_dfr(target_vars, function(var) {
  res <- results[[var]]
  n_fc <- length(res$actual)
  
  tibble::tibble(
    variable = var,
    quarter = as.character(res$forecast_quarters),
    quarter_end = zoo::as.Date(res$forecast_quarters, frac = 1),
    step_ahead = seq_len(n_fc),
    horizon = dplyr::case_when(
      step_ahead == 1 ~ "1-step ahead",
      step_ahead == 4 ~ "1-year ahead",
      TRUE ~ paste0(step_ahead, "-step ahead")
    ),
    midas_trend = res$forecast_trend,
    midas_simple = res$forecast_simple,
    actual = res$actual
  )
})

forecast_targets <- forecast_df |>
  dplyr::filter(step_ahead %in% forecast_horizons) |>
  dplyr::select(variable, horizon, quarter_end, midas_trend, midas_simple, actual)

readr::write_csv(forecast_df, file.path(OUT_DIR, "midas_forecasts_full.csv"))
readr::write_csv(forecast_targets, file.path(OUT_DIR, "midas_forecasts_targets.csv"))

# --- Evaluation --------------------------------------------------------------
eval_df <- forecast_df |>
  dplyr::filter(step_ahead %in% forecast_horizons) |>
  tidyr::pivot_longer(
    cols = c(midas_trend, midas_simple),
    names_to = "model",
    values_to = "forecast"
  ) |>
  dplyr::mutate(
    error = forecast - actual,
    squared_error = error^2,
    abs_error = abs(error)
  )

metrics <- eval_df |>
  dplyr::group_by(model, horizon) |>
  dplyr::summarise(
    rmse = sqrt(mean(squared_error, na.rm = TRUE)),
    mae = mean(abs_error, na.rm = TRUE),
    n_obs = sum(!is.na(error)),
    .groups = "drop"
  )

readr::write_csv(metrics, file.path(OUT_DIR, "midas_evaluation.csv"))

# --- Summary Output ----------------------------------------------------------
summary_path <- file.path(OUT_DIR, "midas_summary.txt")
sink(summary_path)

cat("\n==== MIDAS Regression Summary ====\n\n")
cat(sprintf("Training sample: %d quarters\n", train_size))
cat(sprintf("Forecast horizon: %d quarters\n", length(results[[1]]$actual)))
cat(sprintf("Target variables: %s\n", paste(target_vars, collapse = ", ")))

cat("\n==== Model Specifications ====\n\n")
for (var in target_vars) {
  cat(sprintf("\n--- %s ---\n", var))
  if (!is.null(results[[var]]$fit_trend)) {
    cat("\nMIDAS with trend:\n")
    print(summary(results[[var]]$fit_trend))
  } else {
    cat("\nMIDAS with trend: FAILED\n")
  }
  
  if (!is.null(results[[var]]$fit_simple)) {
    cat("\nMIDAS without trend:\n")
    print(summary(results[[var]]$fit_simple))
  } else {
    cat("\nMIDAS without trend: FAILED\n")
  }
}

cat("\n==== Forecast Evaluation ====\n\n")
print(metrics, n = nrow(metrics))

sink()

# --- Completion Message ------------------------------------------------------
message_lines <- c(
  "\nMIDAS pipeline complete. Wrote:\n",
  "  - output/forecasts/midas_forecasts_full.csv\n",
  "  - output/forecasts/midas_forecasts_targets.csv\n",
  "  - output/forecasts/midas_evaluation.csv\n",
  "  - output/forecasts/midas_summary.txt\n"
)

message(paste0(message_lines, collapse = ""))
