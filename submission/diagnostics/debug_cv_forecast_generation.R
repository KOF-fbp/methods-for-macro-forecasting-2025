#!/usr/bin/env Rscript
# Diagnostic: Verify MF-VAR generates different forecasts with different monthly inputs

source("R/setup.R", local = FALSE)
source("R/data_processing.R", local = FALSE)
source("R/benchmark_shared.R", local = FALSE)

data_dir <- "data"
monthly_variables <- c("plkopr", "devkum", "amarbma", "snboffzisa")
target_vars <- c("gdp_growth", "inflation", "exch_rate")
n_lags <- 5L

# Load data
qdat_orig <- read_quarterly_data(data_dir)
monthly_variables <- resolve_monthly_indicators()
monthly_raw <- read_combined_timeseries(data_dir, variables = monthly_variables)
trimmed <- trim_to_overlap(qdat_orig, monthly_raw$ts_list,
                           mode = "ragged", fill_method = "locf",
                           start_strategy = "fill")
stationary <- stationarise_quarterly(trimmed$qdat)
qdat_adj <- stationary$data
transforms <- stationary$transforms

# Use fold 129 (2024 Q1 cutoff)
idx <- 129
train_rows <- 129
q_train_adj <- qdat_adj[1:train_rows, ]
q_train_orig <- trimmed$qdat[1:train_rows, ]
train_last_qtr_cv <- q_train_orig$qtr[nrow(q_train_orig)]

message("Training cutoff quarter: ", train_last_qtr_cv)
message("idx = ", idx, "\n")

# Test three coverage settings
for (extra_months in 0:2) {
  baro_end <- add_months(quarter_to_month_end(train_last_qtr_cv), extra_months)
  monthly_train_trimmed <- lapply(trimmed$monthly, function(ts_obj) {
    stats::window(ts_obj, end = baro_end)
  })
  
  seed_val <- 1000L + idx * 10L + extra_months
  message(sprintf("=== Extra months: %d, seed: %d ===", extra_months, seed_val))
  
  # Run MF-VAR forecaster
  result <- forecast_mfvar(
    q_train_adj,
    monthly_train_trimmed,
    transforms,
    n_lags,
    horizon_quarters = 12L,
    target_vars,
    seed = seed_val,
    return_model = FALSE,
    extract_states = FALSE
  )
  
  # Print 1-step predictions
  step1 <- result$predictions |> dplyr::filter(step_ahead == 1)
  print(step1)
  message("")
}
