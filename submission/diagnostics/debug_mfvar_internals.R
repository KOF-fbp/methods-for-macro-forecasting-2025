#!/usr/bin/env Rscript
# Diagnostic: Check if MCMC estimation is actually running fresh each time

source("R/setup.R", local = FALSE)
source("R/data_processing.R", local = FALSE)
source("R/benchmark_shared.R", local = FALSE)

data_dir <- "data"
monthly_variables <- c("plkopr", "devkum", "amarbma", "snboffzisa")
target_vars <- c("gdp_growth", "inflation", "exch_rate")
n_lags <- 5L

# Load data
qdat_orig <- read_quarterly_data(data_dir)
monthly_raw <- read_combined_timeseries(data_dir, variables = monthly_variables)
trimmed <- trim_to_overlap(qdat_orig, monthly_raw$ts_list,
                           mode = "ragged", fill_method = "locf",
                           start_strategy = "fill")
stationary <- stationarise_quarterly(trimmed$qdat)
qdat_adj <- stationary$data
transforms <- stationary$transforms

# Use fold 129
idx <- 129
train_rows <- 129
q_train_adj <- qdat_adj[1:train_rows, ]
q_train_orig <- trimmed$qdat[1:train_rows, ]
train_last_qtr_cv <- q_train_orig$qtr[nrow(q_train_orig)]

# Test with +0m and +2m (should show max difference)
for (extra_months in c(0, 2)) {
  baro_end <- add_months(quarter_to_month_end(train_last_qtr_cv), extra_months)
  monthly_train_trimmed <- lapply(trimmed$monthly, function(ts_obj) {
    stats::window(ts_obj, end = baro_end)
  })
  
  seed_val <- 1000L + idx * 10L + extra_months
  message(sprintf("\n=== Extra months: %d, seed: %d ===", extra_months, seed_val))
  
  # Build Y and check dimensions
  Y_train <- build_Y(q_train_adj, monthly_train_trimmed)
  message(sprintf("Data structure built. Components: %s", 
                 paste(names(Y_train), collapse = ", ")))
  
  # Check monthly series lengths
  for (nm in names(Y_train)[1:4]) {
    mon_ts <- Y_train[[nm]]
    if (is.ts(mon_ts)) {
      message(sprintf("  %s: ts with %d observations, last value: %.4f",
                     nm, length(mon_ts), tail(as.numeric(mon_ts), 1)))
    } else if (is.matrix(mon_ts)) {
      message(sprintf("  %s: %d x %d matrix", nm, nrow(mon_ts), ncol(mon_ts)))
    }
  }
  
  # Run estimation with verbose output
  message("\nEstimating MF-VAR...")
  mod <- estimate_mfvar_model(
    Y_train,
    n_lags,
    n_fcst = 12 * 3,  # 12 months = 4 quarters
    seed = seed_val
  )
  
  # Check model object
  message(sprintf("Model class: %s", paste(class(mod), collapse = ", ")))
  message(sprintf("Number of variables: %d", ncol(mod$Y)))
  
  # Extract fitted values for last period
  fitted_last <- tail(fitted(mod), 1)
  message(sprintf("Last fitted values: %s", 
                 paste(round(fitted_last[target_vars], 4), collapse = ", ")))
  
  # Generate forecast
  fc <- predict(mod, aggregate_fcst = TRUE, pred_bands = 0.8) |>
    dplyr::filter(variable %in% target_vars, step_ahead == 1)
  message("\n1-step forecast (raw median from mfbvar):")
  print(fc[, c("variable", "median")])
}
