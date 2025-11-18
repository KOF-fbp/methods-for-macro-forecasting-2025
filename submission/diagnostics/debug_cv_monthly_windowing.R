#!/usr/bin/env Rscript
# Diagnostic: Check if monthly data windowing is actually working in CV

source("R/setup.R", local = FALSE)
source("R/data_processing.R", local = FALSE)
source("R/benchmark_shared.R", local = FALSE)

data_dir <- "data"
monthly_variables <- c("plkopr", "devkum", "amarbma", "snboffzisa")

# Load quarterly + monthly data
qdat_orig <- read_quarterly_data(data_dir)
monthly_raw <- read_combined_timeseries(data_dir, variables = monthly_variables)
trimmed <- trim_to_overlap(qdat_orig, monthly_raw$ts_list,
                           mode = "ragged", fill_method = "locf",
                           start_strategy = "fill")

# Take a fold near the end (2024 Q1 cutoff)
train_rows <- 129  # 2024 Q1
q_train_orig <- trimmed$qdat[1:train_rows, ]
train_last_qtr_cv <- q_train_orig$qtr[nrow(q_train_orig)]

message("Training cutoff quarter: ", train_last_qtr_cv)
message("Month-end equivalent: ", paste(quarter_to_month_end(train_last_qtr_cv), collapse = "-"))

# Test three coverage settings
for (extra_months in 0:2) {
  baro_end <- add_months(quarter_to_month_end(train_last_qtr_cv), extra_months)
  message(sprintf("\n=== Extra months: %d, baro_end: %s ===", 
                  extra_months, paste(baro_end, collapse = "-")))
  
  monthly_train_trimmed <- lapply(trimmed$monthly, function(ts_obj) {
    windowed <- stats::window(ts_obj, end = baro_end)
    message(sprintf("  Series: start=%s, end=%s, length=%d, last_value=%.4f",
                   paste(start(ts_obj), collapse = "-"),
                   paste(end(windowed), collapse = "-"),
                   length(windowed),
                   tail(as.numeric(windowed), 1)))
    windowed
  })
  
  # Check if build_Y produces different data structures
  stationary <- stationarise_quarterly(trimmed$qdat)
  q_train_adj <- stationary$data[1:train_rows, ]
  Y_test <- build_Y(q_train_adj, monthly_train_trimmed)
  message(sprintf("  Y monthly block: %d rows x %d cols",
                 nrow(Y_test$monthly[[1]]), ncol(Y_test$monthly[[1]])))
  message(sprintf("  Last monthly observation (plkopr): %.4f",
                 tail(Y_test$monthly[[1]][, "plkopr"], 1)))
}
