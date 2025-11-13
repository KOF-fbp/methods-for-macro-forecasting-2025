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

start_time <- Sys.time()

# --- Configuration -----------------------------------------------------------
DATA_DIR <- file.path(".", "data")
OUT_DIR  <- file.path(".", "output", "forecasts", "midas")
CSV_DIR  <- file.path(OUT_DIR, "csv")
PLOT_DIR <- file.path(OUT_DIR, "plots")
MODEL_DIR <- file.path(OUT_DIR, "models")
for (dir in c(OUT_DIR, CSV_DIR, PLOT_DIR, MODEL_DIR)) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

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

# Use full sample for training (no holdout - we want newest forecasts)
train_size <- n_obs

# Get the end month for the full quarterly data
train_end_qtr <- qdat_orig$qtr[train_size]
train_end_month <- c(lubridate::year(zoo::as.Date(train_end_qtr, frac = 1)),
                     lubridate::quarter(zoo::as.Date(train_end_qtr, frac = 1)) * 3)

# Use all available monthly data up to the last complete quarter
x_train <- stats::window(baro_diff, end = train_end_month)

# For forecasting, we need future monthly values
# MIDAS forecast requires monthly data for the forecast horizon
# Since we forecast 4 quarters ahead (1 year), we need 4*3 = 12 months of monthly data
# Check if we have that available, otherwise use last value as naive forecast
forecast_quarters <- 4
forecast_months_needed <- forecast_quarters * 3

last_month <- train_end_month
new_x_start <- c(last_month[1], last_month[2] + 1)
if (new_x_start[2] > 12) {
  new_x_start[1] <- new_x_start[1] + (new_x_start[2] - 1) %/% 12
  new_x_start[2] <- ((new_x_start[2] - 1) %% 12) + 1
}

# Calculate end month for forecast horizon
years_ahead <- (new_x_start[2] + forecast_months_needed - 1) %/% 12
months_ahead <- ((new_x_start[2] + forecast_months_needed - 1) %% 12) + 1
new_x_end <- c(new_x_start[1] + years_ahead, months_ahead)

# Check what monthly data is actually available
x_available_end <- stats::end(baro_diff)
months_available <- (x_available_end[1] - new_x_start[1]) * 12 + (x_available_end[2] - new_x_start[2]) + 1

if (months_available >= forecast_months_needed) {
  # We have enough future monthly data
  x_new <- stats::window(baro_diff, start = new_x_start, end = new_x_end)
} else if (months_available > 0) {
  # We have some future data, but not enough - extend with last value
  x_partial <- stats::window(baro_diff, start = new_x_start, end = x_available_end)
  last_val <- tail(as.numeric(x_partial), 1)
  n_missing <- forecast_months_needed - months_available
  
  # Create full series with available + extended
  x_full <- c(as.numeric(x_partial), rep(last_val, n_missing))
  x_new <- stats::ts(x_full, start = new_x_start, frequency = 12)
} else {
  # No future monthly data - use last training value
  last_val <- tail(as.numeric(baro_diff), 1)
  x_new <- stats::ts(rep(last_val, forecast_months_needed), start = new_x_start, frequency = 12)
}

# Fit MIDAS models for each target variable
n_forecast_quarters <- 12
results <- list()

for (var in target_vars) {
  message(sprintf("  • Fitting MIDAS for %s...", var))
  
  y_series <- y_ts_list[[var]]
  y_train <- y_series  # Use full series
  
  trend_train <- seq_len(length(y_train))
  
  # Future trend values for forecasting
  n_forecast_quarters <- 4
  trend_forecast <- (length(y_train) + 1):(length(y_train) + n_forecast_quarters)
  
  # MIDAS with trend - use data parameter pattern with AR(2)
  fit_trend <- tryCatch({
    data_list <- list(y = y_train, x = x_train, trend = trend_train)
    formula_obj <- stats::as.formula("y ~ trend + mls(y, k = 1:2, m = 1) + fmls(x, k = 2, m = 3)")
    midasr::midas_r(formula_obj, data = data_list, start = list(x = rep(0, 3)))
  }, error = function(e) {
    warning(sprintf("MIDAS (trend) failed for %s: %s", var, conditionMessage(e)))
    NULL
  })
  
  # MIDAS without trend - use data parameter pattern with AR(2)
  fit_simple <- tryCatch({
    data_list <- list(y = y_train, x = x_train)
    formula_obj <- stats::as.formula("y ~ mls(y, k = 1:2, m = 1) + fmls(x, k = 2, m = 3)")
    midasr::midas_r(formula_obj, data = data_list, start = list(x = rep(0, 3)))
  }, error = function(e) {
    warning(sprintf("MIDAS (simple) failed for %s: %s", var, conditionMessage(e)))
    NULL
  })
  
  # Generate forecasts
  fc_trend <- if (!is.null(fit_trend)) {
    tryCatch({
      midasr::forecast(fit_trend, 
                      newdata = list(x = x_new, trend = trend_forecast),
                      h = n_forecast_quarters,
                      method = "dynamic")$mean
    }, error = function(e) {
      warning(sprintf("MIDAS (trend) forecast failed for %s", var))
      rep(NA_real_, n_forecast_quarters)
    })
  } else {
    rep(NA_real_, n_forecast_quarters)
  }
  
  fc_simple <- if (!is.null(fit_simple)) {
    tryCatch({
      midasr::forecast(fit_simple,
                      newdata = list(x = x_new),
                      h = n_forecast_quarters,
                      method = "dynamic")$mean
    }, error = function(e) {
      warning(sprintf("MIDAS (simple) forecast failed for %s", var))
      rep(NA_real_, n_forecast_quarters)
    })
  } else {
    rep(NA_real_, n_forecast_quarters)
  }
  
  # Generate future quarter dates
  last_qtr <- qdat_orig$qtr[n_obs]
  forecast_quarters <- last_qtr + (1:n_forecast_quarters) / 4
  
  results[[var]] <- list(
    fit_trend = fit_trend,
    fit_simple = fit_simple,
    forecast_trend = fc_trend,
    forecast_simple = fc_simple,
    forecast_quarters = forecast_quarters
  )
}

# --- Output Forecasts --------------------------------------------------------
message("Generating output files...")

forecast_df <- purrr::map_dfr(target_vars, function(var) {
  res <- results[[var]]
  n_fc <- length(res$forecast_quarters)
  
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
    midas_simple = res$forecast_simple
  )
})

forecast_targets <- forecast_df |>
  dplyr::filter(step_ahead %in% forecast_horizons) |>
  dplyr::select(variable, horizon, quarter_end, midas_trend, midas_simple)

readr::write_csv(forecast_df, file.path(CSV_DIR, "midas_forecasts_full.csv"))
readr::write_csv(forecast_targets, file.path(CSV_DIR, "midas_forecasts_targets.csv"))

# --- Plots -------------------------------------------------------------------

plot_paths <- list()

for (var in target_vars) {
  var_data <- forecast_df |>
    dplyr::filter(variable == var)
  
  if (!nrow(var_data)) next
  
  # Prepare historical data (from 2023 onwards for context)
  hist_subset <- qdat_orig |>
    dplyr::filter(.data$qtr >= zoo::as.yearqtr("2023 Q1"))
  
  hist_df <- tibble::tibble(
    time = zoo::as.Date(hist_subset$qtr, frac = 1),
    value = if (var == "exch_rate") exp(hist_subset[[var]]) else hist_subset[[var]]
  )
  
  last_actual <- max(hist_df$time)
  forecast_subset <- var_data |>
    dplyr::mutate(
      time = quarter_end,
      midas_trend = if (var == "exch_rate") exp(midas_trend) else midas_trend,
      midas_simple = if (var == "exch_rate") exp(midas_simple) else midas_simple
    )
  
  # Create anchor point to join history with forecasts
  first_fc_date <- min(forecast_subset$time)
  if (first_fc_date > last_actual) {
    last_value <- tail(hist_df$value, 1)
    anchor <- tibble::tibble(
      time = last_actual,
      midas_trend = last_value,
      midas_simple = last_value
    )
    forecast_subset <- dplyr::bind_rows(anchor, forecast_subset) |>
      dplyr::arrange(time)
  }
  
  # Determine variable-specific labels
  var_labels <- list(
    gdp_growth = list(title = "GDP growth: history and MIDAS forecasts", ylab = "Annualised percentage"),
    inflation = list(title = "Inflation: history and MIDAS forecasts", ylab = "Annualised percentage"),
    exch_rate = list(title = "Exchange rate: history and MIDAS forecasts", ylab = "CHF per EUR")
  )
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = hist_df, ggplot2::aes(x = time, y = value), colour = "#4c4c4c") +
    ggplot2::geom_vline(xintercept = last_actual, linetype = "dotted", colour = "#4c4c4c") +
    ggplot2::geom_line(
      data = forecast_subset,
      ggplot2::aes(x = time, y = midas_trend, colour = "MIDAS (with trend)"),
      linewidth = 1
    ) +
    ggplot2::geom_line(
      data = forecast_subset,
      ggplot2::aes(x = time, y = midas_simple, colour = "MIDAS (simple)"),
      linewidth = 1,
      linetype = "dashed"
    ) +
    ggplot2::scale_x_date(labels = function(x) format(zoo::as.yearqtr(x), "%Y Q%q")) +
    ggplot2::scale_colour_manual(
      name = NULL,
      values = c("MIDAS (with trend)" = "#7570b3", "MIDAS (simple)" = "#e7298a")
    ) +
    ggplot2::labs(
      title = var_labels[[var]]$title,
      subtitle = "Comparison of MIDAS specifications",
      x = "Quarter",
      y = var_labels[[var]]$ylab
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top")
  
  plot_path <- file.path(PLOT_DIR, sprintf("midas_forecast_%s_context.png", var))
  ggplot2::ggsave(plot_path, p, width = 8, height = 4.5, dpi = 120)
  plot_paths[[var]] <- plot_path
}

# --- Persist models ----------------------------------------------------------
model_list <- lapply(target_vars, function(var) {
  list(
    fit_trend = results[[var]]$fit_trend,
    fit_simple = results[[var]]$fit_simple
  )
})
names(model_list) <- target_vars
saveRDS(model_list, file.path(MODEL_DIR, "midas_models.rds"))

# --- Summary Output ----------------------------------------------------------
summary_path <- file.path(OUT_DIR, "midas_summary.txt")
sink(summary_path)

cat("\n==== MIDAS Regression Summary ====\n\n")
cat(sprintf("Training sample: %d quarters\n", train_size))
cat(sprintf("Forecast horizon: %d quarters ahead (1 year)\n", 4))
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

cat("\n==== Note ====\n")
cat("For model evaluation and benchmarking, see run_benchmarks.R\n")

sink()

# --- Completion Message ------------------------------------------------------
message_lines <- c(
  "\nMIDAS pipeline complete. Wrote:\n",
  "  - output/forecasts/midas/midas_summary.txt\n",
  "  - output/forecasts/midas/csv/midas_forecasts_full.csv\n",
  "  - output/forecasts/midas/csv/midas_forecasts_targets.csv\n"
)

for (var in names(plot_paths)) {
  message_lines <- c(message_lines, sprintf("  - output/forecasts/midas/plots/midas_forecast_%s_context.png\n", var))
}

message_lines <- c(
  message_lines,
  "  - output/forecasts/midas/models/midas_models.rds\n"
)

elapsed_time <- difftime(Sys.time(), start_time, units = "secs")
message_lines <- c(
  message_lines,
  sprintf("\nCompleted in %.1f seconds (%.1f minutes)", elapsed_time, elapsed_time / 60)
)

message(paste0(message_lines, collapse = ""))
