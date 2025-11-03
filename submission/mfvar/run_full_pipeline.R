# ==============================================================================
# Complete MF-VAR Pipeline Using Raw SNB Data
# ==============================================================================
# This script runs the full Mixed-Frequency VAR workflow:
# 1. Parses RAW SNB CSV files directly (no pre-parsed data)
# 2. Loads quarterly KOF data
# 3. Merges and aligns all data sources
# 4. Estimates Bayesian MF-VAR with Minnesota prior
# 5. Generates multi-step ahead forecasts
# 6. Creates publication-quality visualizations
# ==============================================================================

library(data.table)
library(zoo)
library(lubridate)
library(KFAS)
library(mvtnorm)
library(ggplot2)

# Load mfvar package functions  
devtools::load_all(".")

# ==============================================================================
# SNB DATA PARSING FUNCTIONS
# ==============================================================================

#' Parse SNB semicolon format CSV
parse_snb_file <- function(filepath, dimension_filter = NULL) {
  raw <- fread(filepath, sep = ";", skip = 3, header = TRUE)
  if (ncol(raw) == 4) {
    setnames(raw, c("date", "d0", "d1", "value"))
    raw[, dimension := paste(d0, d1, sep = ";")]
    raw[, c("d0", "d1") := NULL]
  } else if (ncol(raw) == 3) {
    setnames(raw, c("date", "dimension", "value"))
  }
  raw[, date := as.Date(paste0(date, "-01"))]
  raw[, date := ceiling_date(date, "month") - days(1)]
  raw <- raw[value != ""]
  raw[, value := as.numeric(value)]
  if (!is.null(dimension_filter)) raw <- raw[dimension %in% dimension_filter]
  raw <- raw[!is.na(value)]
  return(raw)
}

pivot_snb_data <- function(dt) {
  wide <- dcast(dt, date ~ dimension, value.var = "value")
  setorder(wide, date)
  return(wide)
}

# Parse monthly CPI - LD2010100 from https://data.snb.ch/en/topics/uvo/cube/plkopr
parse_monthly_cpi <- function(base_path) {
  filepath <- file.path(base_path, "CPI-snb-data-plkopr-en-all-20251021_0900.csv")
  raw <- parse_snb_file(filepath, dimension_filter = c("LD2010100"))
  cpi <- pivot_snb_data(raw)
  if ("LD2010100" %in% names(cpi)) setnames(cpi, "LD2010100", "cpi_index")
  return(cpi)
}

# Parse monthly forex - M0;EUR1, M0;USD1 from https://data.snb.ch/en/topics/ziredev/cube/devkum
parse_monthly_forex <- function(base_path) {
  filepath <- file.path(base_path, "Forex_snb-data-devkum-en-all-20251001-1430.csv")
  raw <- parse_snb_file(filepath)
  raw <- raw[dimension %in% c("M0;EUR1", "M0;USD1")]
  raw[dimension == "M0;EUR1", dimension := "eur_chf"]
  raw[dimension == "M0;USD1", dimension := "usd_chf"]
  forex <- pivot_snb_data(raw)
  return(forex)
}

# Parse monthly labour - E from https://data.snb.ch/en/topics/uvo/cube/amarbma
parse_monthly_labour <- function(base_path) {
  filepath <- file.path(base_path, "Labour_market_snb-data-amarbma-en-all-20251021-1035.csv")
  raw <- parse_snb_file(filepath)
  raw <- raw[dimension == "E"]
  labour <- pivot_snb_data(raw)
  if ("E" %in% names(labour)) {
    labour[, unemployment_total := E / 1000000]  # Scale to millions
    labour[, E := NULL]
  }
  return(labour)
}

# Parse quarterly KOF data
parse_quarterly_data <- function(base_path) {
  filepath <- file.path(base_path, "data_quarterly.csv")
  qdata <- fread(filepath)
  qdata[, date := as.Date(paste0(date, "-01"))]
  qdata[, date := ceiling_date(date, "month") - days(1)]
  return(qdata[, .(date, gdp)])
}

# ==============================================================================
# STEP 1: PARSE RAW SNB DATA
# ==============================================================================

cat("\n=== STEP 1: PARSING RAW SNB CSV FILES ===\n\n")

data_path <- "../data"

cpi_data <- parse_monthly_cpi(data_path)
cat(sprintf("✓ CPI: %d obs from %s to %s\n", 
            nrow(cpi_data), min(cpi_data$date), max(cpi_data$date)))

forex_data <- parse_monthly_forex(data_path)
cat(sprintf("✓ Forex: %d obs from %s to %s\n", 
            nrow(forex_data), min(forex_data$date), max(forex_data$date)))

labour_data <- parse_monthly_labour(data_path)
cat(sprintf("✓ Labour: %d obs from %s to %s\n", 
            nrow(labour_data), min(labour_data$date), max(labour_data$date)))

quarterly_data <- parse_quarterly_data(data_path)
cat(sprintf("✓ Quarterly GDP: %d obs from %s to %s\n", 
            nrow(quarterly_data), min(quarterly_data$date), max(quarterly_data$date)))

# Merge all data sources
monthly_data <- copy(cpi_data)
monthly_data <- merge(monthly_data, forex_data, by = "date", all = TRUE)
monthly_data <- merge(monthly_data, labour_data, by = "date", all = TRUE)
monthly_data <- merge(monthly_data, quarterly_data, by = "date", all.x = TRUE)

# Find maximum common period (restrict to 2002+ for numerical stability)
monthly_complete <- monthly_data[complete.cases(monthly_data[, .(cpi_index, eur_chf, usd_chf, unemployment_total)])]
monthly_complete <- monthly_complete[date >= as.Date("2002-01-01")]

cat(sprintf("\n✓ Merged data: %d observations from %s to %s\n", 
            nrow(monthly_complete), min(monthly_complete$date), max(monthly_complete$date)))
cat(sprintf("  Quarters with GDP: %d\n\n", sum(!is.na(monthly_complete$gdp))))

data_all <- monthly_complete

# ==============================================================================
# STEP 2: LOAD AND MERGE DATA
# ==============================================================================

cat("\n=== STEP 2: DATA LOADING ===\n\n")

# Data is already loaded from Step 1 (data_all)
# No additional loading needed

cat(sprintf("Data loaded: %d observations\n", nrow(data_all)))
cat(sprintf("  Date range: %s to %s\n", min(data_all$date), max(data_all$date)))
cat(sprintf("  Quarters with GDP: %d\n\n", sum(!is.na(data_all$gdp))))# ==============================================================================
# STEP 3: VARIABLE SELECTION AND FILTERING
# ==============================================================================

cat("\n=== STEP 3: VARIABLE SELECTION ===\n\n")

# Use all available data - no filtering by year
# The data already spans the maximum common period (2002-2025)

# Variables are already selected in the clean dataset:
# Monthly: cpi_index, eur_chf, usd_chf, unemployment_total
# Quarterly: gdp

monthly_vars <- c("cpi_index", "eur_chf", "usd_chf", "unemployment_total")
quarterly_vars <- c("gdp")

cat("Monthly variables:\n")
for (v in monthly_vars) {
  non_na <- sum(!is.na(data_all[[v]]))
  cat(sprintf("  - %s: %d observations\n", v, non_na))
}

cat("\nQuarterly variables:\n")
for (v in quarterly_vars) {
  non_na <- sum(!is.na(data_all[[v]]))
  cat(sprintf("  - %s: %d observations\n", v, non_na))
}
cat("\n")

# ==============================================================================
# STEP 4: DATA TRANSFORMATION (LOG LEVELS)
# ==============================================================================

cat("\n=== STEP 4: VARIABLE TRANSFORMATION ===\n\n")

# The data is already clean and aligned
# Add quarter-end indicator
data_all[, is_quarter_end := month(date) %in% c(3, 6, 9, 12)]

# Transform to log levels for growth rate modeling
data_all[, log_cpi := log(cpi_index)]
data_all[, log_eur := log(eur_chf)]
data_all[, log_usd := log(usd_chf)]
data_all[, log_unemp := log(unemployment_total)]
data_all[, log_gdp := log(gdp)]

# Variable names for model
var_names <- c("log_cpi", "log_eur", "log_usd", "log_unemp", "log_gdp")
freq_types <- c("monthly", "monthly", "monthly", "monthly", "quarterly")
names(freq_types) <- var_names

cat("Transformed variables:\n")
for (v in var_names) {
  cat(sprintf("  - %s (%s)\n", v, freq_types[v]))
}
cat("\n")

# ==============================================================================
# STEP 5: STANDARDIZATION FOR NUMERICAL STABILITY
# ==============================================================================

cat("\n=== STEP 5: STANDARDIZATION ===\n\n")

# Keep only complete cases for monthly variables
# (GDP will be NA except at quarter ends - that's expected)
monthly_complete_idx <- complete.cases(data_all[, .(log_cpi, log_eur, log_usd, log_unemp)])
data_complete <- data_all[monthly_complete_idx]

cat(sprintf("Rows with complete monthly data: %d\n", nrow(data_complete)))
cat(sprintf("GDP observations: %d\n\n", sum(!is.na(data_complete$log_gdp))))

# Compute means and standard deviations (including GDP despite NAs)
means <- sapply(var_names, function(v) mean(data_complete[[v]], na.rm = TRUE))
sds <- sapply(var_names, function(v) sd(data_complete[[v]], na.rm = TRUE))

# Standardize
for (v in var_names) {
  data_complete[, (paste0(v, "_std")) := (get(v) - means[v]) / sds[v]]
}

# Create standardized variable names
var_names_std <- paste0(var_names, "_std")

cat("Standardization parameters:\n")
for (i in seq_along(var_names)) {
  cat(sprintf("  %s: mean=%.3f, sd=%.3f\n", var_names[i], means[i], sds[i]))
}
cat("\n")

# ==============================================================================
# STEP 6: PREPARE FOR MF-VAR
# ==============================================================================

cat("\n=== STEP 6: MF-VAR DATA PREPARATION ===\n\n")

# Extract the data matrix (standardized)
Y <- as.matrix(data_complete[, ..var_names_std])

# Create frequency indicator vector
# 1 = monthly observed, 0 = quarterly (mostly missing)
freq_indicator <- rep(1, length(var_names))
freq_indicator[freq_types == "quarterly"] <- 0

# For quarterly variables, GDP is already NA except at quarter ends
# The data is already in the right format

# Count observations per variable
obs_counts <- colSums(!is.na(Y))

cat("Final dataset for MF-VAR:\n")
cat(sprintf("  Total months: %d\n", nrow(Y)))
cat(sprintf("  Variables: %d\n\n", ncol(Y)))

cat("Observations per variable:\n")
for (i in seq_along(var_names)) {
  cat(sprintf("  %s: %d obs (%s)\n", 
              var_names[i], obs_counts[i], 
              ifelse(freq_indicator[i] == 1, "monthly", "quarterly")))
}
cat("\n")

# ==============================================================================
# STEP 8: PREPARE CALENDAR AND METADATA
# ==============================================================================

cat("\n=== STEP 8: CALENDAR AND METADATA PREPARATION ===\n\n")

# Create calendar object for the package API
calendar <- list(
  dates = data_complete$date,
  is_quarter_end = data_complete$is_quarter_end
)

# Create metadata for each variable
meta <- list(
  freq = setNames(freq_types, var_names),
  release_delay = setNames(rep(0, length(var_names)), var_names)
)

cat(sprintf("Calendar: %d dates from %s to %s\n", 
            length(calendar$dates), 
            min(calendar$dates), 
            max(calendar$dates)))
cat(sprintf("Metadata: %d variables\n\n", length(meta$freq)))

# ==============================================================================
# STEP 9: BAYESIAN MF-VAR ESTIMATION
# ==============================================================================

cat("\n=== STEP 9: BAYESIAN MF-VAR ESTIMATION ===\n\n")

# Model specification
n_vars <- ncol(Y)
p_lags <- 2  # VAR(2) model
lambda <- 0.5  # Minnesota prior tightness

cat(sprintf("Model: VAR(%d) with %d variables\n", p_lags, n_vars))
cat(sprintf("Minnesota prior: lambda = %.2f\n\n", lambda))

# Hyperparameters for Minnesota prior
hyper <- list(
  lambda = lambda,    # Overall tightness
  nu = 1.0,          # Lag decay
  theta = 1.0        # Cross-variable shrinkage
)

cat("Prior specification:\n")
cat(sprintf("  Lambda (overall tightness): %.2f\n", hyper$lambda))
cat(sprintf("  Nu (lag decay): %.2f\n", hyper$nu))
cat(sprintf("  Theta (cross-variable shrinkage): %.2f\n\n", hyper$theta))

# Gibbs sampler settings
n_draws <- 500
burnin <- 250

cat(sprintf("Gibbs sampler: %d total draws (%d burnin)\n\n", 
            n_draws, burnin))

# Run Gibbs sampler using package API
cat("Running Gibbs sampler...\n")
colnames(Y) <- var_names  # Ensure column names are set

posterior <- gibbs_mfvar(
  y_obs = Y,
  calendar = calendar,
  meta = meta,
  p = p_lags,
  hyper = hyper,
  n_draws = n_draws,
  burnin = burnin,
  thinning = 1,
  seed = 42
)

cat("\n✓ Gibbs sampler completed\n")
cat(sprintf("  Posterior draws retained: %d\n\n", nrow(posterior$A)))

# ==============================================================================
# STEP 10: POSTERIOR SUMMARY
# ==============================================================================

cat("\n=== STEP 10: POSTERIOR SUMMARY ===\n\n")

# Compute posterior means
A_mean <- colMeans(posterior$A)
# Sigma_draws is 3D: (n x n x n_draws) or (draw x n x n)?
Sigma_dims <- dim(posterior$Sigma_draws)
if (length(Sigma_dims) == 3) {
  # If it's (draw, n, n) or (n, n, draw)
  if (Sigma_dims[1] == nrow(posterior$A)) {
    # Format: (n_draws, n, n)
    Sigma_mean <- apply(posterior$Sigma_draws, 2:3, mean)
  } else {
    # Format: (n, n, n_draws)
    Sigma_mean <- apply(posterior$Sigma_draws, 1:2, mean)
  }
} else {
  Sigma_mean <- posterior$Sigma
}

# Reshape A_mean to matrix form
A_matrix <- matrix(A_mean, nrow = n_vars * p_lags + 1, ncol = n_vars)
rownames(A_matrix) <- c(paste0("Lag", rep(1:p_lags, each = n_vars)), "Intercept")
colnames(A_matrix) <- var_names

cat("Posterior mean A matrix (first few rows):\n")
print(head(A_matrix, 10))
cat("\n")

# Name Sigma matrix
if (ncol(Sigma_mean) == n_vars && nrow(Sigma_mean) == n_vars) {
  rownames(Sigma_mean) <- var_names
  colnames(Sigma_mean) <- var_names
  cat("Posterior mean Sigma (covariance matrix):\n")
  print(round(Sigma_mean, 4))
} else {
  cat("Posterior mean Sigma dimensions:", dim(Sigma_mean), "\n")
  cat("First few values:\n")
  print(head(Sigma_mean))
}
cat("\n")

# ==============================================================================
# STEP 11: FORECASTING
# ==============================================================================

cat("\n=== STEP 11: FORECASTING ===\n\n")

h_max <- 24  # 24 months ahead

cat(sprintf("Forecast horizon: %d months\n\n", h_max))

# Generate forecasts using the package API
forecasts_result <- predict_mfvar(
  posterior = posterior,
  horizons_months = h_max,
  nsim = 500,
  seed = 42
)

cat("✓ Forecasts generated\n\n")

# Extract forecast matrices
forecast_mean <- forecasts_result$y_mean
forecast_q05 <- forecasts_result$y_lower
forecast_q95 <- forecasts_result$y_upper

# Convert back to original scale
forecast_mean_original <- t(t(forecast_mean) * sds[var_names] + means[var_names])
forecast_q05_original <- t(t(forecast_q05) * sds[var_names] + means[var_names])
forecast_q95_original <- t(t(forecast_q95) * sds[var_names] + means[var_names])

colnames(forecast_mean_original) <- var_names
colnames(forecast_q05_original) <- var_names
colnames(forecast_q95_original) <- var_names

cat("Forecast summary at key horizons (original scale, log levels):\n")
horizons_to_show <- c(1, 3, 6, 12, 24)
horizons_to_show <- horizons_to_show[horizons_to_show <= h_max]
cat(sprintf("Horizon: %s\n", paste(horizons_to_show, collapse = ", ")))
for (v in var_names) {
  cat(sprintf("  %s: %s\n", v, 
              paste(sprintf("%.3f", forecast_mean_original[horizons_to_show, v]), 
                    collapse = ", ")))
}
cat("\n")

# ==============================================================================
# STEP 12: VISUALIZATION
# ==============================================================================

cat("\n=== STEP 12: CREATING VISUALIZATIONS ===\n\n")

# Create output directory
output_dir <- "../output/mfvar_full_pipeline"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Get historical data in original scale
historical_data <- data_complete[, ..var_names]

# Create forecast dates
last_date <- max(data_complete$date, na.rm = TRUE)
forecast_dates <- seq.Date(from = last_date %m+% months(1), by = "month", length.out = h_max)

# Plot each variable
for (v in var_names) {
  
  # Prepare data for plotting
  plot_data <- data.frame(
    date = c(data_complete$date, forecast_dates),
    value = c(historical_data[[v]], forecast_mean_original[, v]),
    lower = c(rep(NA, nrow(data_complete)), forecast_q05_original[, v]),
    upper = c(rep(NA, nrow(data_complete)), forecast_q95_original[, v]),
    type = c(rep("Historical", nrow(data_complete)), rep("Forecast", h_max))
  )
  
  # Only show last 60 months of historical data for clarity
  n_hist_show <- 60
  plot_data_subset <- plot_data[
    (nrow(data_complete) - n_hist_show + 1):nrow(plot_data), 
  ]
  
  # Create plot
  p <- ggplot(plot_data_subset, aes(x = date)) +
    geom_line(aes(y = value, color = type), linewidth = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
    geom_vline(xintercept = last_date, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = c("Historical" = "black", "Forecast" = "blue")) +
    labs(
      title = paste("MF-VAR Forecast:", v),
      subtitle = sprintf("%d-month ahead forecast with 90%% credible interval", h_max),
      x = "Date",
      y = "Log Level",
      color = NULL
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10)
    )
  
  # Save plot
  filename <- file.path(output_dir, paste0("forecast_", v, ".png"))
  ggsave(filename, p, width = 10, height = 6, dpi = 300)
  
  cat(sprintf("✓ Saved forecast plot for %s\n", v))
}

cat("\n")

# ==============================================================================
# STEP 13: SAVE RESULTS
# ==============================================================================

cat("\n=== STEP 13: SAVING RESULTS ===\n\n")

# Save posterior
saveRDS(posterior, file.path(output_dir, "posterior.rds"))
cat("✓ Saved posterior draws\n")

# Save forecasts
saveRDS(forecasts_result, file.path(output_dir, "forecasts.rds"))
cat("✓ Saved forecast results\n")

# Save forecast summaries as CSV
forecast_summary <- data.frame(
  horizon = 1:h_max,
  date = forecast_dates,
  forecast_mean_original
)
fwrite(forecast_summary, file.path(output_dir, "forecast_summary.csv"))
cat("✓ Saved forecast summary CSV\n")

# Save posterior means
A_mean_dt <- as.data.table(A_matrix, keep.rownames = TRUE)
setnames(A_mean_dt, "rn", "Parameter")
fwrite(A_mean_dt, file.path(output_dir, "A_posterior_mean.csv"))

# Sigma is already named above
Sigma_mean_dt <- as.data.table(Sigma_mean, keep.rownames = TRUE)
setnames(Sigma_mean_dt, "rn", "Variable")
fwrite(Sigma_mean_dt, file.path(output_dir, "Sigma_posterior_mean.csv"))

cat("✓ Saved posterior parameter estimates\n")

# Save processed data
fwrite(data_complete, file.path(output_dir, "processed_data.csv"))
cat("✓ Saved processed data\n")

# Save transformation parameters
transformation_info <- data.frame(
  variable = var_names,
  frequency = freq_types,
  mean = means[var_names],
  sd = sds[var_names]
)
fwrite(transformation_info, file.path(output_dir, "transformation_info.csv"))
cat("✓ Saved transformation parameters\n")

cat("\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n" %+% strrep("=", 80) %+% "\n")
cat("MIXED FREQUENCY VAR PIPELINE COMPLETED SUCCESSFULLY!\n")
cat(strrep("=", 80) %+% "\n\n")

cat("Summary:\n")
cat(sprintf("  Data period: %s to %s\n", min(data_complete$date), max(data_complete$date)))
cat(sprintf("  Total observations: %d months\n", nrow(data_complete)))
cat(sprintf("  Variables: %d (%d monthly, %d quarterly)\n", 
            n_vars, sum(freq_indicator == 1), sum(freq_indicator == 0)))
cat(sprintf("  Model: VAR(%d)\n", p_lags))
cat(sprintf("  Posterior draws: %d\n", nrow(posterior$A)))
cat(sprintf("  Forecast horizon: %d months\n", h_max))
cat(sprintf("\nOutput directory: %s\n", output_dir))
cat(sprintf("  - %d forecast plots (PNG)\n", length(var_names)))
cat("  - Posterior draws (RDS)\n")
cat("  - Forecast results (RDS)\n")
cat("  - Summary tables (CSV)\n")
cat("\n")

cat("Variables included:\n")
for (i in seq_along(var_names)) {
  cat(sprintf("  %d. %s (%s, %d obs)\n", 
              i, var_names[i], freq_types[i], obs_counts[i]))
}

cat("\n" %+% strrep("=", 80) %+% "\n\n")
