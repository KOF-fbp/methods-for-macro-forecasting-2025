#!/usr/bin/env Rscript

# Combined Model Forecast Plots
# ---------------------------------------------------------------
# Creates overlay plots showing MF-VAR, MIDAS (trend & simple),
# and AR(2) forecasts together with historical data for comparison.
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
source(file.path("R", "plotting.R"))

activate_project()
load_required_packages(required_pkgs)

start_time <- Sys.time()

# --- Configuration -----------------------------------------------------------
DATA_DIR <- file.path(".", "data")
MFVAR_DIR <- file.path(".", "output", "forecasts", "mfvar")
MIDAS_DIR <- file.path(".", "output", "forecasts", "midas")
OUT_DIR <- file.path(".", "output", "forecasts", "combined")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# --- Load forecasts ----------------------------------------------------------
message("Loading model forecasts...")

# Check if forecast files exist
mfvar_path <- file.path(MFVAR_DIR, "csv", "mfvar_forecasts_full.csv")
midas_path <- file.path(MIDAS_DIR, "csv", "midas_forecasts_full.csv")

if (!file.exists(mfvar_path)) {
  stop("MF-VAR forecasts not found. Run: Rscript run_mfvar_package.R")
}

if (!file.exists(midas_path)) {
  stop("MIDAS forecasts not found. Run: Rscript run_midas.R")
}

# Load full forecasts to get all quarters (not just targets)
mfvar_fc <- readr::read_csv(mfvar_path, show_col_types = FALSE) |>
  dplyr::filter(variable %in% c("gdp_growth", "inflation", "exch_rate"))
midas_fc <- readr::read_csv(midas_path, show_col_types = FALSE)

# --- Load historical data ----------------------------------------------------
qdat_raw <- read_quarterly_data(DATA_DIR)
monthly_variables <- c("plkopr", "devkum", "amarbma")
monthly_data <- read_combined_timeseries(DATA_DIR, variables = monthly_variables)

start_quarter <- zoo::as.yearqtr(monthly_data$start_date)
qdat_filtered <- qdat_raw |>
  dplyr::filter(.data$qtr >= start_quarter)

if (!nrow(qdat_filtered)) {
  stop("Quarterly dataset does not contain observations after the monthly data start date.")
}

trimmed <- trim_to_overlap(qdat_filtered, monthly_data$ts_list)
stationary <- stationarise_quarterly(trimmed$qdat)
qdat_orig <- trimmed$qdat
qdat_adj <- stationary$data
transforms <- stationary$transforms

# Calculate proper quarter_end for MF-VAR forecasts based on last observation
last_obs_qtr <- tail(qdat_orig$qtr, 1)
mfvar_fc <- mfvar_fc |>
  dplyr::arrange(variable, time) |>
  dplyr::group_by(variable) |>
  dplyr::mutate(
    step_ahead = dplyr::row_number(),
    forecast_qtr = last_obs_qtr + (step_ahead / 4),
    quarter_end = zoo::as.Date(forecast_qtr, frac = 1)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-forecast_qtr)

# Align MIDAS forecasts to the same quarter grid used by MF-VAR for plotting
midas_fc <- midas_fc |>
  dplyr::mutate(
    step_ahead = as.integer(.data$step_ahead),
    aligned_qtr = last_obs_qtr + (step_ahead / 4),
    aligned_quarter_end = zoo::as.Date(aligned_qtr, frac = 1)
  )

target_vars <- target_variables

# --- Generate AR(2) forecasts ------------------------------------------------
message("Generating AR(2) benchmark forecasts...")

# Get the maximum forecast horizon from MF-VAR
mfvar_quarterly <- mfvar_fc |>
  dplyr::filter(variable %in% target_vars)

max_horizon <- mfvar_quarterly |>
  dplyr::group_by(variable) |>
  dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
  dplyr::pull(n) |>
  max()

ar2_forecasts <- list()

for (var in target_vars) {
  var_fc <- mfvar_quarterly |>
    dplyr::filter(variable == var) |>
    dplyr::arrange(time)
  
  n_steps <- nrow(var_fc)
  
  # Generate AR(2) forecasts in adjusted space
  preds_adj <- predict_ar2(qdat_adj[[var]], n_steps, var_label = var, context = "combined plot")
  
  # Restore to original scale
  indices <- compute_time_index(nrow(qdat_adj), seq_len(n_steps))
  preds_orig <- restore_series_values(preds_adj, rep(var, n_steps), indices, transforms)
  
  ar2_tbl <- tibble::tibble(
    time = var_fc$quarter_end,
    ar2 = preds_orig
  )

  if (identical(var, "exch_rate")) {
    ar2_tbl <- ar2_tbl |>
      dplyr::mutate(ar2 = exp(ar2))
  }

  ar2_forecasts[[var]] <- ar2_tbl
}

# --- Create combined plots ---------------------------------------------------
message("Creating combined forecast plots...")

plot_paths <- list()

# GDP Growth
gdp_mfvar <- mfvar_fc |>
  dplyr::filter(variable == "gdp_growth") |>
  dplyr::transmute(
    time = quarter_end,
    lower = lower,
    median = median,
    upper = upper
  )

gdp_midas <- midas_fc |>
  dplyr::filter(variable == "gdp_growth") |>
  dplyr::transmute(
    time = aligned_quarter_end,
    midas_trend = midas_trend,
    midas_simple = midas_simple
  )

gdp_history <- tibble::tibble(
  time = zoo::as.Date(qdat_orig$qtr, frac = 1),
  value = qdat_orig$gdp_growth
)

if (nrow(gdp_mfvar) > 0 && nrow(gdp_midas) > 0) {
  plot_paths$gdp <- plot_combined_forecasts(
    mfvar_df = gdp_mfvar,
    midas_df = gdp_midas,
    ar_df = ar2_forecasts$gdp_growth,
    history_df = gdp_history,
    out_dir = OUT_DIR,
    title = "GDP growth: all model forecasts",
    y_label = "Annualised percentage",
    file_name = "combined_gdp_growth.png"
  )
}

# Inflation
infl_mfvar <- mfvar_fc |>
  dplyr::filter(variable == "inflation") |>
  dplyr::transmute(
    time = quarter_end,
    lower = lower,
    median = median,
    upper = upper
  )

infl_midas <- midas_fc |>
  dplyr::filter(variable == "inflation") |>
  dplyr::transmute(
    time = aligned_quarter_end,
    midas_trend = midas_trend,
    midas_simple = midas_simple
  )

infl_history <- tibble::tibble(
  time = zoo::as.Date(qdat_orig$qtr, frac = 1),
  value = qdat_orig$inflation
)

if (nrow(infl_mfvar) > 0 && nrow(infl_midas) > 0) {
  plot_paths$inflation <- plot_combined_forecasts(
    mfvar_df = infl_mfvar,
    midas_df = infl_midas,
    ar_df = ar2_forecasts$inflation,
    history_df = infl_history,
    out_dir = OUT_DIR,
    title = "Inflation: all model forecasts",
    y_label = "Annualised percentage",
    file_name = "combined_inflation.png"
  )
}

# Exchange Rate
exch_mfvar <- mfvar_fc |>
  dplyr::filter(variable == "exch_rate") |>
  dplyr::transmute(
    time = quarter_end,
    lower = lower,
    median = median,
    upper = upper
  )

exch_midas <- midas_fc |>
  dplyr::filter(variable == "exch_rate") |>
  dplyr::transmute(
    time = aligned_quarter_end,
    midas_trend = exp(midas_trend),
    midas_simple = exp(midas_simple)
  )

exch_history <- tibble::tibble(
  time = zoo::as.Date(qdat_orig$qtr, frac = 1),
  value = exp(qdat_orig$exch_rate)
)

if (nrow(exch_mfvar) > 0 && nrow(exch_midas) > 0) {
  plot_paths$exch_rate <- plot_combined_forecasts(
    mfvar_df = exch_mfvar,
    midas_df = exch_midas,
    ar_df = ar2_forecasts$exch_rate,
    history_df = exch_history,
    out_dir = OUT_DIR,
    title = "Exchange rate: all model forecasts",
    y_label = "CHF per EUR",
    file_name = "combined_exchange_rate.png"
  )
}

# --- Completion message ------------------------------------------------------
message_lines <- c(
  "\nCombined forecast plots complete. Wrote:\n"
)

for (var in names(plot_paths)) {
  file_name <- basename(plot_paths[[var]])
  message_lines <- c(message_lines, sprintf("  - output/forecasts/combined/%s\n", file_name))
}

elapsed_time <- difftime(Sys.time(), start_time, units = "secs")
message_lines <- c(
  message_lines,
  sprintf("\nCompleted in %.1f seconds (%.1f minutes)", elapsed_time, elapsed_time / 60)
)

message(paste0(message_lines, collapse = ""))
