# Mixed-Frequency VAR with KOF Barometer
# ---------------------------------------------------------------
# Orchestrates the MF-VAR workflow by sourcing helper modules
# housed under ./R/. The pipeline ingests data, estimates the
# mixed-frequency VAR, benchmarks against an AR(2), and produces
# forecasts, evaluation tables, and plots.
# ---------------------------------------------------------------

source(file.path("R", "setup.R"))
source(file.path("R", "data_processing.R"))
source(file.path("R", "evaluation.R"))
source(file.path("R", "plotting.R"))

activate_project()
load_required_packages(required_pkgs)

variable <- step_ahead <- horizon <- lower <- median <- upper <- NULL

# --- I/O paths ---------------------------------------------------------------
DATA_DIR <- file.path(".", "data")
OUT_DIR  <- file.path(".", "output")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# --- Data preparation -------------------------------------------------------
# Pull the transformed quarterly series and aligned barometer input.
qdat_raw <- read_quarterly_data(DATA_DIR)
baro_raw <- fetch_kof_barometer()
trimmed <- trim_to_overlap(qdat_raw, baro_raw)
qdat <- trimmed$qdat
baro_ts <- window_baro(trimmed$baro_ts, qdat)
Y <- build_Y(qdat, baro_ts)

target_vars <- target_variables
n_lags <- 5

# --- Evaluation suites ------------------------------------------------------
# Benchmark MF-VAR forecasts against AR(2) both on a holdout window and
# in rolling one-step-ahead cross-validation.
holdout_results <- run_holdout_evaluation(qdat, baro_ts, n_lags, target_vars, OUT_DIR)
cv_results <- run_cross_validation(qdat, baro_ts, n_lags, target_vars, OUT_DIR)

# --- Estimation and forecasting --------------------------------------------
# Refit the MF-VAR on the full sample and produce 12 quarter-ahead forecasts
# (sufficient to cover the 1-year horizon after aggregation).
mod_ss <- estimate_mfvar_model(Y, n_lags, n_fcst = 12, seed = 123)

fc <- predict(mod_ss, aggregate_fcst = TRUE, pred_bands = 0.8)
fc_q <- fc |>
  dplyr::filter(variable %in% target_vars) |>
  dplyr::arrange(variable, time) |>
  dplyr::group_by(variable) |>
  dplyr::mutate(step_ahead = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    horizon = dplyr::case_when(
      step_ahead == 1 ~ "1-step ahead",
      step_ahead == 4 ~ "1-year ahead",
      TRUE ~ NA_character_
    ),
    # Convert exchange-rate forecasts back to levels for reporting only.
    median = dplyr::if_else(variable == "exch_rate", exp(median), median),
    lower  = dplyr::if_else(variable == "exch_rate", exp(lower), lower),
    upper  = dplyr::if_else(variable == "exch_rate", exp(upper), upper),
    quarter_end = zoo::as.Date(zoo::as.yearqtr(fcst_date), frac = 1)
  )

fc_targets <- fc_q |>
  dplyr::filter(!is.na(horizon)) |>
  dplyr::select(variable, step_ahead, horizon, quarter_end, median, lower, upper)

# Confirm we produced both the 1-step and 1-year forecasts for every target.
expected_horizons <- tidyr::expand_grid(variable = target_vars, step_ahead = c(1L, 4L))
missing_targets <- expected_horizons |>
  dplyr::anti_join(fc_targets, by = c("variable", "step_ahead"))

if (nrow(missing_targets)) {
  missing_msg <- paste(missing_targets$variable, paste0("step ", missing_targets$step_ahead), collapse = ", ")
  warning(sprintf("Forecast table is missing required horizons: %s", missing_msg))
}

fc_targets <- fc_targets |>
  dplyr::select(variable, horizon, quarter_end, median, lower, upper)

readr::write_csv(fc,         file.path(OUT_DIR, "mfvar_forecasts_full.csv"))
readr::write_csv(fc_targets, file.path(OUT_DIR, "mfvar_forecasts_targets.csv"))

# --- Summaries --------------------------------------------------------------
summary_path <- file.path(OUT_DIR, "mfvar_summary.txt")
sink(summary_path)
cat("\n==== MF-VAR summary (Minnesota prior, IW covariance) ====\n\n")
print(summary(mod_ss))

cat("\n==== Forecast evaluation ====\n")
if (!is.null(holdout_results$table)) {
  cat(sprintf("\nHoldout horizon: %d quarter(s).\n\n", holdout_results$horizon))
  print(holdout_results$table, n = nrow(holdout_results$table))
} else {
  cat("\nSkipped (insufficient holdout sample after reserving lags).\n")
}

cat("\n==== Rolling 1-step cross-validation ====\n")
if (!is.null(cv_results$table)) {
  cat(sprintf("\nFolds: %d (last %d quarter(s)).\n\n", cv_results$folds, cv_results$horizon))
  print(cv_results$table, n = nrow(cv_results$table))
} else {
  cat("\nSkipped (not enough data or no valid folds).\n")
}
sink()

# --- Plots ------------------------------------------------------------------
# Visualise each target relative to the AR(2) benchmark when forecasts exist.
fc_gdp <- fc_q |>
  dplyr::filter(variable == "gdp_growth") |>
  dplyr::transmute(
    time = quarter_end,
    lower = lower,
    median = median,
    upper = upper
  )

fc_infl <- fc_q |>
  dplyr::filter(variable == "inflation") |>
  dplyr::transmute(
    time = quarter_end,
    lower = lower,
    median = median,
    upper = upper
  )

fc_exch <- fc_q |>
  dplyr::filter(variable == "exch_rate") |>
  dplyr::transmute(
    time = quarter_end,
    lower = lower,
    median = median,
    upper = upper
  )

gdp_plot_path <- NULL
gdp_context_path <- NULL
inflation_plot_path <- NULL
inflation_context_path <- NULL
exch_plot_path <- NULL
exch_context_path <- NULL

if (nrow(fc_gdp)) {
  # The first MF-VAR forecast may be a nowcast of the current quarter.
  # AR(2) can only forecast future periods, so we prepend the last observed
  # value if the first forecast date matches the last observation quarter.
  last_qtr_end <- zoo::as.Date(tail(qdat$qtr, 1), frac = 1)
  first_fc_date <- fc_gdp$time[1]
  
  if (first_fc_date == last_qtr_end) {
    # First forecast is a nowcast; AR(2) uses observed value, then forecasts n-1 ahead
    ar2_vals <- c(tail(qdat$gdp_growth, 1), predict_ar2(qdat$gdp_growth, nrow(fc_gdp) - 1, var_label = "gdp_growth", context = "forecast horizon"))
  } else {
    # All forecasts are genuine future periods
    ar2_vals <- predict_ar2(qdat$gdp_growth, nrow(fc_gdp), var_label = "gdp_growth", context = "forecast horizon")
  }
  
  ar2_gdp <- tibble::tibble(time = fc_gdp$time, ar2 = ar2_vals)
  gdp_plot_path <- plot_gdp_forecasts(fc_gdp, ar2_gdp, OUT_DIR)
  gdp_context_path <- plot_gdp_forecasts_with_history(fc_gdp, ar2_gdp, qdat, OUT_DIR)
}

if (nrow(fc_infl)) {
  last_qtr_end <- zoo::as.Date(tail(qdat$qtr, 1), frac = 1)
  first_fc_date <- fc_infl$time[1]
  
  if (first_fc_date == last_qtr_end) {
    ar2_vals <- c(tail(qdat$inflation, 1), predict_ar2(qdat$inflation, nrow(fc_infl) - 1, var_label = "inflation", context = "forecast horizon"))
  } else {
    ar2_vals <- predict_ar2(qdat$inflation, nrow(fc_infl), var_label = "inflation", context = "forecast horizon")
  }
  
  ar2_infl <- tibble::tibble(time = fc_infl$time, ar2 = ar2_vals)
  inflation_plot_path <- plot_inflation_forecasts(fc_infl, ar2_infl, OUT_DIR)
  inflation_context_path <- plot_inflation_forecasts_with_history(fc_infl, ar2_infl, qdat, OUT_DIR)
}

if (nrow(fc_exch)) {
  last_qtr_end <- zoo::as.Date(tail(qdat$qtr, 1), frac = 1)
  first_fc_date <- fc_exch$time[1]
  
  if (first_fc_date == last_qtr_end) {
    # For exchange rate: last observed is in log space, AR(2) forecasts in log, convert to level
    ar2_vals <- c(tail(qdat$exch_rate, 1), predict_ar2(qdat$exch_rate, nrow(fc_exch) - 1, var_label = "exch_rate", context = "forecast horizon"))
    ar2_exch <- tibble::tibble(time = fc_exch$time, ar2 = exp(ar2_vals))
  } else {
    ar2_vals <- predict_ar2(qdat$exch_rate, nrow(fc_exch), var_label = "exch_rate", context = "forecast horizon")
    ar2_exch <- tibble::tibble(time = fc_exch$time, ar2 = exp(ar2_vals))
  }
  
  exch_plot_path <- plot_exch_rate_forecasts(fc_exch, ar2_exch, OUT_DIR)
  exch_context_path <- plot_exch_rate_forecasts_with_history(fc_exch, ar2_exch, qdat, OUT_DIR)
}

# --- Persist model ----------------------------------------------------------
saveRDS(mod_ss, file.path(OUT_DIR, "mfvar_model_ss.rds"))

# --- Completion message -----------------------------------------------------
message_lines <- c(
  "Done. Wrote:\n",
  "  - output/mfvar_summary.txt\n",
  "  - output/mfvar_forecasts_full.csv\n",
  "  - output/mfvar_forecasts_targets.csv\n"
)

if (!is.null(holdout_results$path)) {
  message_lines <- c(message_lines, "  - output/forecast_evaluation.csv\n")
} else {
  message_lines <- c(message_lines, "  - forecast evaluation skipped (not enough holdout data)\n")
}

if (!is.null(cv_results$path)) {
  message_lines <- c(message_lines, "  - output/forecast_cross_validation.csv\n")
} else {
  message_lines <- c(message_lines, "  - cross-validation skipped or unavailable\n")
}

if (!is.null(cv_results$folds_path)) {
  message_lines <- c(message_lines, "  - output/forecast_cross_validation_folds.csv\n")
}

if (!is.null(gdp_plot_path)) {
  message_lines <- c(message_lines, "  - output/forecast_gdp_growth.png\n")
}
if (!is.null(gdp_context_path)) {
  message_lines <- c(message_lines, "  - output/forecast_gdp_growth_context.png\n")
}
if (!is.null(inflation_plot_path)) {
  message_lines <- c(message_lines, "  - output/forecast_inflation.png\n")
}
if (!is.null(inflation_context_path)) {
  message_lines <- c(message_lines, "  - output/forecast_inflation_context.png\n")
}
if (!is.null(exch_plot_path)) {
  message_lines <- c(message_lines, "  - output/forecast_exchange_rate.png\n")
}
if (!is.null(exch_context_path)) {
  message_lines <- c(message_lines, "  - output/forecast_exchange_rate_context.png\n")
}

message_lines <- c(
  message_lines,
  "  - output/mfvar_model_ss.rds"
)

message(paste0(message_lines, collapse = ""))
