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
qdat_raw <- read_quarterly_data(DATA_DIR)
baro_raw <- fetch_kof_barometer()
trimmed <- trim_to_overlap(qdat_raw, baro_raw)
qdat <- trimmed$qdat
baro_ts <- window_baro(trimmed$baro_ts, qdat)
Y <- build_Y(qdat, baro_ts)

target_vars <- target_variables
n_lags <- 5

# --- Evaluation suites ------------------------------------------------------
holdout_results <- run_holdout_evaluation(qdat, baro_ts, n_lags, target_vars, OUT_DIR)
cv_results <- run_cross_validation(qdat, baro_ts, n_lags, target_vars, OUT_DIR)

# --- Estimation and forecasting --------------------------------------------
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
    median = dplyr::if_else(variable == "exch_rate", exp(median), median),
    lower  = dplyr::if_else(variable == "exch_rate", exp(lower), lower),
    upper  = dplyr::if_else(variable == "exch_rate", exp(upper), upper),
    quarter_end = zoo::as.Date(zoo::as.yearqtr(fcst_date), frac = 1)
  )

fc_targets <- fc_q |>
  dplyr::filter(!is.na(horizon)) |>
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
fc_gdp <- fc_q |>
  dplyr::filter(variable == "gdp_growth") |>
  dplyr::transmute(
    time = quarter_end,
    lower = lower,
    median = median,
    upper = upper
  )

gdp_plot_path <- NULL
context_plot_path <- NULL
if (nrow(fc_gdp)) {
  ar2_vals <- predict_ar2(qdat$gdp_growth, nrow(fc_gdp), var_label = "gdp_growth", context = "forecast horizon")
  ar2_gdp <- tibble::tibble(time = fc_gdp$time, ar2 = ar2_vals)
  gdp_plot_path <- plot_gdp_forecasts(fc_gdp, ar2_gdp, OUT_DIR)
  context_plot_path <- plot_gdp_forecasts_with_history(fc_gdp, ar2_gdp, qdat, OUT_DIR)
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

if (!is.null(gdp_plot_path)) {
  message_lines <- c(message_lines, "  - output/forecast_gdp_growth.png\n")
}
if (!is.null(context_plot_path)) {
  message_lines <- c(message_lines, "  - output/forecast_gdp_growth_context.png\n")
}

message_lines <- c(
  message_lines,
  "  - output/mfvar_model_ss.rds"
)

message(paste0(message_lines, collapse = ""))
