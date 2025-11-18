# Mixed-Frequency VAR with Combined Monthly Indicators
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
source(file.path("R", "latent_states.R"))

activate_project()
load_required_packages(required_pkgs)

start_time <- Sys.time()

variable <- step_ahead <- horizon <- lower <- median <- upper <- NULL

months_per_quarter <- 3L

add_quarters <- function(yq, n = 1L) {
  zoo::as.yearqtr(yq) + (as.integer(n) / months_per_quarter)
}

quarter_month_sequence <- function(yq, length_out = months_per_quarter) {
  start_date <- zoo::as.Date(zoo::as.yearqtr(yq), frac = 0)
  seq.Date(start_date, by = "1 month", length.out = length_out)
}

count_observed_months <- function(last_quarter, monthly_list) {
  if (!length(monthly_list)) {
    return(0L)
  }
  latest_yearmon <- max(vapply(monthly_list, function(ts_obj) {
    ts_end <- stats::end(ts_obj)
    zoo::as.yearmon(sprintf("%04d-%02d", ts_end[1], ts_end[2]))
  }, zoo::as.yearmon("1900-01")))

  next_quarter <- add_quarters(last_quarter, 1L)
  quarter_months <- zoo::as.yearmon(quarter_month_sequence(next_quarter))
  sum(quarter_months <= latest_yearmon)
}

compute_ragged_nowcast <- function(variable, months_observed, target_quarter, latent_states_long, monthly_forecast) {
  if (months_observed <= 0L) {
    return(NA_real_)
  }

  month_seq <- quarter_month_sequence(target_quarter)
  observed_dates <- month_seq[seq_len(months_observed)]

  observed_vals <- latent_states_long |>
    dplyr::filter(.data$variable == !!variable, .data$date %in% observed_dates) |>
    dplyr::arrange(.data$date) |>
    dplyr::pull(.data$value)

  if (length(observed_vals) != months_observed) {
    return(NA_real_)
  }

  remaining <- months_per_quarter - months_observed
  future_vals <- numeric(0)
  if (remaining > 0L) {
    future_dates <- month_seq[(months_observed + 1):months_per_quarter]
    future_vals <- monthly_forecast |>
      dplyr::filter(.data$variable == !!variable, .data$fcst_date %in% future_dates) |>
      dplyr::arrange(.data$fcst_date) |>
      dplyr::pull(.data$median)
    if (length(future_vals) != remaining) {
      return(NA_real_)
    }
  }

  mean(c(observed_vals, future_vals))
}

cli_args <- commandArgs(trailingOnly = TRUE)
early_strategy <- getOption("mfvar.early_monthly", Sys.getenv("MFVAR_EARLY_MONTHLY", "fill"))
arg_match <- cli_args[grepl("^--early-monthly=", cli_args)]
if (length(arg_match)) {
  early_candidate <- sub("^--early-monthly=", "", arg_match[1])
  if (nzchar(early_candidate)) {
    early_strategy <- early_candidate
  }
}
if (!early_strategy %in% c("fill", "omit")) {
  warning(sprintf("Unknown early-monthly strategy '%s'; defaulting to 'fill'.", early_strategy))
  early_strategy <- "fill"
}
message(sprintf("→ Early monthly data handling strategy: %s", early_strategy))

# Use strategy-dependent seed so MCMC draws differ between fill/omit
mfvar_seed <- if (identical(early_strategy, "fill")) 123L else 456L

if (identical(Sys.getenv("MFVAR_VERSION"), "manual")) {
  stop(
    paste(
      "Manual MF-VAR specification not yet implemented on main branch.",
      "See placeholder 'run_mfvar_manual.R' or the dedicated feature branch."
    )
  )
}

# --- I/O paths ---------------------------------------------------------------
DATA_DIR <- file.path(".", "data")
OUT_DIR  <- file.path(".", "output", "forecasts", "mfvar")
CSV_DIR  <- file.path(OUT_DIR, "csv")
PLOT_DIR <- file.path(OUT_DIR, "plots")
MODEL_DIR <- file.path(OUT_DIR, "models")
for (dir in c(OUT_DIR, CSV_DIR, PLOT_DIR, MODEL_DIR)) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

# --- Data preparation -------------------------------------------------------
# Pull the transformed quarterly series and align with monthly indicators sourced
# from the combined SNB dataset. We keep only periods where every chosen
# monthly series is available.
qdat_raw <- read_quarterly_data(DATA_DIR)
monthly_variables <- resolve_monthly_indicators()
monthly_data <- read_combined_timeseries(DATA_DIR, variables = monthly_variables)

trimmed <- trim_to_overlap(
  qdat_raw,
  monthly_data$ts_list,
  mode = "ragged",
  fill_method = "locf",
  start_strategy = early_strategy
)
stationary <- stationarise_quarterly(trimmed$qdat)
qdat_orig <- trimmed$qdat
qdat_adj <- stationary$data
transforms <- stationary$transforms
monthly_ts <- window_monthly_series(trimmed$monthly, qdat_orig, end_mode = "available")
last_obs_qtr <- tail(qdat_orig$qtr, 1)
ragged_months_observed <- count_observed_months(last_obs_qtr, monthly_ts)
Y <- build_Y(qdat_adj, monthly_ts)

target_vars <- target_variables
n_lags <- 5

# --- Estimation and forecasting --------------------------------------------
# Refit the MF-VAR on the full sample and produce 12 months of forecasts
# (which aggregate into 4 quarters = 1-year horizon for quarterly variables).
mod_ss <- estimate_mfvar_model(Y, n_lags, n_fcst = 12, seed = mfvar_seed)

monthly_forecast_full <- NULL
if (ragged_months_observed > 0L) {
  monthly_forecast_full <- tryCatch(
    predict(mod_ss, aggregate_fcst = FALSE, pred_bands = 0.8),
    error = function(err) {
      warning(sprintf("Monthly forecasts unavailable: %s", conditionMessage(err)), call. = FALSE)
      NULL
    }
  )
}

latent_states_path <- NULL
latent_states_plot <- NULL
latent_heatmap_plot <- NULL
latent_states <- NULL
need_latent_states <- ragged_months_observed > 0L || !identical(Sys.getenv("MFVAR_EXTRACT_STATES"), "0")
if (isTRUE(need_latent_states)) {
  latent_states <- tryCatch(
    extract_latent_states(mod_ss, summary = "mean"),
    error = function(err) {
      warning(sprintf("Latent state extraction failed: %s", conditionMessage(err)), call. = FALSE)
      NULL
    }
  )
}

if (!identical(Sys.getenv("MFVAR_EXTRACT_STATES"), "0") && !is.null(latent_states)) {
  latent_states_path <- save_latent_states_csv(latent_states, CSV_DIR)
  latent_states_plot <- plot_latent_states(latent_states, PLOT_DIR)
  latent_heatmap_plot <- plot_latent_states(latent_states, PLOT_DIR, mode = "heatmap")
}

fc <- predict(mod_ss, aggregate_fcst = TRUE, pred_bands = 0.8)
n_obs <- nrow(qdat_adj)
fc <- fc |>
  dplyr::group_by(variable) |>
  dplyr::mutate(
    step_ahead_tmp = dplyr::row_number(),
    time_index = dplyr::if_else(
      variable %in% names(transforms),
      as.integer(compute_time_index(n_obs, step_ahead_tmp)),
      NA_integer_
    ),
    lower = restore_series_values(lower, variable, time_index, transforms),
    median = restore_series_values(median, variable, time_index, transforms),
    upper = restore_series_values(upper, variable, time_index, transforms)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(-step_ahead_tmp, -time_index)

# Split forecasts into quarterly (limited to 4 steps) and monthly (all steps)
fc_q <- fc |>
  dplyr::filter(variable %in% target_vars) |>
  dplyr::arrange(variable, time) |>
  dplyr::group_by(variable) |>
  dplyr::mutate(step_ahead = dplyr::row_number()) |>
  dplyr::ungroup() |>
  # Keep only first 4 quarters to match MIDAS (1 year ahead)
  dplyr::filter(step_ahead <= 4) |>
  dplyr::mutate(
    horizon = dplyr::case_when(
      step_ahead == 1 ~ "1-step ahead",
      step_ahead == 4 ~ "1-year ahead",
      TRUE ~ NA_character_
    ),
    # Convert exchange-rate forecasts back to levels for reporting only.
    median = dplyr::if_else(variable == "exch_rate", exp(median), median),
    # Calculate quarter_end from last observation quarter + step_ahead quarters
    forecast_qtr = last_obs_qtr + (step_ahead / 4),
    quarter_end = zoo::as.Date(forecast_qtr, frac = 1)
  ) |>
  dplyr::select(-forecast_qtr, -lower, -upper)

if (ragged_months_observed > 0L && !is.null(latent_states) && !is.null(monthly_forecast_full)) {
  latent_states_long <- latent_states |>
    tidyr::pivot_longer(-date, names_to = "variable", values_to = "value") |>
    dplyr::filter(.data$variable %in% target_vars, !is.na(.data$value))

  monthly_fcst_targets <- monthly_forecast_full |>
    dplyr::filter(.data$variable %in% target_vars)

  if (nrow(latent_states_long) && nrow(monthly_fcst_targets)) {
    target_quarter <- add_quarters(last_obs_qtr, 1L)
    ragged_vals <- vapply(target_vars, function(var) {
      compute_ragged_nowcast(
        variable = var,
        months_observed = ragged_months_observed,
        target_quarter = target_quarter,
        latent_states_long = latent_states_long,
        monthly_forecast = monthly_fcst_targets
      )
    }, numeric(1))

    time_index <- compute_time_index(n_obs, 1L)
    ragged_restored <- restore_series_values(ragged_vals, target_vars, rep(time_index, length(target_vars)), transforms)
    ragged_tbl <- tibble::tibble(variable = target_vars, ragged_nowcast = ragged_restored) |>
      dplyr::mutate(
        ragged_nowcast = dplyr::if_else(variable == "exch_rate", exp(ragged_nowcast), ragged_nowcast)
      )

    fc_q <- fc_q |>
      dplyr::left_join(ragged_tbl, by = "variable") |>
      dplyr::mutate(
        median = dplyr::if_else(step_ahead == 1L & !is.na(ragged_nowcast), ragged_nowcast, median)
      ) |>
      dplyr::select(-ragged_nowcast)
  }
}

fc_m <- fc |>
  dplyr::filter(!variable %in% target_vars) |>
  dplyr::select(-dplyr::any_of(c("lower", "upper")))

# Combine for full forecast file (quarterly limited to 4 steps, monthly unlimited)
fc_full <- dplyr::bind_rows(fc_q, fc_m)

fc_targets <- fc_q |>
  dplyr::filter(!is.na(horizon)) |>
  dplyr::select(variable, step_ahead, horizon, quarter_end, median)

# Confirm we produced both the 1-step and 1-year forecasts for every target.
expected_horizons <- tidyr::expand_grid(variable = target_vars, step_ahead = c(1L, 4L))
missing_targets <- expected_horizons |>
  dplyr::anti_join(fc_targets, by = c("variable", "step_ahead"))

if (nrow(missing_targets)) {
  missing_msg <- paste(missing_targets$variable, paste0("step ", missing_targets$step_ahead), collapse = ", ")
  warning(sprintf("Forecast table is missing required horizons: %s", missing_msg))
}

fc_targets <- fc_targets |>
  dplyr::select(variable, horizon, quarter_end, median)

readr::write_csv(fc_full,    file.path(CSV_DIR, "mfvar_forecasts_full.csv"))
readr::write_csv(fc_targets, file.path(CSV_DIR, "mfvar_forecasts_targets.csv"))

# --- Summaries --------------------------------------------------------------
summary_path <- file.path(OUT_DIR, "mfvar_summary.txt")
sink(summary_path)
cat("\n==== MF-VAR summary (Minnesota prior, IW covariance) ====\n\n")
print(summary(mod_ss))
cat("\n==== Note ====\n")
cat("For model evaluation and benchmarking, see run_benchmarks.R\n")
sink()

# --- Plots ------------------------------------------------------------------
# Visualise each target relative to the AR(2) benchmark when forecasts exist.
fc_gdp <- fc_q |>
  dplyr::filter(variable == "gdp_growth") |>
  dplyr::transmute(
    time = quarter_end,
    median = median
  )

fc_infl <- fc_q |>
  dplyr::filter(variable == "inflation") |>
  dplyr::transmute(
    time = quarter_end,
    median = median
  )

fc_exch <- fc_q |>
  dplyr::filter(variable == "exch_rate") |>
  dplyr::transmute(
    time = quarter_end,
    median = median
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
  last_qtr_end <- zoo::as.Date(tail(qdat_orig$qtr, 1), frac = 1)
  first_fc_date <- fc_gdp$time[1]

  if (first_fc_date == last_qtr_end) {
    future_steps <- max(nrow(fc_gdp) - 1, 0)
    future_preds <- if (future_steps) {
      preds_adj <- predict_ar2(qdat_adj$gdp_growth, future_steps, var_label = "gdp_growth", context = "forecast horizon")
      indices <- compute_time_index(nrow(qdat_adj), seq_len(future_steps))
      restore_series_values(preds_adj, rep("gdp_growth", future_steps), indices, transforms)
    } else {
      numeric(0)
    }
    ar2_vals <- c(tail(qdat_orig$gdp_growth, 1), future_preds)
  } else {
    future_steps <- nrow(fc_gdp)
    preds_adj <- predict_ar2(qdat_adj$gdp_growth, future_steps, var_label = "gdp_growth", context = "forecast horizon")
    indices <- compute_time_index(nrow(qdat_adj), seq_len(future_steps))
    ar2_vals <- restore_series_values(preds_adj, rep("gdp_growth", future_steps), indices, transforms)
  }

  ar2_gdp <- tibble::tibble(time = fc_gdp$time, ar2 = ar2_vals)
  gdp_context_path <- plot_gdp_forecasts_with_history(fc_gdp, ar2_gdp, qdat_orig, PLOT_DIR)
}

if (nrow(fc_infl)) {
  last_qtr_end <- zoo::as.Date(tail(qdat_orig$qtr, 1), frac = 1)
  first_fc_date <- fc_infl$time[1]

  if (first_fc_date == last_qtr_end) {
    future_steps <- max(nrow(fc_infl) - 1, 0)
    future_preds <- if (future_steps) {
      preds_adj <- predict_ar2(qdat_adj$inflation, future_steps, var_label = "inflation", context = "forecast horizon")
      indices <- compute_time_index(nrow(qdat_adj), seq_len(future_steps))
      restore_series_values(preds_adj, rep("inflation", future_steps), indices, transforms)
    } else {
      numeric(0)
    }
    ar2_vals <- c(tail(qdat_orig$inflation, 1), future_preds)
  } else {
    future_steps <- nrow(fc_infl)
    preds_adj <- predict_ar2(qdat_adj$inflation, future_steps, var_label = "inflation", context = "forecast horizon")
    indices <- compute_time_index(nrow(qdat_adj), seq_len(future_steps))
    ar2_vals <- restore_series_values(preds_adj, rep("inflation", future_steps), indices, transforms)
  }

  ar2_infl <- tibble::tibble(time = fc_infl$time, ar2 = ar2_vals)
  inflation_context_path <- plot_inflation_forecasts_with_history(fc_infl, ar2_infl, qdat_orig, PLOT_DIR)
}

if (nrow(fc_exch)) {
  last_qtr_end <- zoo::as.Date(tail(qdat_orig$qtr, 1), frac = 1)
  first_fc_date <- fc_exch$time[1]

  if (first_fc_date == last_qtr_end) {
    future_steps <- max(nrow(fc_exch) - 1, 0)
    future_preds <- if (future_steps) {
      preds_adj <- predict_ar2(qdat_adj$exch_rate, future_steps, var_label = "exch_rate", context = "forecast horizon")
      indices <- compute_time_index(nrow(qdat_adj), seq_len(future_steps))
      restore_series_values(preds_adj, rep("exch_rate", future_steps), indices, transforms)
    } else {
      numeric(0)
    }
    ar2_vals <- c(tail(qdat_orig$exch_rate, 1), future_preds)
  } else {
    future_steps <- nrow(fc_exch)
    preds_adj <- predict_ar2(qdat_adj$exch_rate, future_steps, var_label = "exch_rate", context = "forecast horizon")
    indices <- compute_time_index(nrow(qdat_adj), seq_len(future_steps))
    ar2_vals <- restore_series_values(preds_adj, rep("exch_rate", future_steps), indices, transforms)
  }

  ar2_exch <- tibble::tibble(time = fc_exch$time, ar2 = exp(ar2_vals))
  exch_context_path <- plot_exch_rate_forecasts_with_history(fc_exch, ar2_exch, qdat_orig, PLOT_DIR)
}

# --- Persist model ----------------------------------------------------------
saveRDS(mod_ss, file.path(MODEL_DIR, "mfvar_model_ss.rds"))

# --- Completion message -----------------------------------------------------
message_lines <- c(
  "Done. Wrote:\n",
  "  - output/forecasts/mfvar/mfvar_summary.txt\n",
  "  - output/forecasts/mfvar/csv/mfvar_forecasts_full.csv\n",
  "  - output/forecasts/mfvar/csv/mfvar_forecasts_targets.csv\n"
)

if (!is.null(gdp_context_path)) {
  message_lines <- c(message_lines, "  - output/forecasts/mfvar/plots/forecast_gdp_growth_context.png\n")
}
if (!is.null(inflation_context_path)) {
  message_lines <- c(message_lines, "  - output/forecasts/mfvar/plots/forecast_inflation_context.png\n")
}
if (!is.null(exch_context_path)) {
  message_lines <- c(message_lines, "  - output/forecasts/mfvar/plots/forecast_exchange_rate_context.png\n")
}

if (!is.null(latent_states_path)) {
  message_lines <- c(message_lines, "  - output/forecasts/mfvar/csv/mfvar_latent_states.csv\n")
}
if (!is.null(latent_states_plot)) {
  message_lines <- c(message_lines, "  - output/forecasts/mfvar/plots/mfvar_latent_states_timeseries.png\n")
}
if (!is.null(latent_heatmap_plot)) {
  message_lines <- c(message_lines, "  - output/forecasts/mfvar/plots/mfvar_latent_states_heatmap.png\n")
}

message_lines <- c(
  message_lines,
  "  - output/forecasts/mfvar/models/mfvar_model_ss.rds"
)

elapsed_time <- difftime(Sys.time(), start_time, units = "secs")
message_lines <- c(
  message_lines,
  sprintf("\n\nCompleted in %.1f seconds (%.1f minutes)", elapsed_time, elapsed_time / 60)
)

message(paste0(message_lines, collapse = ""))
