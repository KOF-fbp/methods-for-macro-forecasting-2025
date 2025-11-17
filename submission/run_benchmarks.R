#!/usr/bin/env Rscript

if (!interactive()) {
  args_all <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args_all, value = TRUE)
  if (length(file_arg)) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE)
    setwd(dirname(script_path))
  }
}

cli_args <- commandArgs(trailingOnly = TRUE)
fast_mode <- any(cli_args %in% c("--fast", "-f")) || identical(Sys.getenv("MFVAR_FAST"), "1")
skip_cv <- "--no-cv" %in% cli_args
if ("--with-cv" %in% cli_args) skip_cv <- FALSE
max_folds_override <- NA_integer_
max_folds_arg <- cli_args[grepl("^--max-folds=", cli_args)]
if (length(max_folds_arg)) {
  max_folds_override <- suppressWarnings(as.integer(sub("^--max-folds=", "", max_folds_arg[1])))
}
early_strategy <- getOption("mfvar.early_monthly", Sys.getenv("MFVAR_EARLY_MONTHLY", "fill"))
early_arg <- cli_args[grepl("^--early-monthly=", cli_args)]
if (length(early_arg)) {
  early_candidate <- sub("^--early-monthly=", "", early_arg[1])
  if (nzchar(early_candidate)) {
    early_strategy <- early_candidate
  }
}
if (!early_strategy %in% c("fill", "omit")) {
  warning(sprintf("Unknown early-monthly strategy '%s'; defaulting to 'fill'.", early_strategy))
  early_strategy <- "fill"
}

# Use strategy-dependent seed so MCMC draws differ between fill/omit
mfvar_seed <- if (identical(early_strategy, "fill")) 123L else 456L
if (fast_mode && !("--with-cv" %in% cli_args)) {
  skip_cv <- TRUE
}
if (fast_mode) {
  message("→ Fast mode enabled: using single-threaded execution and trimmed CV settings.")
}
if (skip_cv) {
  message("→ Cross-validation will be skipped (use --with-cv to re-enable).")
}
message(sprintf("→ Early monthly data handling strategy: %s", early_strategy))

source(file.path("R", "setup.R"))
source(file.path("R", "data_processing.R"))
source(file.path("R", "evaluation.R"))
source(file.path("R", "latent_states.R"))
source(file.path("R", "benchmark_shared.R"))
source(file.path("R", "benchmark_cv.R"))

stage_status <- local({
  stage_env <- new.env(parent = emptyenv())
  stage_env$idx <- 0L
  stage_env$current <- NULL
  function(name = NULL, status = c("start", "done", "skip", "error")) {
    status <- match.arg(status)
    if (identical(status, "start")) {
      if (is.null(name)) stop("Stage name required when status = 'start'")
      stage_env$idx <- stage_env$idx + 1L
      stage_env$current <- name
      message(sprintf("\n[Stage %d] %s ...", stage_env$idx, name))
    } else {
      stage_name <- if (is.null(name)) stage_env$current else name
      if (is.null(stage_name)) stage_name <- "<unnamed stage>"
      label <- switch(status,
        done = "completed",
        skip = "skipped",
        error = "failed",
        status
      )
      message(sprintf("[Stage %d] %s %s.", stage_env$idx, stage_name, label))
      if (status %in% c("done", "skip", "error")) {
        stage_env$current <- NULL
      }
    }
  }
})

stage_status("Project setup", "start")
activate_project()

all_pkgs <- unique(c(required_pkgs, "midasr", "forecast", "purrr", "future", "future.apply"))
load_required_packages(all_pkgs)
stage_status(status = "done")

DATA_DIR <- file.path(".", "data")
OUT_DIR <- file.path(".", "output", "benchmarks")
OUT_CSV_DIR <- file.path(OUT_DIR, "csv")
OUT_PLOTS_DIR <- file.path(OUT_DIR, "plots")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
if (!dir.exists(OUT_CSV_DIR)) dir.create(OUT_CSV_DIR, recursive = TRUE)
if (!dir.exists(OUT_PLOTS_DIR)) dir.create(OUT_PLOTS_DIR, recursive = TRUE)

forecast_steps <- c(1L, 4L)
history_quarters <- 4L
n_lags <- 5

quarter_start_month <- function(q) {
  q_date <- zoo::as.Date(q, frac = 0)
  c(lubridate::year(q_date), lubridate::month(q_date))
}

convert_for_plot <- function(value, var) {
  if (identical(var, "exch_rate")) exp(value) else value
}

label_for_var <- function(var) {
  switch(
    var,
    "gdp_growth" = "Annualised percentage",
    "inflation" = "Annualised percentage",
    "exch_rate" = "CHF per EUR",
    "Value"
  )
}

qdat_raw <- read_quarterly_data(DATA_DIR)

# Load KOF Barometer for MIDAS-KOF models
baro_raw <- fetch_kof_barometer()

# Load combined timeseries for MF-VAR (4 SNB monthly indicators)
monthly_raw <- read_combined_timeseries(
  DATA_DIR,
  variables = c("plkopr", "devkum", "amarbma", "snboffzisa")
)

# Trim quarterly data to overlap with monthly indicators
trimmed <- trim_to_overlap(
  qdat_raw,
  monthly_raw$ts_list,
  mode = "ragged",
  fill_method = "locf",
  start_strategy = early_strategy
)
qdat_orig <- trimmed$qdat
monthly_series_list <- window_monthly_series(trimmed$monthly, qdat_orig, end_mode = "available")

# Also prepare KOF Barometer for MIDAS-KOF models
baro_ts <- window_baro(baro_raw, qdat_orig, end_mode = "available")

stationary <- stationarise_quarterly(qdat_orig)
qdat_adj <- stationary$data
transforms <- stationary$transforms

baro_diff_series <- local({
  first_quarter_date <- zoo::as.Date(qdat_orig$qtr[1], frac = 0)
  prev_year <- lubridate::year(first_quarter_date)
  prev_month <- (lubridate::quarter(first_quarter_date) - 1L) * months_per_quarter
  if (prev_month == 0L) {
    prev_month <- 12L
    prev_year <- prev_year - 1L
  }

  diff_source <- stats::window(baro_ts, start = c(prev_year, prev_month))
  diff_series <- base::diff(diff_source)
  first_month_of_quarter <- (lubridate::quarter(first_quarter_date) - 1L) * months_per_quarter + 1L
  stats::window(
    diff_series,
    start = c(lubridate::year(first_quarter_date), first_month_of_quarter)
  )
})

stage_status("Data preparation", "start")

target_vars <- target_variables
y_ts_list <- build_q_ts(qdat_orig)

max_holdout <- nrow(qdat_adj) - (n_lags + 1)
if (max_holdout < 0) max_holdout <- 0
eval_horizon <- min(max(forecast_steps), max_holdout)

if (eval_horizon < max(forecast_steps)) {
  stop("Need at least ", max(forecast_steps), " holdout quarters to evaluate 1-year-ahead errors. Reduce lag length or extend the sample.")
}

train_rows <- nrow(qdat_adj) - eval_horizon
q_train_adj <- qdat_adj |> dplyr::slice_head(n = train_rows)
q_train_orig <- qdat_orig |> dplyr::slice_head(n = train_rows)
q_test_orig <- qdat_orig |> dplyr::slice_tail(n = eval_horizon)
forecast_quarters <- q_test_orig$qtr
forecast_dates <- zoo::as.Date(forecast_quarters, frac = 1)

stage_status(status = "done")

stage_status("Holdout evaluation", "start")

train_last_qtr <- q_train_orig$qtr[nrow(q_train_orig)]
train_last_month <- quarter_to_month_end(train_last_qtr)
x_train_full <- stats::window(baro_diff_series, end = train_last_month)

if (length(x_train_full) != train_rows * months_per_quarter) {
  stop("Monthly regressor length does not match the training sample.")
}

test_start_month <- quarter_start_month(forecast_quarters[1])
test_end_month <- quarter_to_month_end(forecast_quarters[eval_horizon])
x_future_full <- stats::window(baro_diff_series, start = test_start_month, end = test_end_month)

if (length(x_future_full) != eval_horizon * months_per_quarter) {
  stop("Monthly regressor length does not cover the holdout horizon.")
}

# Prepare monthly data for MF-VAR (use all available training data)
# Calculate the end month for training data
train_last_qtr_mfvar <- q_train_orig$qtr[nrow(q_train_orig)]
train_last_month_mfvar <- quarter_to_month_end(train_last_qtr_mfvar)
monthly_train_holdout <- lapply(monthly_series_list, function(ts_obj) {
  stats::window(ts_obj, end = train_last_month_mfvar)
})

mfvar_holdout_result <- forecast_mfvar(
  q_train_adj,
  monthly_train_holdout,
  transforms,
  n_lags,
  eval_horizon,
  target_vars,
  seed = mfvar_seed,
  return_model = FALSE,
  extract_states = TRUE
)

mfvar_holdout <- mfvar_holdout_result$predictions |>
  dplyr::mutate(model = "MF-VAR")

latent_states_holdout <- mfvar_holdout_result$latent_states

holdout_horizon_steps <- seq_len(eval_horizon)
time_indices_holdout <- compute_time_index(train_rows, holdout_horizon_steps)
ar_generator_holdout <- make_ar_generator(
  q_train_adj = q_train_adj,
  transforms = transforms,
  time_indices = time_indices_holdout,
  horizon_steps = holdout_horizon_steps,
  base_context = "holdout"
)

ar_holdout <- collect_forecast_tbl(
  target_vars = target_vars,
  horizon_steps = holdout_horizon_steps,
  model_label = "AR(2)",
  generator_fn = ar_generator_holdout,
  warn_context = "during holdout",
  warn_on_all_na = FALSE
)

midas_trend_holdout <- collect_forecast_tbl(
  target_vars = target_vars,
  horizon_steps = seq_len(eval_horizon),
  model_label = "MIDAS (trend)",
  generator_fn = function(var) {
    forecast_midas_series(
      y_series = y_ts_list[[var]],
      train_rows = train_rows,
      x_train_full = x_train_full,
      x_future_full = x_future_full,
      horizon = eval_horizon,
      include_trend = TRUE
    )
  },
  warn_context = "during holdout"
)

midas_simple_holdout <- collect_forecast_tbl(
  target_vars = target_vars,
  horizon_steps = seq_len(eval_horizon),
  model_label = "MIDAS",
  generator_fn = function(var) {
    forecast_midas_series(
      y_series = y_ts_list[[var]],
      train_rows = train_rows,
      x_train_full = x_train_full,
      x_future_full = x_future_full,
      horizon = eval_horizon,
      include_trend = FALSE
    )
  },
  warn_context = "during holdout"
)

midas_latent_trend_holdout <- collect_forecast_tbl(
  target_vars = target_vars,
  horizon_steps = seq_len(eval_horizon),
  model_label = "MIDAS-Latent (trend)",
  generator_fn = function(var) {
    forecast_midas_latent(
      y_series = y_ts_list[[var]],
      train_rows = train_rows,
      latent_states_df = latent_states_holdout,
      variable_name = var,
      horizon = eval_horizon,
      include_trend = TRUE
    )
  },
  warn_context = "during holdout"
)

midas_latent_simple_holdout <- collect_forecast_tbl(
  target_vars = target_vars,
  horizon_steps = seq_len(eval_horizon),
  model_label = "MIDAS-Latent",
  generator_fn = function(var) {
    forecast_midas_latent(
      y_series = y_ts_list[[var]],
      train_rows = train_rows,
      latent_states_df = latent_states_holdout,
      variable_name = var,
      horizon = eval_horizon,
      include_trend = FALSE
    )
  },
  warn_context = "during holdout"
)

# --- Gather predictions ----------------------------------------------------
predictions_tbl <- dplyr::bind_rows(
  mfvar_holdout,
  ar_holdout,
  midas_trend_holdout,
  midas_simple_holdout,
  midas_latent_trend_holdout,
  midas_latent_simple_holdout
) |>
  dplyr::mutate(
    quarter_end = forecast_dates[step_ahead],
    horizon = label_horizon(step_ahead)
  )

actual_tbl <- q_test_orig |>
  dplyr::mutate(step_ahead = dplyr::row_number()) |>
  dplyr::select(step_ahead, tidyselect::all_of(target_vars)) |>
  tidyr::pivot_longer(cols = -step_ahead, names_to = "variable", values_to = "actual") |>
  dplyr::mutate(
    quarter_end = forecast_dates[step_ahead],
    horizon = label_horizon(step_ahead)
  )

# --- Metrics for specified horizons ----------------------------------------
metric_inputs <- predictions_tbl |>
  dplyr::inner_join(actual_tbl, by = c("variable", "step_ahead", "quarter_end", "horizon")) |>
  dplyr::mutate(error = prediction - actual)

holdout_metrics_detailed <- metric_inputs |>
  dplyr::group_by(variable, model, step_ahead, horizon) |>
  summarise_metrics()

metrics_tbl <- metric_inputs |>
  dplyr::filter(step_ahead %in% forecast_steps) |>
  dplyr::group_by(model, horizon) |>
  summarise_metrics()

# --- Forecast table (wide) -------------------------------------------------
forecast_wide <- predictions_tbl |>
  tidyr::pivot_wider(
    id_cols = c(variable, step_ahead, horizon, quarter_end),
    names_from = model,
    values_from = prediction
  ) |>
  dplyr::left_join(actual_tbl |> dplyr::select(variable, step_ahead, horizon, quarter_end, actual),
                   by = c("variable", "step_ahead", "horizon", "quarter_end")) |>
  dplyr::arrange(variable, step_ahead)

stage_status(status = "done")

run_cv <- !skip_cv

if (run_cv) {
  stage_status("Cross-validation evaluation", "start")
} else {
  stage_status("Cross-validation evaluation", "start")
  stage_status(status = "skip")
}

cv_initial_quarter <- zoo::as.yearqtr("2015 Q4")
cv_extra_months <- 0:2
extra_env <- Sys.getenv("MFVAR_CV_EXTRA_MONTHS", "")
if (nzchar(extra_env)) {
  extra_split <- unlist(strsplit(extra_env, ","))
  extra_parsed <- suppressWarnings(as.integer(extra_split))
  if (length(extra_parsed) && !all(is.na(extra_parsed))) {
    cv_extra_months <- stats::na.omit(extra_parsed)
  }
}
if (fast_mode) {
  candidate <- intersect(cv_extra_months, 0:1)
  if (length(candidate)) cv_extra_months <- candidate
}
if (!length(cv_extra_months)) cv_extra_months <- 0L

cv_max_folds <- 28L
env_max <- suppressWarnings(as.integer(Sys.getenv("MFVAR_CV_MAX_FOLDS", "")))
if (!is.na(env_max)) {
  cv_max_folds <- env_max
}
if (!is.na(max_folds_override)) {
  cv_max_folds <- max_folds_override
}
if (fast_mode) {
  cv_max_folds <- min(cv_max_folds, 4L)
}

if (run_cv) {
  cv_output <- run_benchmark_cross_validation(
    qdat_adj = qdat_adj,
    qdat_orig = qdat_orig,
    transforms = transforms,
    monthly_series_list = monthly_series_list,
    baro_diff_series = baro_diff_series,
    y_ts_list = y_ts_list,
    target_vars = target_vars,
    forecast_steps = forecast_steps,
    n_lags = n_lags,
    extra_months_options = cv_extra_months,
    max_folds = cv_max_folds,
    initial_train_quarter = cv_initial_quarter,
    progress = !fast_mode
  )

  cv_results <- cv_output$results
  cv_metrics_tbl <- cv_output$metrics_by_horizon
  cv_metrics_overall <- cv_output$metrics_overall
  cv_folds_tbl <- cv_output$folds

  if (!nrow(cv_results)) {
    stage_status(status = "skip")
  } else {
    stage_status(status = "done")
    message(sprintf(
      "Cross-validation processed %d folds across %d coverage settings.",
      cv_output$fold_count,
      length(cv_output$extra_values)
    ))
    timing_totals <- cv_output$timings$totals
    if (length(timing_totals)) {
      message(sprintf(
        "  • CV time (sec): MF-VAR %.2f | MIDAS %.2f | MIDAS-Latent %.2f | AR %.2f | Total %.2f",
        timing_totals["mfvar_seconds"],
        timing_totals["midas_seconds"],
        timing_totals["midas_latent_seconds"],
        timing_totals["ar_seconds"],
        timing_totals["total_seconds"]
      ))
    }
  }
} else {
  cv_output <- list(
    results = tibble::tibble(),
    metrics_by_horizon = tibble::tibble(),
    metrics_overall = tibble::tibble(),
    folds = tibble::tibble(),
    fold_count = 0L,
    extra_values = integer(),
    timings = list(per_fold = tibble::tibble(), totals = numeric())
  )
  cv_results <- cv_output$results
  cv_metrics_tbl <- cv_output$metrics_by_horizon
  cv_metrics_overall <- cv_output$metrics_overall
  cv_folds_tbl <- cv_output$folds
}

cv_timings <- cv_output$timings
cv_timings_tbl <- if (!is.null(cv_timings$per_fold)) cv_timings$per_fold else tibble::tibble()
cv_timing_totals <- if (!is.null(cv_timings$totals)) cv_timings$totals else numeric()

# --- Output summaries and plots --------------------------------------------
stage_status("Output generation", "start")
output_time <- system.time({
  summary_horizon_tbl <- metrics_tbl |>
    dplyr::mutate(
      RMSE = sprintf("%.4f", rmse),
      MAE = sprintf("%.4f", mae),
      Observations = as.character(observations)
    ) |>
    dplyr::select(model, horizon, Observations, RMSE, MAE)

  summary_overall_tbl <- metric_inputs |>
    dplyr::group_by(model) |>
    summarise_metrics() |>
    dplyr::mutate(
      RMSE = sprintf("%.4f", rmse),
      MAE = sprintf("%.4f", mae),
      Observations = as.character(observations)
    ) |>
    dplyr::select(model, Observations, RMSE, MAE)

  summary_cv_tbl <- if (nrow(cv_metrics_tbl)) {
    cv_metrics_tbl |>
      dplyr::arrange(extra_months, model, horizon) |>
      dplyr::mutate(
        `Monthly data` = coverage_label(extra_months),
        RMSE = sprintf("%.4f", rmse),
        MAE = sprintf("%.4f", mae),
        Observations = as.character(observations)
      ) |>
      dplyr::select(`Monthly data`, model, horizon, Observations, RMSE, MAE)
  } else {
    tibble::tibble()
  }

  summary_path <- file.path(OUT_DIR, "model_benchmark_summary.md")
  cv_summary_lines <- table_to_markdown(summary_cv_tbl, c("Monthly data", "Model", "Horizon", "Observations", "RMSE", "MAE"))
  if (!length(cv_summary_lines)) {
    if (!run_cv) {
      cv_summary_lines <- "Cross-validation skipped (--fast/--no-cv)."
    } else {
      cv_summary_lines <- "No cross-validation results (insufficient data)."
    }
  }
  summary_lines <- c(
    "# Benchmark Error Summary",
    "",
    "## Holdout RMSE and MAE by Horizon (aggregated across variables)",
    table_to_markdown(summary_horizon_tbl, c("Model", "Horizon", "Observations", "RMSE", "MAE")),
    "",
    "## Holdout Overall Average Errors",
    table_to_markdown(summary_overall_tbl, c("Model", "Observations", "RMSE", "MAE")),
    "",
    "## Expanding Window Cross-Validation RMSE and MAE by Monthly Coverage",
    cv_summary_lines
  )
  readr::write_lines(summary_lines, summary_path)

  plot_models <- c("Actual", "MF-VAR", "MIDAS (trend)", "MIDAS", "MIDAS-Latent (trend)", "MIDAS-Latent", "AR(2)")
  colour_map <- c(
    "Actual" = "#000000",
    "MF-VAR" = "#1b9e77",
    "MIDAS (trend)" = "#7570b3",
    "MIDAS" = "#d95f02",
    "MIDAS-Latent (trend)" = "#66a61e",
    "MIDAS-Latent" = "#e6ab02",
    "AR(2)" = "#e7298a"
  )
  linetype_map <- c(
    "Actual" = "solid",
    "MF-VAR" = "solid",
    "MIDAS (trend)" = "dashed",
    "MIDAS" = "dashed",
    "MIDAS-Latent (trend)" = "dotdash",
    "MIDAS-Latent" = "dotdash",
    "AR(2)" = "dotted"
  )

  plot_paths <- purrr::map_chr(target_vars, function(var) {
    history_values <- tail(q_train_orig[[var]], history_quarters)
    history_dates <- zoo::as.Date(tail(q_train_orig$qtr, history_quarters), frac = 1)
    history_df <- tibble::tibble(
      quarter_end = history_dates,
      model = factor("Actual", levels = plot_models),
      display_value = convert_for_plot(history_values, var)
    )

    future_actual_df <- actual_tbl |>
      dplyr::filter(variable == var, step_ahead <= max(forecast_steps)) |>
      dplyr::mutate(
        model = factor("Actual", levels = plot_models),
        display_value = convert_for_plot(actual, var)
      ) |>
      dplyr::select(quarter_end, model, display_value)

    actual_series <- dplyr::bind_rows(history_df, future_actual_df) |>
      dplyr::arrange(quarter_end)

    forecast_series <- predictions_tbl |>
      dplyr::filter(variable == var, step_ahead <= max(forecast_steps)) |>
      dplyr::mutate(
        model = factor(model, levels = plot_models),
        display_value = convert_for_plot(prediction, var)
      ) |>
      dplyr::select(quarter_end, model, display_value)

    p <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = actual_series,
        ggplot2::aes(x = quarter_end, y = display_value, colour = model, linetype = model),
        linewidth = 1
      ) +
      ggplot2::geom_point(
        data = actual_series,
        ggplot2::aes(x = quarter_end, y = display_value, colour = model),
        size = 2
      ) +
      ggplot2::geom_line(
        data = forecast_series,
        ggplot2::aes(x = quarter_end, y = display_value, colour = model, linetype = model, group = model),
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        data = forecast_series,
        ggplot2::aes(x = quarter_end, y = display_value, colour = model),
        size = 2
      ) +
      ggplot2::scale_colour_manual(values = colour_map, drop = FALSE) +
      ggplot2::scale_linetype_manual(values = linetype_map, drop = FALSE) +
      ggplot2::labs(
        title = paste("Forecast comparison:", var),
        subtitle = "History (last 4 quarters) with 1-step to 1-year-ahead forecasts",
        x = "Quarter end",
        y = label_for_var(var),
        colour = NULL,
        linetype = NULL
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "bottom")

    plot_path <- file.path(OUT_PLOTS_DIR, paste0("model_benchmark_plot_", var, ".png"))
    ggplot2::ggsave(plot_path, plot = p, width = 8, height = 5, dpi = 150)
    plot_path
  })

  if (nrow(cv_metrics_tbl) && "extra_months" %in% names(cv_metrics_tbl)) {
    cv_metrics_export <- cv_metrics_tbl |>
      dplyr::mutate(monthly_coverage = coverage_label(extra_months)) |>
      dplyr::relocate(monthly_coverage, .before = model)
  } else {
    cv_metrics_export <- cv_metrics_tbl
  }

  if (nrow(cv_results) && all(c("extra_months", "fold_index") %in% names(cv_results))) {
    cv_results_export <- cv_results |>
      dplyr::mutate(monthly_coverage = coverage_label(extra_months)) |>
      dplyr::rename(fold = fold_index) |>
      dplyr::relocate(monthly_coverage, extra_months, fold, cutoff_quarter, forecast_quarter, .before = variable) |>
      dplyr::arrange(extra_months, fold, variable, step_ahead)
  } else {
    cv_results_export <- cv_results
  }

  readr::write_csv(metrics_tbl, file.path(OUT_CSV_DIR, "model_benchmark_metrics.csv"))
  readr::write_csv(holdout_metrics_detailed, file.path(OUT_CSV_DIR, "model_benchmark_holdout_detailed.csv"))
  readr::write_csv(forecast_wide, file.path(OUT_CSV_DIR, "model_benchmark_forecasts.csv"))
  readr::write_csv(cv_metrics_export, file.path(OUT_CSV_DIR, "model_benchmark_cv_metrics.csv"))
  readr::write_csv(cv_results_export, file.path(OUT_CSV_DIR, "model_benchmark_cv_predictions.csv"))
  if (nrow(cv_timings_tbl)) {
    readr::write_csv(cv_timings_tbl, file.path(OUT_CSV_DIR, "model_benchmark_cv_timings.csv"))
  }

  cat(
    "Benchmark comparison complete.\n",
    "  - output/benchmarks/csv/model_benchmark_metrics.csv\n",
    "  - output/benchmarks/csv/model_benchmark_holdout_detailed.csv\n",
    "  - output/benchmarks/csv/model_benchmark_forecasts.csv\n",
    "  - output/benchmarks/csv/model_benchmark_cv_metrics.csv\n",
    "  - output/benchmarks/csv/model_benchmark_cv_predictions.csv\n",
    if (nrow(cv_timings_tbl)) "  - output/benchmarks/csv/model_benchmark_cv_timings.csv\n" else "",
    "  - output/benchmarks/model_benchmark_summary.md\n",
    paste0("  - ", plot_paths, collapse = "\n"),
    "\n",
    sep = ""
  )
})

stage_status(status = "done")
message(sprintf("  • Output generation completed in %.2f seconds.", output_time[["elapsed"]]))
message("\nBenchmark pipeline complete.")
