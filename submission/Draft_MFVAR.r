# Mixed-Frequency VAR with KOF Barometer
# ---------------------------------------------------------------
# This script estimates a mixed-frequency Bayesian VAR (MF-VAR)
# combining the monthly KOF Economic Barometer with quarterly
# series from ./data/data_quarterly.csv.
# ---------------------------------------------------------------
# testing
# --- Project setup ----------------------------------------------------------
activate_project <- function() {
  args_full <- commandArgs(trailingOnly = FALSE)
  script_arg <- grep("^--file=", args_full, value = TRUE)
  if (length(script_arg)) {
    script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
    setwd(dirname(script_path))
  }

  activate_path <- file.path("renv", "activate.R")
  if (!file.exists(activate_path)) {
    stop("Missing renv activation script at renv/activate.R. Run this from the project root or restore renv.")
  }
  source(activate_path, local = TRUE)
}

load_required_packages <- function(pkgs) {
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs)) {
    stop(
      "Missing required packages: ", paste(missing_pkgs, collapse = ", "),
      "\nRun `renv::restore()` in the project to install them."
    )
  }

  suppressPackageStartupMessages(
    invisible(lapply(pkgs, library, character.only = TRUE))
  )
}

activate_project()

required_pkgs <- c(
  "mfbvar", "kofdata", "readr", "dplyr", "tidyr",
  "stringr", "zoo", "xts", "lubridate", "tibble", "ggplot2"
)
load_required_packages(required_pkgs)

# --- Helpers ----------------------------------------------------------------
quarter_to_month_end <- function(yq) {
  end_month <- zoo::as.yearmon(yq) + (2 / 12)
  end_date <- as.Date(end_month)
  c(lubridate::year(end_date), lubridate::month(end_date))
}

build_q_ts <- function(q_subset) {
  q_z <- lapply(names(q_subset)[-1], function(v) zoo::zoo(q_subset[[v]], q_subset$qtr))
  names(q_z) <- names(q_subset)[-1]
  lapply(q_z, stats::as.ts)
}

build_Y <- function(q_subset, baro_subset) {
  q_ts_local <- build_q_ts(q_subset)
  list(
    kofbarometer = baro_subset,
    quarterly = cbind(
      gdp_growth = q_ts_local[["gdp_growth"]],
      inflation  = q_ts_local[["inflation"]],
      exch_rate  = q_ts_local[["exch_rate"]]
    )
  )
}

safe_rmse <- function(pred, obs) {
  residuals <- pred - obs
  residuals <- residuals[!is.na(residuals)]
  if (!length(residuals)) return(NA_real_)
  sqrt(mean(residuals^2))
}

safe_mae <- function(pred, obs) {
  residuals <- pred - obs
  residuals <- residuals[!is.na(residuals)]
  if (!length(residuals)) return(NA_real_)
  mean(abs(residuals))
}

estimate_mfvar_model <- function(Y, n_lags, n_fcst, seed = 123) {
  set.seed(seed)
  prior_obj <- set_prior(
    Y = Y,
    n_lags = n_lags,
    n_reps = 4000,
    n_burnin = 2000,
    n_thin = 4,
    n_fcst = n_fcst,
    d = "intercept",
    aggregation = "average",
    check_roots = TRUE
  )
  estimate_mfbvar(prior_obj, prior = "minn", variance = "iw")
}

# --- I/O paths ---------------------------------------------------------------
DATA_DIR <- file.path(".", "data")
OUT_DIR  <- file.path(".", "output")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# --- Read and transform quarterly data --------------------------------------
q_path <- file.path(DATA_DIR, "data_quarterly.csv")
stopifnot(file.exists(q_path))
qraw <- readr::read_csv(q_path, show_col_types = FALSE)

vars_q <- c("rvgdp", "cpi", "wkfreuro")
missing <- setdiff(vars_q, names(qraw))
if (length(missing)) {
  stop("Missing expected columns in quarterly CSV: ", paste(missing, collapse = ", "))
}

qdat <- qraw %>%
  mutate(qtr = zoo::as.yearqtr(date, format = "%Y-%m")) %>%
  arrange(qtr) %>%
  transmute(
    qtr,
    gdp_growth = 400 * (log(rvgdp) - dplyr::lag(log(rvgdp))),
    inflation  = 400 * (log(cpi)   - dplyr::lag(log(cpi))),
    exch_rate  = log(wkfreuro)
  ) %>%
  drop_na()

# --- Fetch monthly KOF Economic Barometer -----------------------------------
baro_ts <- NULL
for (key in c("kofbarometer", "ch.kof.barometer")) {
  baro_try <- try(kofdata::get_time_series(key), silent = TRUE)
  if (!inherits(baro_try, "try-error") && length(baro_try) > 0) {
    baro_ts <- baro_try[[1]]
    break
  }
}
if (is.null(baro_ts)) {
  stop("Could not download KOF Barometer via 'kofdata' (tried keys 'kofbarometer' and 'ch.kof.barometer').")
}
baro_ts <- stats::ts(
  as.numeric(baro_ts) - mean(as.numeric(baro_ts), na.rm = TRUE),
  start = stats::start(baro_ts),
  frequency = 12
)

# restrict to quarters supported by both data sources
baro_end <- stats::end(baro_ts)
last_q_num <- floor(baro_end[2] / 3)
if (last_q_num == 0) {
  last_q_year <- baro_end[1] - 1
  last_q_num <- 4
} else {
  last_q_year <- baro_end[1]
}
q_cutoff <- zoo::as.yearqtr(sprintf("%d Q%d", last_q_year, last_q_num))
qdat <- qdat %>% filter(qtr <= q_cutoff)
if (!nrow(qdat)) {
  stop("No overlapping quarters between the quarterly dataset and the KOF Barometer.")
}

# --- Align sample windows ----------------------------------------------------
q_start <- qdat$qtr[1]
q_start_date <- zoo::as.Date(q_start, frac = 1)
q_start_year <- lubridate::year(q_start_date)
q_start_q <- lubridate::quarter(q_start_date)
m_start <- c(q_start_year, (q_start_q - 1) * 3 + 1)
m_start_back2 <- c(
  m_start[1] - as.integer(m_start[2] <= 2),
  ((m_start[2] + 10) %% 12) + 1
)
q_end <- qdat$qtr[nrow(qdat)]
q_end_date <- zoo::as.Date(q_end, frac = 1)
q_end_year <- lubridate::year(q_end_date)
q_end_q <- lubridate::quarter(q_end_date)
m_end <- c(q_end_year, q_end_q * 3)
baro_ts <- stats::window(baro_ts, start = m_start_back2, end = m_end)

# --- Build the mixed-frequency data list ------------------------------------
Y <- build_Y(qdat, baro_ts)

n_lags <- 5

# --- Forecast evaluation ----------------------------------------------------
eval_table <- NULL
evaluation_path <- NULL
eval_horizon <- {
  max_holdout <- nrow(qdat) - (n_lags + 1)
  if (max_holdout < 0) max_holdout <- 0
  min(4, max_holdout)
}

if (eval_horizon >= 1) {
  q_train <- qdat %>% dplyr::slice_head(n = nrow(qdat) - eval_horizon)
  q_eval <- qdat %>% dplyr::slice_tail(n = eval_horizon)

  baro_train_end <- quarter_to_month_end(q_train$qtr[nrow(q_train)])
  baro_train <- stats::window(baro_ts, end = baro_train_end)
  Y_train <- build_Y(q_train, baro_train)

  mod_eval <- estimate_mfvar_model(Y_train, n_lags, n_fcst = eval_horizon, seed = 123)
  fc_eval_raw <- predict(mod_eval, aggregate_fcst = TRUE, pred_bands = 0.8)

  fc_eval <- fc_eval_raw %>%
    filter(variable %in% c("gdp_growth", "inflation", "exch_rate")) %>%
    arrange(variable, time) %>%
    group_by(variable) %>%
    mutate(step_ahead = dplyr::row_number()) %>%
    filter(step_ahead <= eval_horizon) %>%
    ungroup() %>%
    select(variable, step_ahead, mfvar = median)

  actual_eval <- q_eval %>%
    mutate(step_ahead = dplyr::row_number()) %>%
    select(step_ahead, gdp_growth, inflation, exch_rate) %>%
    tidyr::pivot_longer(-step_ahead, names_to = "variable", values_to = "actual")

  mfvar_eval <- dplyr::inner_join(fc_eval, actual_eval, by = c("variable", "step_ahead"))
  mfvar_metrics <- mfvar_eval %>%
    group_by(variable) %>%
    summarise(
      model = "MF-VAR",
      rmse = safe_rmse(mfvar, actual),
      mae = safe_mae(mfvar, actual),
      .groups = "drop"
    )

  ar_preds_list <- lapply(c("gdp_growth", "inflation", "exch_rate"), function(var) {
    series <- q_train[[var]]
    preds <- tryCatch({
      fit <- stats::arima(series, order = c(2, 0, 0))
      as.numeric(stats::predict(fit, n.ahead = eval_horizon)$pred)
    }, error = function(e) {
      warning(sprintf("AR(2) benchmark failed for %s: %s", var, e$message))
      rep(NA_real_, eval_horizon)
    })
    tibble::tibble(variable = var, step_ahead = seq_len(eval_horizon), ar2 = preds)
  })

  ar_eval <- dplyr::bind_rows(ar_preds_list) %>%
    dplyr::inner_join(actual_eval, by = c("variable", "step_ahead"))

  ar_metrics <- ar_eval %>%
    group_by(variable) %>%
    summarise(
      model = "AR(2)",
      rmse = safe_rmse(ar2, actual),
      mae = safe_mae(ar2, actual),
      .groups = "drop"
    )

  eval_table <- dplyr::bind_rows(mfvar_metrics, ar_metrics) %>%
    arrange(variable, model)

  evaluation_path <- file.path(OUT_DIR, "forecast_evaluation.csv")
  readr::write_csv(eval_table, evaluation_path)
} else {
  message("Evaluation skipped: not enough observations left after reserving lags.")
}

cv_table <- NULL
cv_path <- NULL
cv_folds <- 0
cv_horizon <- {
  max_cv <- nrow(qdat) - (n_lags + 2)
  if (max_cv < 0) max_cv <- 0
  min(8, max_cv)
}

if (cv_horizon >= 1) {
  target_vars <- c("gdp_growth", "inflation", "exch_rate")
  cv_indices <- seq.int(nrow(qdat) - cv_horizon + 1, nrow(qdat))
  cv_records <- vector("list", length(cv_indices))

  for (i in seq_along(cv_indices)) {
    idx <- cv_indices[i]
    train_rows <- idx - 1
    if (train_rows <= n_lags) {
      cv_records[[i]] <- NULL
      next
    }

    q_train <- qdat %>% dplyr::slice_head(n = train_rows)
    q_test <- qdat %>% dplyr::slice(idx)

    baro_train_end <- quarter_to_month_end(q_train$qtr[nrow(q_train)])
    baro_train <- stats::window(baro_ts, end = baro_train_end)
    Y_cv <- build_Y(q_train, baro_train)

    mod_cv <- try(estimate_mfvar_model(Y_cv, n_lags, n_fcst = 1, seed = 200 + idx), silent = TRUE)
    if (inherits(mod_cv, "try-error")) {
      warning(sprintf("MF-VAR cross-validation fold %d failed: %s", idx, mod_cv))
      cv_records[[i]] <- NULL
      next
    }

    fc_cv_raw <- try(predict(mod_cv, aggregate_fcst = TRUE, pred_bands = 0.8), silent = TRUE)
    if (inherits(fc_cv_raw, "try-error")) {
      warning(sprintf("MF-VAR prediction failed for fold %d: %s", idx, fc_cv_raw))
      cv_records[[i]] <- NULL
      next
    }

    mfvar_fold <- fc_cv_raw %>%
      filter(variable %in% target_vars) %>%
      select(variable, mfvar = median)

    actual_fold <- q_test %>%
      select(dplyr::all_of(target_vars)) %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = "variable", values_to = "actual")

    ar_fold <- tibble::tibble(
      variable = target_vars,
      ar2 = vapply(target_vars, function(var) {
        series <- q_train[[var]]
        tryCatch({
          fit <- withCallingHandlers(
            stats::arima(series, order = c(2, 0, 0)),
            warning = function(w) {
              warning(sprintf("AR(2) warning in fold %d for %s: %s", idx, var, w$message))
              invokeRestart("muffleWarning")
            }
          )
          as.numeric(stats::predict(fit, n.ahead = 1)$pred[1])
        }, error = function(e) {
          warning(sprintf("AR(2) benchmark failed in fold %d for %s: %s", idx, var, e$message))
          NA_real_
        })
      }, numeric(1))
    )

    cv_records[[i]] <- actual_fold %>%
      left_join(mfvar_fold, by = "variable") %>%
      left_join(ar_fold, by = "variable") %>%
      mutate(fold_index = idx)
  }

  cv_results <- dplyr::bind_rows(cv_records)
  if (nrow(cv_results)) {
    cv_folds <- dplyr::n_distinct(cv_results$fold_index)

    mfvar_cv <- cv_results %>%
      group_by(variable) %>%
      summarise(
        model = "MF-VAR",
        rmse = safe_rmse(mfvar, actual),
        mae = safe_mae(mfvar, actual),
        .groups = "drop"
      )

    ar_cv <- cv_results %>%
      group_by(variable) %>%
      summarise(
        model = "AR(2)",
        rmse = safe_rmse(ar2, actual),
        mae = safe_mae(ar2, actual),
        .groups = "drop"
      )

    cv_table <- dplyr::bind_rows(mfvar_cv, ar_cv) %>%
      arrange(variable, model)

    cv_path <- file.path(OUT_DIR, "forecast_cross_validation.csv")
    readr::write_csv(cv_results, cv_path)
  } else {
    message("Cross-validation skipped: no valid folds produced.")
  }
} else {
  message("Cross-validation skipped: not enough data for folds beyond lag length.")
}

# --- Prior, estimation, and forecasting -------------------------------------
mod_ss <- estimate_mfvar_model(Y, n_lags, n_fcst = 12, seed = 123)

# --- Summaries ---------------------------------------------------------------
summary_path <- file.path(OUT_DIR, "mfvar_summary.txt")
sink(summary_path)
cat("\n==== MF-VAR summary (Minnesota prior, IW covariance) ====\n\n")
print(summary(mod_ss))

cat("\n==== Forecast evaluation ====\n")
if (!is.null(eval_table)) {
  cat(sprintf("\nHoldout horizon: %d quarter(s).\n\n", eval_horizon))
  print(eval_table, n = nrow(eval_table))
} else {
  cat("\nSkipped (insufficient holdout sample after reserving lags).\n")
}

cat("\n==== Rolling 1-step cross-validation ====\n")
if (!is.null(cv_table)) {
  cat(sprintf("\nFolds: %d (last %d quarter(s)).\n\n", cv_folds, cv_horizon))
  print(cv_table, n = nrow(cv_table))
} else {
  cat("\nSkipped (not enough data or no valid folds).\n")
}
sink()

# --- Forecasts ---------------------------------------------------------------
fc <- predict(mod_ss, aggregate_fcst = TRUE, pred_bands = 0.8)

fc_q <- fc %>%
  filter(variable %in% c("gdp_growth", "inflation", "exch_rate")) %>%
  arrange(variable, time) %>%
  group_by(variable) %>%
  mutate(step_ahead = row_number()) %>%
  ungroup() %>%
  mutate(
    horizon = case_when(
      step_ahead == 1 ~ "1-step ahead",
      step_ahead == 4 ~ "1-year ahead",
      TRUE ~ NA_character_
    ),
    median = if_else(variable == "exch_rate", exp(median), median),
    lower  = if_else(variable == "exch_rate", exp(lower), lower),
    upper  = if_else(variable == "exch_rate", exp(upper), upper)
  )

fc_targets <- fc_q %>%
  filter(!is.na(horizon)) %>%
  select(variable, horizon, time, median, lower, upper)

readr::write_csv(fc,         file.path(OUT_DIR, "mfvar_forecasts_full.csv"))
readr::write_csv(fc_targets, file.path(OUT_DIR, "mfvar_forecasts_targets.csv"))

# --- Plots -------------------------------------------------------------------
fc_gdp <- fc_q %>% filter(variable == "gdp_growth")
if (nrow(fc_gdp)) {
  p <- ggplot(fc_gdp, aes(x = time)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
    geom_line(aes(y = median)) +
    labs(
      title = "MF-VAR forecast for GDP growth (annualised %)",
      x = "Quarter",
      y = "Annualised percentage"
    ) +
    theme_minimal(base_size = 12)
  ggsave(file.path(OUT_DIR, "forecast_gdp_growth.png"), p, width = 8, height = 4.5, dpi = 120)
}

# --- Persist model -----------------------------------------------------------
saveRDS(mod_ss, file.path(OUT_DIR, "mfvar_model_ss.rds"))

message_lines <- c(
  "Done. Wrote:\n",
  "  - output/mfvar_summary.txt\n",
  "  - output/mfvar_forecasts_full.csv\n",
  "  - output/mfvar_forecasts_targets.csv\n"
)

if (!is.null(evaluation_path)) {
  message_lines <- c(message_lines, "  - output/forecast_evaluation.csv\n")
} else {
  message_lines <- c(message_lines, "  - forecast evaluation skipped (not enough holdout data)\n")
}

if (!is.null(cv_path)) {
  message_lines <- c(message_lines, "  - output/forecast_cross_validation.csv\n")
} else {
  message_lines <- c(message_lines, "  - cross-validation skipped or unavailable\n")
}

message_lines <- c(
  message_lines,
  "  - output/forecast_gdp_growth.png (if GDP present)\n",
  "  - output/mfvar_model_ss.rds"
)

message(paste0(message_lines, collapse = ""))
