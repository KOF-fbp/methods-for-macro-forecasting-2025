# Mixed-Frequency VAR with KOF Barometer
# ---------------------------------------------------------------
# This script estimates a mixed-frequency Bayesian VAR (MF-VAR)
# combining the monthly KOF Economic Barometer with quarterly
# series from ./data/data_quarterly.csv.
# ---------------------------------------------------------------
# testing
# --- Project setup ----------------------------------------------------------
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

# --- Helper: ensure packages are available ----------------------------------
ensure_pkg <- function(pkgs) {
  miss <- vapply(pkgs, function(pkg) !requireNamespace(pkg, quietly = TRUE), logical(1))
  missing_pkgs <- pkgs[miss]
  if (length(missing_pkgs)) {
    stop(
      "Missing required packages: ", paste(missing_pkgs, collapse = ", "),
      "\nRun `renv::restore()` in the project to install them."
    )
  }
}

need_pkgs <- c(
  "mfbvar", "kofdata", "readr", "dplyr", "tidyr",
  "stringr", "zoo", "xts", "lubridate", "tibble", "ggplot2"
)
ensure_pkg(need_pkgs)

suppressPackageStartupMessages({
  library(mfbvar)
  library(kofdata)
  library(readr); library(dplyr); library(tidyr); library(stringr)
  library(zoo); library(xts); library(lubridate); library(tibble)
  library(ggplot2)
})

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

q_z_list <- lapply(names(qdat)[-1], function(v) zoo::zoo(qdat[[v]], qdat$qtr))
names(q_z_list) <- names(qdat)[-1]
q_ts <- lapply(q_z_list, stats::as.ts)

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
Y <- list(
  kofbarometer = baro_ts,
  quarterly = cbind(
    gdp_growth = q_ts[["gdp_growth"]],
    inflation  = q_ts[["inflation"]],
    exch_rate  = q_ts[["exch_rate"]]
  )
)

n_lags <- 5

# --- Prior, estimation, and forecasting -------------------------------------
set.seed(123)
prior_obj <- set_prior(
  Y = Y,
  n_lags = n_lags,
  n_reps = 4000,
  n_burnin = 2000,
  n_thin = 4,
  n_fcst = 12,
  d = "intercept",
  aggregation = "average",
  check_roots = TRUE
)

mod_ss <- estimate_mfbvar(prior_obj, prior = "minn", variance = "iw")

# --- Summaries ---------------------------------------------------------------
sink(file.path(OUT_DIR, "mfvar_summary.txt"))
cat("\n==== MF-VAR summary (Minnesota prior, IW covariance) ====\n\n")
print(summary(mod_ss))
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
    labs(title = "MF-VAR forecast for GDP growth (annualised %)", x = NULL, y = NULL) +
    theme_minimal(base_size = 12)
  ggsave(file.path(OUT_DIR, "forecast_gdp_growth.png"), p, width = 8, height = 4.5, dpi = 120)
}

# --- Persist model -----------------------------------------------------------
saveRDS(mod_ss, file.path(OUT_DIR, "mfvar_model_ss.rds"))

message(
  "Done. Wrote:\n",
  "  - output/mfvar_summary.txt\n",
  "  - output/mfvar_forecasts_full.csv\n",
  "  - output/mfvar_forecasts_targets.csv\n",
  "  - output/forecast_gdp_growth.png (if GDP present)\n",
  "  - output/mfvar_model_ss.rds"
)
