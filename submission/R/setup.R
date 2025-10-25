# Project setup utilities

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

required_pkgs <- c(
  "mfbvar", "kofdata", "readr", "dplyr", "tidyr",
  "stringr", "zoo", "xts", "lubridate", "tibble", "ggplot2"
)

target_variables <- c("gdp_growth", "inflation", "exch_rate")

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
  prior_obj <- mfbvar::set_prior(
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
  mfbvar::estimate_mfbvar(prior_obj, prior = "minn", variance = "iw")
}
