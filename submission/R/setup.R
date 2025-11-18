# Project setup utilities
# -------------------------
# This file holds helper functions that prepare the R session
# (library loading, working directory setup) and provide core
# utilities shared across the MF-VAR pipeline. The intent is to
# keep the top-level scripts concise and avoid duplicating logic.

activate_project <- function() {
  # Ensure relative paths inside R scripts point to the project root.
  # Rscript does not automatically set the working directory to the
  # script location, so we manually detect it from command line args.
  args_full <- commandArgs(trailingOnly = FALSE)
  script_arg <- grep("^--file=", args_full, value = TRUE)
  if (length(script_arg)) {
    script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
    setwd(dirname(script_path))
  }

  # Load renv so package versions match the lockfile. Without this
  # call, running the scripts outside of RStudio could pick up the
  # wrong package versions from the global library.
  activate_path <- file.path("renv", "activate.R")
  if (!file.exists(activate_path)) {
    stop("Missing renv activation script at renv/activate.R. Run this from the project root or restore renv.")
  }
  source(activate_path, local = TRUE)
}

load_required_packages <- function(pkgs) {
  # Load every package listed in `pkgs`. We do an upfront availability
  # check because beginners often see cryptic errors when a package is
  # absent; here we halt with a clear message pointing to renv::restore().
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs)) {
    stop(
      "Missing required packages: ", paste(missing_pkgs, collapse = ", "),
      "\nRun `renv::restore()` in the project to install them."
    )
  }

  # With packages confirmed, attach them in one go while suppressing
  # their startup banners to keep console output tidy.
  suppressPackageStartupMessages(
    invisible(lapply(pkgs, library, character.only = TRUE))
  )
}

# Names of packages every script assumes are present and attached.
# Tidyverse tools cover data wrangling/plotting; mfbvar provides the
# Bayesian VAR machinery; kofdata handles the barometer download.
required_pkgs <- c(
  "mfbvar", "kofdata", "readr", "dplyr", "tidyr",
  "stringr", "zoo", "xts", "lubridate", "tibble", "ggplot2"
)


######################################################################
# Good Stuff
######################################################################

# Quarterly series the model forecasts and evaluates. Downstream code
# uses this vector to keep tables/plots aligned, so the names must match
# the column names produced by the data-processing step.
target_variables <- c("gdp_growth", "inflation", "exch_rate")

default_monthly_indicators <- c(
  "plkopr",
  "devkum",
  "amarbma",
  "snboffzisa",
  "smi_monthly_avg",
  "smi_monthly_return"
)

resolve_monthly_indicators <- function() {
  option_value <- getOption("mfvar.monthly_indicators", NULL)
  if (!is.null(option_value)) {
    indicators <- trimws(as.character(option_value))
    indicators <- indicators[nzchar(indicators)]
    if (length(indicators)) {
      return(indicators)
    }
  }

  env_value <- Sys.getenv("MFVAR_MONTHLY_INDICATORS", "")
  if (nzchar(env_value)) {
    indicators <- trimws(strsplit(env_value, ",", fixed = TRUE)[[1]])
    indicators <- indicators[nzchar(indicators)]
    if (length(indicators)) {
      return(indicators)
    }
    warning("MFVAR_MONTHLY_INDICATORS override ignored because it produced no valid entries.")
  }

  default_monthly_indicators
}

# Guarded versions of RMSE/MAE that drop NAs so incomplete folds do not
# crash the evaluation suite. Returning NA_real_ when no residuals are
# available makes it easy to spot missing metrics later on.
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
  # Train the mixed-frequency VAR using the Minnesota prior and an
  # inverse-Wishart prior on the covariance matrix. The `Y` object is
  # the structured list created by build_Y(), containing both the
  # quarterly and monthly inputs the mfbvar package expects.
  set.seed(seed)
  n_reps <- getOption("mfvar.n_reps", 4000L)
  n_burnin <- getOption("mfvar.n_burnin", 2000L)
  n_thin <- getOption("mfvar.n_thin", 4L)
  prior_obj <- mfbvar::set_prior(
    Y = Y,
    n_lags = n_lags,
    n_reps = n_reps,
    n_burnin = n_burnin,
    n_thin = n_thin,
    n_fcst = n_fcst,
    d = "intercept",
    aggregation = "average",
    check_roots = TRUE
  )
  # Once the prior is set up, draw posterior samples for the state-space
  # representation. The returned object stores draws for coefficients,
  # covariance parameters, and forecast distributions.
  mfbvar::estimate_mfbvar(prior_obj, prior = "minn", variance = "iw")
}

predict_ar2 <- function(series, n_ahead, var_label = "series", context = NULL) {
  # Produce AR(2) benchmark forecasts for a univariate quarterly series.
  # This is our sanity-check model; if the MF-VAR cannot beat it, we know
  # the hyperparameters or inputs may need attention.
  stopifnot(n_ahead >= 1)
  series <- series[is.finite(series)]
  if (length(series) < 4) {
    ctx <- if (is.null(context)) "" else sprintf(" (%s)", context)
    warning(sprintf("AR(2)%s for %s skipped: not enough observations", ctx, var_label))
    return(rep(NA_real_, n_ahead))
  }

  methods <- list(
    # Try a few estimation approaches, ordered from fastest to most robust.
    # Beginners often rely on auto.arima(); here we cycle manually so we
    # can fall back gracefully if one approach fails to converge.
    list(name = "stats::ar YW", fit = function() stats::ar(series, order.max = 2, aic = FALSE, method = "yw")),
    list(name = "stats::ar OLS", fit = function() stats::ar(series, order.max = 2, aic = FALSE, method = "ols")),
    list(name = "stats::arima", fit = function() stats::arima(series, order = c(2, 0, 0), transform.pars = FALSE, optim.control = list(maxit = 2000)))
  )

  last_issue <- NULL

  for (method in methods) {
    warn_msg <- NULL
    # withCallingHandlers lets us intercept warnings without stopping
    # execution. This keeps the loop running even if a particular method
    # complains about singularities or non-stationarity.
    fit <- tryCatch(
      withCallingHandlers(
        method$fit(),
        warning = function(w) {
          warn_msg <<- conditionMessage(w)
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        last_issue <<- sprintf("%s (%s fit)", conditionMessage(e), method$name)
        NULL
      }
    )

    if (is.null(fit)) {
      next
    }

    if (!is.null(warn_msg)) {
      last_issue <- sprintf("%s (%s fit)", warn_msg, method$name)
      next
    }

    preds <- tryCatch(
      {
        fc <- stats::predict(fit, n.ahead = n_ahead)
        if (is.list(fc) && !is.null(fc$pred)) fc$pred else fc
      },
      error = function(e) {
        last_issue <<- sprintf("%s (%s predict)", conditionMessage(e), method$name)
        NULL
      }
    )

    if (is.null(preds)) {
      next
    }

    preds <- as.numeric(preds)
    if (all(is.finite(preds))) {
      return(preds)
    }

    last_issue <- sprintf("Non-finite predictions (%s)", method$name)
  }

  # If every method fails, log the most informative issue we saw and
  # return NA forecasts. This keeps the MF-VAR evaluation running while
  # signalling that the fallback model needs attention.
  ctx <- if (is.null(context)) "" else sprintf(" (%s)", context)
  msg <- if (is.null(last_issue)) "no diagnostic" else last_issue
  warning(sprintf("AR(2)%s for %s failed: %s", ctx, var_label, msg))
  rep(NA_real_, n_ahead)
}

predict_rw_trend <- function(series, n_ahead, var_label = "series", context = NULL) {
  stopifnot(n_ahead >= 1)
  series <- series[is.finite(series)]
  if (!length(series)) {
    ctx <- if (is.null(context)) "" else sprintf(" (%s)", context)
    warning(sprintf("RW-trend%s for %s skipped: no finite observations", ctx, var_label))
    return(rep(NA_real_, n_ahead))
  }

  last_value <- tail(series, 1)
  if (length(series) >= 2) {
    drift <- mean(diff(series), na.rm = TRUE)
    if (!is.finite(drift)) {
      drift <- 0
    }
  } else {
    drift <- 0
  }

  last_value + drift * seq_len(n_ahead)
}
