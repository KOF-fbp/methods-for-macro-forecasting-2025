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

predict_ar2 <- function(series, n_ahead, var_label = "series", context = NULL) {
  stopifnot(n_ahead >= 1)
  series <- series[is.finite(series)]
  if (length(series) < 4) {
    ctx <- if (is.null(context)) "" else sprintf(" (%s)", context)
    warning(sprintf("AR(2)%s for %s skipped: not enough observations", ctx, var_label))
    return(rep(NA_real_, n_ahead))
  }

  methods <- list(
    list(name = "stats::ar YW", fit = function() stats::ar(series, order.max = 2, aic = FALSE, method = "yw")),
    list(name = "stats::ar OLS", fit = function() stats::ar(series, order.max = 2, aic = FALSE, method = "ols")),
    list(name = "stats::arima", fit = function() stats::arima(series, order = c(2, 0, 0), transform.pars = FALSE, optim.control = list(maxit = 2000)))
  )

  last_issue <- NULL

  for (method in methods) {
    warn_msg <- NULL
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

  ctx <- if (is.null(context)) "" else sprintf(" (%s)", context)
  msg <- if (is.null(last_issue)) "no diagnostic" else last_issue
  warning(sprintf("AR(2)%s for %s failed: %s", ctx, var_label, msg))
  rep(NA_real_, n_ahead)
}
