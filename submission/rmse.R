#in this file, we will test BVAR model with different priors, selected
#variables and lags to compute the RMSE in each case.

#results in this file will be used to compare different
# models and select the best one to use in main.R

#--------------------- libraries ---------------------#
library(ggplot2)
library(dplyr)
library(tidyr)
library(BVAR)
library(lubridate)
library(vars)  # needed for classical VAR benchmark

source("stationary.R")
# --------------------- set up ------------------------#
# set seed for reproducibility
set.seed(42)

# load data and data manipulation
df <- utils::read.csv("data/data_quarterly.csv")

df$date <- as.Date(paste0(df$date, "-01")) # format date
df <- df %>% filter(date <= as.Date("2025-07-01")) # until 01.10.2025

# inflate CPI to get inflation rate
df$inflation <- (df$cpi - dplyr::lag(df$cpi, 1)) / dplyr::lag(df$cpi, 1) 
df <- df %>% filter(!is.na(inflation)) #remove first NA row

# -------------------- apply log + growth transformations --------------------
# define the rate variables (do NOT log-transform these)
rate_variables <- c("inflation", "urilo", "srate", "srate_ge")

# safe log: add small offset if non-positive values present
safe_log <- function(x, tiny = 1e-6) {
  x_num <- as.numeric(x)
  if (all(is.na(x_num))) return(x_num)
  if (any(x_num <= 0, na.rm = TRUE)) {
    offset <- abs(min(x_num, na.rm = TRUE)) + tiny
    warning("Non-positive values found; adding offset = ", signif(offset, 6), " before log.")
    x_num <- x_num + offset
  }
  return(log(x_num))
}

# apply log to non-rate variables (excluding date)
for (var in names(df)) {
  if (!(var %in% rate_variables) && var != "date") {
    df[[var]] <- safe_log(df[[var]])
  }
}

# convert to growth rates (percent) for the same non-rate variables
for (var in names(df)) {
  if (!(var %in% rate_variables) && var != "date") {
    df[[var]] <- 100 * (df[[var]] - dplyr::lag(df[[var]], 1))
  }
}

# remove initial rows with NA introduced by growth calculation if needed
# (subsequent code already removes rows with NA in gdp)

#check if stationary using functions from stationary.R
for (var in setdiff(colnames(df), "date")) {  # excluding date column
  cat("Checking stationarity for:", var, "\n")
  ts_data <- ts(df[[var]])
  stationary_ts <- make_stationary(ts_data, use_log = TRUE)   # returns aligned vector (same length)
  df[[var]] <- as.numeric(stationary_ts)                     # assign directly, no extra NA
}
df <- df %>% filter(!is.na(gdp))  # remove first row with NA

# ---------------------- lags ---------------------#

# possible lags: 1, 4 (one year), 8 (two years)
lags <- c(1, 4)


# ------------------ selected variables ------------------#

#selected variables for BVAR model

selected_variables_1 <- c("gdp", "cpi", "wkfreuro", "consp", "consg", 
                          "ifix", "icnstr", "ime", "exc1", "imc1", "ltot", "uroff", "wage", 
                          "srate", "poilusd", "wd", "pcioecd", "vaabcde", "vaghji")

#from this initial set, we will also try a smaller set of variables

selected_variables_2 <- c("gdp", "cpi", "wkfreuro", "consp", "consg", "ifix", 
                          "exc1", "imc1", "ltot", "uroff", "wage", "srate", 
                          "poilusd", "wd", "pcioecd")

selected_variables_3 <- c("gdp", "cpi", "wkfreuro", "consp", "exc1", "ltot",
                          "wage", "srate")

#-------------------------------------------------------------------
# List of variable sets to test
variable_sets <- list(
  set_1 = selected_variables_1,
  set_2 = selected_variables_2,
  set_3 = selected_variables_3
)






#--------------------- priors ---------------------#
# Define different prior configurations to test
mn <- bv_minnesota(
  lambda = bv_lambda(mode = 0.5, sd = 0.1, min = 0.001, max = 5),
  alpha  = bv_alpha(mode = 4),
)

soc <- bv_soc(mode = 1, sd = 0.5)   # shrink sum of AR coeffs to 1, with variance
priors <- bv_priors(
  hyper = c("lambda", "alpha", "psi"),
  mn = mn,
  soc = soc
)

# Diffuse (flat) prior: approssimazione per OLS usando Minnesota con lambda molto piccolo
diffuse_prior_bv <- bv_priors(
  hyper = c("lambda", "alpha"),
  mn = bv_minnesota(
    lambda = bv_lambda(mode = 1e-6, sd = 1e-6, min = 1e-12, max = 1),
    alpha  = bv_alpha(mode = 4)
  )
)

# Prior basati su componenti già definiti (mn e soc)
mn_prior_bv <- bv_priors(
  hyper = c("lambda", "alpha"),
  mn = mn
)

soc_prior_bv <- bv_priors(
  hyper = c("lambda", "alpha", "psi"),
  mn = mn,
  soc = soc
)

# Lista di prior pronta per essere passata a BVAR::bvar
prior_configs <- list(
  mn      = mn_prior_bv,
  soc     = soc_prior_bv,
  diffuse = diffuse_prior_bv
)

rolling_window_size <- 40  # e.g., 40 quarters (10 years)

#---------------------- end of setup ---------------------#
compute_bvar_rmse <- function(data, variables, lags, prior, model_type = c("bvar", "var")) {
  cat("Computing RMSE for variables:", paste(variables, collapse = ", "), 
      "with lags =", lags, "and model type =", model_type, "\n")

  model_type <- match.arg(model_type)
  errors_matrix <- NULL
  valid_windows <- 0
  
  for (i in seq(rolling_window_size, nrow(data) - 1)) {
    train_data <- data[(i - rolling_window_size + 1):i, , drop = FALSE]
    test_data  <- data[i + 1, , drop = FALSE]
    
    # subset and coerce to numeric
    train_subset <- train_data[, variables, drop = FALSE]
    train_num <- as.data.frame(lapply(train_subset, function(x) as.numeric(x)))
    names(train_num) <- variables
    
    # basic validation: no NA, finite, enough rows
    if (any(is.na(train_num)) || any(!is.finite(as.matrix(train_num)))) {
      message(sprintf("Skipping window ending at row %d: NA or non-finite in training data.", i))
      next
    }
    if (nrow(train_num) <= max(lags)) {
      message(sprintf("Skipping window ending at row %d: not enough observations (n=%d).", i, nrow(train_num)))
      next
    }
    
    # also validate test row
    test_vals <- as.numeric(test_data[, variables])
    if (any(is.na(test_vals)) || any(!is.finite(test_vals))) {
      message(sprintf("Skipping window ending at row %d: NA or non-finite in test data.", i))
      next
    }
    
    if (model_type == "bvar") {
      model <- tryCatch(
        BVAR::bvar(data = train_num, lags = lags, priors = prior, ndraw = 1000, burnin = 500),
        error = function(e) {
          message("bvar() failed at window ending ", i, ": ", conditionMessage(e))
          return(NULL)
        }
      )
      if (is.null(model)) next
      
      forecast <- tryCatch(
        predict(model, h = 1),
        error = function(e) {
          message("predict() failed at window ending ", i, ": ", conditionMessage(e))
          return(NULL)
        }
      )
      if (is.null(forecast) || is.null(forecast$fcst)) next
      
      forecasted_mean <- sapply(forecast$fcst, function(x) x$mean[1])
      error <- forecasted_mean - test_vals
      
    } else { # classical VAR benchmark
      model_var <- tryCatch(
        vars::VAR(train_num, p = max(1, lags), type = "const"),
        error = function(e) {
          message("VAR() failed at window ending ", i, ": ", conditionMessage(e))
          return(NULL)
        }
      )
      if (is.null(model_var)) next
      
      pred <- tryCatch(
        predict(model_var, n.ahead = 1, ci = 0.95),
        error = function(e) {
          message("predict.VAR() failed at window ending ", i, ": ", conditionMessage(e))
          return(NULL)
        }
      )
      if (is.null(pred) || is.null(pred$fcst)) next
      
      # extract point forecasts for each variable
      forecasted_mean <- sapply(variables, function(v) {
        fcst_mat <- pred$fcst[[v]]
        # ensure we have forecast matrix and take fcst column at row 1
        if (!is.null(fcst_mat) && "fcst" %in% colnames(fcst_mat)) {
          as.numeric(fcst_mat[1, "fcst"])
        } else {
          NA_real_
        }
      })
      error <- forecasted_mean - test_vals
    }
    
    if (any(is.na(error)) || any(!is.finite(error))) {
      message(sprintf("Skipping window ending at row %d: invalid forecast error.", i))
      next
    }
    
    if (is.null(errors_matrix)) {
      errors_matrix <- matrix(error, nrow = 1)
    } else {
      errors_matrix <- rbind(errors_matrix, error)
    }
    valid_windows <- valid_windows + 1
  }
  
  if (is.null(errors_matrix) || valid_windows == 0) {
    warning("No valid rolling windows were processed. Returning NA for RMSE.")
    return(NA_real_)
  }
  
  rmse_values <- sqrt(colMeans(errors_matrix^2))
  overall_rmse <- mean(rmse_values)
  return(overall_rmse)
}


#–-------------------- BVAR/VAR RMSE computation ---------------------#

results <- list()

for (lag in lags) {
  print(paste("Testing lag:", lag))
  for (var_set_name in names(variable_sets)) {
    selected_vars <- variable_sets[[var_set_name]]
    
    for (prior_name in names(prior_configs)) {
      prior_config <- prior_configs[[prior_name]]
      
      model_type <- if (prior_name == "diffuse") "var" else "bvar"
      
      rmse_result <- compute_bvar_rmse(
        data = df,
        variables = selected_vars,
        lags = lag,
        prior = prior_config,
        model_type = model_type
      )
      
      results[[paste(lag, var_set_name, prior_name, sep = "_")]] <- rmse_result
      print(paste("Lag:", lag, "Variable Set:", var_set_name, "Prior:", prior_name, "Model:", model_type, "RMSE:", rmse_result))
    }
  }
}
