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
df$inflation <- 100*(df$cpi - dplyr::lag(df$cpi, 1)) / dplyr::lag(df$cpi, 1) 
df <- df %>% filter(!is.na(inflation)) #remove first NA row

# gdp growth instead of gdp
df$gdp <- log(df$gdp)
# -------------------- apply log + growth transformations --------------------
# define the rate variables (do NOT log-transform these)
rate_variables <- c("inflation", "urilo", "srate", "srate_ge")

# safe log: add small offset if non-positive values present
#safe_log <- function(x, tiny = 1e-6) {
#  x_num <- as.numeric(x)
#  if (all(is.na(x_num))) return(x_num)
#  if (any(x_num <= 0, na.rm = TRUE)) {
#    offset <- abs(min(x_num, na.rm = TRUE)) + tiny
#    warning("Non-positive values found; adding offset = ", signif(offset, 6), " before log.")
#    x_num <- x_num + offset
#  }
#  return(log(x_num))
#}

# apply log to non-rate variables (excluding date)
# (var in names(df)) {
#  if (!(var %in% rate_variables) && var != "date") {
#    df[[var]] <- safe_log(df[[var]])
#  }
#}

# convert to growth rates (percent) for the same non-rate variables
for (var in names(df)) {
  if (!(var %in% rate_variables) && var != "date") {
    df[[var]] <-  (df[[var]] - dplyr::lag(df[[var]], 1))
  }
}

# remove initial rows with NA introduced by growth calculation if needed
# (subsequent code already removes rows with NA in gdp)

#check if stationary using functions from stationary.R
#for (var in setdiff(colnames(df), "date")) {  # excluding date column
#  cat("Checking stationarity for:", var, "\n")
#  ts_data <- ts(df[[var]])
#  stationary_ts <- make_stationary(ts_data, use_log = TRUE)   # returns aligned vector (same length)
#  df[[var]] <- as.numeric(stationary_ts)                     # assign directly, no extra NA
#}
df <- df %>% filter(!is.na(gdp))  # remove first row with NA

# ---------------------- lags ---------------------#

# possible lags: 1, 4 (one year), 8 (two years)
lags <- c(1,4)

# ------------------ selected variables ------------------#

#selected variables for BVAR model

selected_variables_0 <- c("gdp", "inflation", "wkfreuro")
#"consp"
selected_variables_1 <- c("gdp", "inflation", "wkfreuro", "consg", 
                          #"ifix", "icnstr", #"ime", #"exc1", #"imc1", 
                          "ltot", #"uroff",
                          "wage", 
                          #"srate",
                          "poilusd", "pcioecd")#, #"vaabcde", #"vaghji")

#from this initial set, we will also try a smaller set of variables

#selected_variables_2 <- c("gdp", "inflation", "wkfreuro", "consg", "ifix", 
#                          "exc1", "imc1", "ltot", "uroff", "wage", #"srate", 
#                          "poilusd", "pcioecd")

#selected_variables_3 <- c("gdp", "inflation", "wkfreuro", "exc1", "ltot",
#                         "wage")#, "srate")


# just for checkig the variables
#for (var in selected_variables_1) {
#  plot_data <- data.frame(
#    date = df$date,
#    value = df[[var]]
#  )
#  
#  p <- ggplot(plot_data, aes(x = date, y = value)) +
#    geom_line() +
#    labs(title = paste("Time Series of", var),
#         y = var) +
#    theme_bw()
#  print(p)
#}
#-------------------------------------------------------------------
# List of variable sets to test
variable_sets <- list(
  set_0 = selected_variables_0,
  set_1 = selected_variables_1#,
  #set_2 = selected_variables_2,
  #set_3 = selected_variables_3
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
  hyper = c("lambda", "alpha"),
  mn = mn,
  soc = soc
)

# Lista di prior pronta per essere passata a BVAR::bvar
prior_configs <- list(
  mn      = mn_prior_bv,
  soc     = soc_prior_bv,
  diffuse = diffuse_prior_bv
)

rolling_window_size <- 110  # e.g., 40 quarters (10 years)

#---------------------- end of setup ---------------------#

compute_bvar_rmse <- function(data, variables, lags, prior, window_size){
  
  horizon <- 1
  n_obs <- nrow(data)

  forecast_variables <- c("gdp", "inflation", "wkfreuro")

  pred_q50 <- matrix(NA_real_, nrow = n_obs, ncol = length(variables),
                     dimnames = list(NULL, variables))
  
  for (i in seq(from = window_size + lags, to = n_obs - horizon)) {
    
    train_start <- i - window_size + 1
    train_end <- i
    
    # select the data for the rolling window
    y_train <- data[train_start:train_end, ]
    
    # fitting model
    invisible(trained_model <- bvar(
      y_train %>% dplyr::select(all_of(variables)),
      lags = lags,
      n_draw = 10000,
      n_burn = 2500,
      n_thin = 1,
      priors = prior,
      verbose = FALSE
    ))
    
    prediction <- predict(trained_model, horizon = horizon)
    
    pred_q50[i + horizon, ] <- prediction$quants["50%", 1, ]
  }

  # create a data frame with results (one column for variable, one for rmse one
  res <- data.frame(
    variable = forecast_variables,
    rmse = numeric(length(forecast_variables))
  )
  
  for (i in seq_along(forecast_variables)) {
    
    var <- forecast_variables[i]

    cat(var, ": rmse: " )
    valid_indices <- which(!is.na(pred_q50[, var]))
    rmse <- sqrt(mean((pred_q50[valid_indices, var] - data[valid_indices, var])^2))
    mae <- mean(abs(pred_q50[valid_indices, var] - data[valid_indices, var]))
    cat(rmse, "\n")
    
    res$rmse[i] <- rmse
  }
  
  for (i in seq_along(forecast_variables)) {
    
    var <- forecast_variables[i]

    plot_data <- data.frame(
      date = data$date,
      actual = data[[var]],
      forecast = pred_q50[, var]
    )
    
    p <- ggplot(plot_data, aes(x = date)) +
      geom_line(aes(y = actual, color = "Actual")) +
      geom_line(aes(y = forecast, color = "Forecast")) +
      labs(title = paste("BVAR Forecast vs Actual for", var),
           y = var,
           color = "Legend") +
      theme_bw()
    print(p)
  }
  
  return(res)
}

#–-------------------- BVAR/VAR RMSE computation ---------------------#

results_df <- data.frame(
  variable = character(),
  rmse = numeric(),
  lags = integer(),
  variable_set = character(),
  prior = character(),
  stringsAsFactors = FALSE
)

for (lag in lags) {
 
  for (var_set_name in names(variable_sets)) {

    selected_vars <- variable_sets[[var_set_name]]
    
    for (prior_name in names(prior_configs)) {
      cat("testing: ", lag, " ", var_set_name, " ",prior_name, "\n")
      prior_config <- prior_configs[[prior_name]]
      
      temp_results <- compute_bvar_rmse(
        data = df,
        variables = selected_vars,
        lags = lag,
        prior = prior_config,
        window_size = rolling_window_size
      )
      temp_results$lags <- lag
      temp_results$variable_set <- var_set_name
      temp_results$prior <- prior_name
      results_df <- rbind(results_df, temp_results)
    }
  }
}

results_df

# save results to the output folder
save(results_df, file = "output/results_df.RData")

