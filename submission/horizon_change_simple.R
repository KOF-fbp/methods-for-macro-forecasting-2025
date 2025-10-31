renv::status()
# Example R script to test the Docker setup
library(ggplot2)
library(dplyr)
library(tidyr)
library(BVAR)
library(lubridate)
library(vars)  # needed for classical VAR benchmark

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

# convert to growth rates (percent) for the same non-rate variables
for (var in names(df)) {
  if (!(var %in% rate_variables) && var != "date") {
    df[[var]] <-  (df[[var]] - dplyr::lag(df[[var]], 1))
  }
}

df <- df %>% filter(!is.na(gdp)) 
# plot with the timeseries of the forecast variables 

df_long <- df %>%
  dplyr::select(date, all_of(forecast_variables)) %>%
  pivot_longer(-date, names_to = "variable", values_to = "rate")
ggplot(df_long, aes(x = date, y = rate, color = variable)) +
  geom_line() +
  labs(title = "Forecast Variables", x = "Date", y = "rate")+
  theme_bw()

# get the correlation between variables (simply interessting to see)
correlations <- df %>%
  dplyr::select(all_of(forecast_variables)) %>%
  cor()
print(correlations) 

# BVAR models for exploration --------------------------------------------------

# to better understand how the bvar models and priors work

## first simple BVAR model for exploration ---------

df <- df[, colSums(is.na(df)) == 0]

model <- bvar(
  df%>% dplyr::select(all_of(forecast_variables)),
  lags = 1,
  n_draw = 100000,
  n_burn = 2500,
  n_thin = 1 #thinning
) 

plot(model)

## BVAR with minisota prior for exploration ---------

mn <- bv_minnesota(
  lambda = bv_lambda(mode = 0.5, sd = 0.1, min = 0.001, max = 5),
  alpha  = bv_alpha(mode = 4),
)

priors <- bv_priors(hyper = c("lambda", "alpha"), mn = mn)

model <- bvar(
  df%>% dplyr::select(all_of(forecast_variables)),
  lags = 12,
  n_draw = 100000,
  n_burn = 25000,
  n_thin = 1,
  priors = priors,
  verbose = TRUE
)

p <- plot(model)
print(p)

## BVAR with Minnesota prior and sum-of-coefficients prior ---------

mn <- bv_minnesota(
  lambda = bv_lambda(mode = 0.2, sd = 0.1, min = 0.001, max = 5),
  alpha  = bv_alpha(mode = 4),
)
soc <- bv_soc(mode = 1, sd = 0.5)   # shrink sum of AR coeffs to 1, with variance
priors <- bv_priors(
  hyper = c("lambda", "alpha", "psi"),
  mn = mn,
  soc = soc
)
model <- bvar(
  df%>% dplyr::select(all_of(forecast_variables)),
  lags = 8,
  n_draw = 100000,
  n_burn = 25000,
  n_thin = 1,
  priors = priors,
  verbose = TRUE
)
plot(model)


# rolling window forecast --------------------------------------

# settings
data <- df
window_size <- 110
horizon <- 1
n_obs <- nrow(data)
lag_number <- 8

# building a matrix to save results of q50
pred_q50 <- matrix(NA_real_, nrow = n_obs, ncol = length(forecast_variables),
                   dimnames = list(NULL, forecast_variables))
# building a matrix to save results of q16
pred_q16 <- matrix(NA_real_, nrow = n_obs, ncol = length(forecast_variables),
                   dimnames = list(NULL, forecast_variables))
# building a matrix to save results of q84
pred_q84 <- matrix(NA_real_, nrow = n_obs, ncol = length(forecast_variables),
                   dimnames = list(NULL, forecast_variables))
# building a matrix to save results of q025
pred_q025 <- matrix(NA_real_, nrow = n_obs, ncol = length(forecast_variables),
                    dimnames = list(NULL, forecast_variables))
# building a matrix to save results of q975
pred_q975 <- matrix(NA_real_, nrow = n_obs, ncol = length(forecast_variables),
                    dimnames = list(NULL, forecast_variables))



# check all for stationarity
library(tseries)
for (var in selected_variables) {
  # check each variable for stationarity
  adf_test <- tseries::adf.test(data[[var]], alternative = "stationary")
  if (adf_test$p.value < 0.05) {
    cat("Variable", var, "is stationary (p-value:", adf_test$p.value, ")\n")
  } else {
    cat("Variable", var, "is non-stationary (p-value:", adf_test$p.value, ")\n")
  }
}


quantile_bands <- c("2.5%", "16%", "50%", "84%", "97.5%")

# set priors -------------
mn <- bv_minnesota(
  lambda = bv_lambda(mode = 1, sd = 1, min = 0.001, max = 5),
  alpha  = bv_alpha(mode = 3, sd = 1),
)
priors <- bv_priors(hyper = c("lambda", "alpha", "psi"), mn = mn)

soc <- bv_soc(mode = 1, sd = 0.4)   # shrink sum of AR coeffs to 1, with variance

priors <- bv_priors(
  hyper = c("lambda", "alpha", "psi"),
  mn = mn,
  soc = soc
  #sur = bv_sur(mode = 2, sd = 0.5, min = 0.0001, max = 50)
)

#bv_met <- bv_metropolis(
#  scale_hess = 0.01,
#  adjust_acc = FALSE,
#  adjust_burn = 0.75,
#  acc_lower = 0.25,
#  acc_upper = 0.45,
#  acc_change = 0.01
#)


# rolling window ---------
for (i in seq(from = window_size + lag_number, to = n_obs - horizon)) {
  print(i)
  train_start <- i - window_size + 1
  train_end <- i
  
  # select the data for the rolling window
  y_train <- data[train_start:train_end, ]
  
  # fitting model
  trained_model <- bvar(
    y_train %>% dplyr::select(all_of(forecast_variables)),
    lags = lag_number,
    n_draw = 10000,
    n_burn = 2500,
    n_thin = 1,
    priors = priors,
    # metropolis = bv_met,
  )
  
  prediction <- predict(trained_model, horizon = horizon, conf_bands = c(0.16, 0.025))
  
  # extract quantiles
  pred_quants <- prediction$quants
  vars <- forecast_variables
  mat <- matrix(NA_real_, nrow = length(vars), ncol = length(quantile_bands),
                dimnames = list(vars, quantile_bands))
  
  for (j in seq_along(vars)) {
    mat[j, ] <- pred_quants[, horizon, j]
  }
  
  # save the results
  results <- as_tibble(mat, rownames = "variable") %>%
    rename(
      q025 = `2.5%`,
      q16  = `16%`,
      q50  = `50%`,
      q84  = `84%`,
      q975 = `97.5%`
    )
  
  pred_q50[i+horizon, ] <- results$q50
  pred_q16[i+horizon, ] <- results$q16
  pred_q84[i+horizon, ] <- results$q84
  pred_q025[i+horizon, ] <- results$q025
  pred_q975[i+horizon, ] <- results$q975
}

for (i in seq_along(forecast_variables)) {
  var <- forecast_variables[i]
  cat("Evaluating variable:", var, "\n")
  cat("--------------------------------", "\n")
  
  valid_indices <- which(!is.na(pred_q50[, var]))
  rmse <- sqrt(mean((pred_q50[valid_indices, var] - data[valid_indices, var])^2))
  mae <- mean(abs(pred_q50[valid_indices, var] - data[valid_indices, var]))
  
  cat("RMSE for", var, ":", rmse, "\n")
  cat("MAE for", var, ":", mae, "\n", "\n")
  
}

# plot the forecasts of the different variables
library(patchwork)
plots <- list()


for (var in forecast_variables) {
  
  df_forecast <- data.frame(
    date     = df$date,
    predicted = pred_q50[, var],
    lower1   = pred_q16[, var],  
    upper1   = pred_q84[, var],   
    lower2   = pred_q025[, var],  
    upper2   = pred_q975[, var]  
  ) %>% filter(!is.na(predicted))
  
  forecast_start <- min(which(!is.na(pred_q50[, var])))
  
  df_actual <- data.frame(date = df$date, actual = data[, var]) %>%
    slice((forecast_start - 20):n())
  
  last_actual <- data.frame(
    date     = df$date[forecast_start - 1],
    predicted = data[forecast_start - 1, var],
    lower1    = data[forecast_start - 1, var],
    upper1    = data[forecast_start - 1, var],
    lower2    = data[forecast_start - 1, var],
    upper2    = data[forecast_start - 1, var]
  )
  df_forecast <- rbind(last_actual, df_forecast)
  
  plot <- ggplot() +
    geom_ribbon(
      data = df_forecast,
      aes(x = date, ymin = lower2, ymax = upper2),
      fill = "blue", alpha = 0.2
    ) +
    geom_ribbon(
      data = df_forecast,
      aes(x = date, ymin = lower1, ymax = upper1),
      fill = "lightblue", alpha = 0.4
    ) +
    geom_line(
      data = df_actual,
      aes(x = date, y = actual),
      color = "gray", size = 0.9
    ) +
    geom_line(
      data = df_forecast, 
      aes(x = date, y = predicted),
      color = "red", size = 1
    ) +
    labs(title = "", x = "Date", y = var) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      panel.grid.minor = element_blank()
    )
  
  plots[[var]] <- plot
}

# plot plots
combined_plot <- wrap_plots(plots, ncol = 1)
combined_plot


# benchmark using normal VAR ( not really a benchmark, var with defussion prior) ---------

library(vars)
window_size <- 110
horizon <- 1
n_obs <- nrow(data)
lag_number <- 1

# matrix for results (point forecasts only)
pred_q50_var <- matrix(NA_real_, nrow = n_obs, ncol = length(forecast_variables),
                       dimnames = list(NULL, forecast_variables))

# rolling window
for (i in seq(from = window_size + lag_number, to = n_obs - horizon)) {
  print(i)
  train_start <- i - window_size + 1
  train_end   <- i
  
  y_train <- data[train_start:train_end, forecast_variables]
  
  # --- estimate OLS VAR
  trained_model <- VAR(
    y_train,
    p    = lag_number,
    type = "const"
  )
  
  # --- forecast (point only)
  prediction <- predict(trained_model, n.ahead = horizon, ci = 0.95)
  
  for (j in seq_along(forecast_variables)) {
    varname <- forecast_variables[j]
    fcst    <- prediction$fcst[[varname]]
    
    pred_q50_var[i + horizon, varname] <- fcst[horizon, "fcst"]
  }
}

# --- evaluation
for (i in seq_along(forecast_variables)) {
  var <- forecast_variables[i]
  cat("Evaluating variable:", var, "\n")
  cat("--------------------------------", "\n")
  
  valid_indices <- which(!is.na(pred_q50_var[, var]))
  rmse <- sqrt(mean((pred_q50_var[valid_indices, var] - data[valid_indices, var])^2))
  mae  <- mean(abs(pred_q50_var[valid_indices, var] - data[valid_indices, var]))
  
  cat("RMSE for", var, ":", rmse, "\n")
  cat("MAE for", var, ":", mae, "\n\n")
}

# plot vs actual vs BVAR forecasts

for (var in forecast_variables) {
  df_plot <- data.frame(
    date      = df$date,
    actual    = data[, var],
    predicted_bvar = pred_q50[, var],
    predicted_var  = pred_q50_var[, var]
  ) %>% filter(!is.na(predicted_bvar) & !is.na(predicted_var))
  
  plot <- ggplot(df_plot, aes(x = date)) +
    geom_line(aes(y = actual), color = "navy", linetype = "dashed", size = 0.9) +
    geom_line(aes(y = predicted_bvar), color = "red", size = 1) +
    geom_line(aes(y = predicted_var), color = "green", size = 1) +
    labs(
      title = "",
      x     = "Date",
      y     = var
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      panel.grid.minor = element_blank()
    )
  
  print(plot)
}

