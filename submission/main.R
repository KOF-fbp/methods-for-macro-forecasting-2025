renv::status()

#--------------------- libraries ---------------------#
library(ggplot2)
library(dplyr)
library(tidyr)
library(BVAR)
library(lubridate)
library(vars)  
library(forecast)
library(tseries)

# --------------------- set up ------------------------#
# set seed for reproducibility
set.seed(42)

# settings

# load data and data manipulation
df <- utils::read.csv("data/data_quarterly.csv")

window_size <- 40
horizon <- 1
lag_number <- 1
selected_variables <- c("gdp", "inflation", "wkfreuro")
#selected_variables<- c("gdp", "inflation", "wkfreuro", "consg", "ifix", 
#                                                "exc1", "imc1", "ltot", "uroff", "wage", "srate", 
#                                                "poilusd", "pcioecd")
#selected_variables <- c("gdp", "inflation", "wkfreuro", "consg", 
#                         "wage", "constot", "cpi", "domdem", "exc1",
#                        "pime", "pimtot1", "unempoff", "urilo", "srate", 
#                        "srate_ge", "wd",
#                          "poilusd", "pcioecd")


df$date <- as.Date(paste0(df$date, "-01")) # format date
df <- df %>% filter(date <= as.Date("2019-01-01")) # until 01.10.2025
df <- df %>% filter(date <= as.Date("2025-10-01")) 
# inflate CPI to get inflation rate
df$inflation <- ((df$cpi - dplyr::lag(df$cpi, 1)) / dplyr::lag(df$cpi, 1) ) * 100
df <- df %>% filter(!is.na(inflation)) #remove first NA row


# gdp growth instead of gdp
#df$gdp <- log(df$gdp)
# -------------------- apply log + growth transformations --------------------

rate_variables <- c("inflation", "urilo", "srate", "srate_ge")
forecast_variables <- c("gdp", "inflation", "wkfreuro")

#plot the forecast variables
par(mfrow=c(3,1))
for (var in forecast_variables) {
  plot(df$date, df[[var]], type='l', main=var, xlab='Date', ylab=var)
}

# difference in difference
for (var in names(df)) {
  if (!(var %in% rate_variables) && var != "date") {
    df[[var]] <-  (log(df[[var]]) - log(dplyr::lag(df[[var]], 1))) *100
  }
}

df <- df %>% filter(!is.na(gdp))  # remove first row with NA
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
  dplyr::select(all_of(selected_variables)) %>%
  cor()
print(correlations) 

# BVAR models for exploration --------------------------------------------------

# to better understand how the bvar models and priors work

## first simple BVAR model for exploration ---------

df <- df[, colSums(is.na(df)) == 0]

if (FALSE){
  
  # BVAR with Minnisota Prior and sum of coefficients
  
  # last 80 data points
  window_data <- df[(nrow(df)-79):nrow(df), ]
  
  mn <- bv_minnesota(
    lambda = bv_lambda(mode = 0.4, sd = 0.5, min = 0.001, max = 5),
    alpha  = bv_alpha(mode = 4),
  )
  
  soc <- bv_soc(mode = 1, sd = 0.5)   # shrink sum of AR coeffs to 1, with variance
  
  priors <- bv_priors(
    hyper = c("lambda", "alpha", "psi"),
    mn = mn,
    soc = soc
  )
  model <- bvar(
    window_data%>% dplyr::select(all_of(forecast_variables)),
    lags = 8,
    n_draw = 100000,
    n_burn = 25000,
    n_thin = 1,
    priors = priors,
    verbose = TRUE
  )
  plot(model)
  
  prediction_1 <- predict(model, horizon = 1, conf_bands = c(0.16, 0.025))
  prediction_4 <- predict(model, horizon = 4, conf_bands = c(0.16, 0.025))

  plot(predict(model, horizon = 1, conf_bands = c(0.16, 0.025)), area = TRUE, t_back = 10,
        vars = c("gdp", "inflation", "wkfreuro"))
  plot(predict(model, horizon = 4, conf_bands = c(0.16, 0.025)), area = TRUE, t_back = 10, orientation = "v",
       vars = c("gdp", "inflation", "wkfreuro"), col = "transparent")
  
  horizon <- 1
  y_train <- window_data
  trained_model <- bvar(
    y_train %>% dplyr::select(all_of(selected_variables)),
    lags = lag_number,
    n_draw = 10000,
    n_burn = 2500,
    n_thin = 1,
    priors = priors,
    verbose = FALSE
  )
  
  
  horizon <- 4
  y_train <- window_data
  
  trained_model <- bvar(
    y_train %>% dplyr::select(all_of(selected_variables)),
    lags = lag_number,
    n_draw = 10000,
    n_burn = 2500,
    n_thin = 1,
    priors = priors,
    verbose = FALSE
  )
  
  # --- setup sizes --------------------------------------------------------------
  n_obs <- nrow(df) + horizon  # FIX: allow full horizon
  
  pred_q50  <- matrix(NA_real_, nrow = n_obs, ncol = length(selected_variables),
                      dimnames = list(NULL, selected_variables))
  pred_q16  <- matrix(NA_real_, nrow = n_obs, ncol = length(selected_variables),
                      dimnames = list(NULL, selected_variables))
  pred_q84  <- matrix(NA_real_, nrow = n_obs, ncol = length(selected_variables),
                      dimnames = list(NULL, selected_variables))
  pred_q025 <- matrix(NA_real_, nrow = n_obs, ncol = length(selected_variables),
                      dimnames = list(NULL, selected_variables))
  pred_q975 <- matrix(NA_real_, nrow = n_obs, ncol = length(selected_variables),
                      dimnames = list(NULL, selected_variables))
  
  # --- model prediction ---------------------------------------------------------
  prediction <- predict(trained_model, horizon = horizon, conf_bands = c(0.16, 0.025))
  
  for (h in 1:horizon) {
    row_idx <- nrow(df) + h
    pred_q50[row_idx,  ] <- prediction$quants["50%",  h, ]
    pred_q16[row_idx,  ] <- prediction$quants["16%",  h, ]
    pred_q84[row_idx,  ] <- prediction$quants["84%",  h, ]
    pred_q025[row_idx, ] <- prediction$quants["2.5%", h, ]
    pred_q975[row_idx, ] <- prediction$quants["97.5%",h, ]
  }
  
  # --- build a proper future date index ----------------------------------------
  # assume df$date is regular Date/POSIXct; infer step from the last two points
  step <- diff(tail(df$date, 2))[1]
  if (is.na(step) || step <= 0) step <- stats::median(diff(df$date))
  future_dates <- seq(from = max(df$date) + step, by = step, length.out = horizon)
  date_all <- c(df$date, future_dates)
  
  # --- plotting ----------------------------------------------------------------
  plots <- list()
  forecast_start <- nrow(df) + 1
  
  for (var in selected_variables) {
    # actuals window (last ~20 points before forecast)
    start_slice <- max(1, forecast_start - 4)
    df_actual <- data.frame(date = df$date, actual = df[[var]]) |>
      dplyr::slice(start_slice:dplyr::n())
    
    # future forecasts + a stitch row at the last actual date
    keep <- (forecast_start - 1):n_obs
    df_forecast <- data.frame(
      date      = date_all[keep],
      predicted = pred_q50[ keep, var],
      lower1    = pred_q16[ keep, var],
      upper1    = pred_q84[ keep, var],
      lower2    = pred_q025[keep, var],
      upper2    = pred_q975[keep, var]
    )
    
    # after this line (which you already have)
    df_forecast[1, c("predicted","lower1","upper1","lower2","upper2")] <- df[[var]][forecast_start - 1]
    
    # USE the full df_forecast (not [-1, ]) so the first row collapses to a point
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
  
  combined_plot <- patchwork::wrap_plots(plots, ncol = 1)
  combined_plot
}

# rolling window forecast --------------------------------------

# settings
data <- df
n_obs <- nrow(data)
# building a matrix to save results of q50
pred_q50 <- matrix(NA_real_, nrow = n_obs, ncol = length(selected_variables),
                   dimnames = list(NULL, selected_variables))
# building a matrix to save results of q16
pred_q16 <- matrix(NA_real_, nrow = n_obs, ncol = length(selected_variables),
                   dimnames = list(NULL, selected_variables))
# building a matrix to save results of q84
pred_q84 <- matrix(NA_real_, nrow = n_obs, ncol = length(selected_variables),
                   dimnames = list(NULL, selected_variables))
# building a matrix to save results of q025
pred_q025 <- matrix(NA_real_, nrow = n_obs, ncol = length(selected_variables),
                   dimnames = list(NULL, selected_variables))
# building a matrix to save results of q975
pred_q975 <- matrix(NA_real_, nrow = n_obs, ncol = length(selected_variables),
                   dimnames = list(NULL, selected_variables))

# check all for stationarity

for (var in selected_variables) {
  adf_test <- tseries::adf.test(data[[var]], alternative = "stationary")
  if (adf_test$p.value < 0.05) { cat("Variable", var, "is stationary (p-value:", adf_test$p.value, ")\n")} 
  else {  cat("Variable", var, "is non-stationary (p-value:", adf_test$p.value, ")\n")}
}
  
# set priors -------------
mn <- bv_minnesota(
  lambda = bv_lambda(mode = 0.5, sd = 1, min = 0.001, max = 5),
  alpha  = bv_alpha(mode = 3),
)

soc <- bv_soc(mode = 1, sd = 0.5)   # shrink sum of AR coeffs to 1, with variance 1

priors <- bv_priors(
  hyper = c("lambda", "alpha", "psi"),
  mn = mn,
  soc = soc
)

# rolling window ---------
for (i in seq(from = window_size + lag_number, to = n_obs - horizon)) {
  print(i)
  train_start <- i - window_size + 1
  train_end <- i
  
  # select the data for the rolling window
  y_train <- data[train_start:train_end, ]
  
  # fitting model
  trained_model <- bvar(
    y_train %>% dplyr::select(all_of(selected_variables)),
    lags = lag_number,
    n_draw = 10000,
    n_burn = 2500,
    n_thin = 1,
    priors = priors,
    verbose = FALSE
  )
  
  prediction <- predict(trained_model, horizon = horizon, conf_bands = c(0.16, 0.025))
  pred_q50[i+horizon, ] <- prediction$quants["50%", horizon,]
  pred_q16[i+horizon, ] <- prediction$quants["16%", horizon,]
  pred_q84[i+horizon, ] <- prediction$quants["84%",horizon,]
  pred_q025[i+horizon, ] <- prediction$quants["2.5%", horizon,]
  pred_q975[i+horizon, ] <- prediction$quants["97.5%", horizon,]
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

# plot plots together
combined_plot <- wrap_plots(plots, ncol = 1)
combined_plot


# Diffusion Prior ------------------

# matrix for results (point forecasts only)
pred_q50_var <- matrix(NA_real_, nrow = n_obs, ncol = length(selected_variables),
                   dimnames = list(NULL, selected_variables))

# rolling window
for (i in seq(from = window_size + lag_number, to = n_obs - horizon)) {

  train_start <- i - window_size + 1
  train_end   <- i
  
  y_train <- data[train_start:train_end, selected_variables]
  
  # estimate OLS VAR
  trained_model <- VAR(
    y_train,
    p    = lag_number,
    type = "const"
  )
  
  # forecast (point only)
  prediction <- predict(trained_model, n.ahead = horizon, ci = 0.95)
  
  for (j in seq_along(selected_variables)) {
    varname <- selected_variables[j]
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

# plot VAR (diffusion Prior) vs actual vs BVAR forecasts

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
      title = paste("Forecast Comparison for", var, "| Selected vs Diffusion Prior (OLS)"),
      x     = "Date", y     = var) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
  print(plot)
}

# for each of forecast variables AR(1) model as a benchmark

# allocate containers for AR(1) quantile forecasts
pred_q50_ar1  <- matrix(NA_real_, nrow = n_obs, ncol = length(forecast_variables),
                        dimnames = list(NULL, forecast_variables))
pred_q16_ar1  <- pred_q50_ar1
pred_q84_ar1  <- pred_q50_ar1
pred_q025_ar1 <- pred_q50_ar1
pred_q975_ar1 <- pred_q50_ar1

for (var in forecast_variables) {
  for (i in seq(from = window_size + 1, to = n_obs - horizon)) {
    # rolling window
    train_start <- i - window_size + 1
    train_end   <- i
    y_train <- data[train_start:train_end, var, drop = TRUE]
    
    ar_model <- Arima(y_train, order = c(1, 0, 0), include.constant = TRUE)
    fc <- forecast(ar_model, h = horizon, level = c(68, 95))
    
    idx68 <- match(68, fc$level)
    idx95 <- match(95, fc$level)
    
    t_idx <- i + horizon
    pred_q50_ar1[t_idx, var]  <- as.numeric(fc$mean[horizon])     # median ≈ mean for Gaussian ARIMA
    pred_q16_ar1[t_idx, var]  <- fc$lower[horizon, idx68]         # ~16th
    pred_q84_ar1[t_idx, var]  <- fc$upper[horizon, idx68]         # ~84th
    pred_q025_ar1[t_idx, var] <- fc$lower[horizon, idx95]         # 2.5th
    pred_q975_ar1[t_idx, var] <- fc$upper[horizon, idx95]         # 97.5th
  }
}

# evaluation
for (i in seq_along(forecast_variables)) {
  var <- forecast_variables[i]
  cat("Evaluating variable:", var, "\n")
  cat("--------------------------------", "\n")
  valid_indices <- which(!is.na(pred_q50_ar1[, var]))
  rmse <- sqrt(mean((pred_q50_ar1[valid_indices, var] - data[valid_indices, var])^2))
  mae  <- mean(abs(pred_q50_ar1[valid_indices, var] - data[valid_indices, var]))
  cat("RMSE for", var, ":", rmse, "\n")
  cat("MAE for", var, ":", mae, "\n\n")
}

library(patchwork)
plots <- list()


for (var in forecast_variables) {
  
  df_forecast <- data.frame(
    date     = df$date,
    predicted = pred_q50_ar1[, var],
    lower1   = pred_q16_ar1[, var],  
    upper1   = pred_q84_ar1[, var],   
    lower2   = pred_q025_ar1[, var],  
    upper2   = pred_q975_ar1[, var]  
  ) %>% filter(!is.na(predicted))
  
  forecast_start <- min(which(!is.na(pred_q50_ar1[, var])))
  
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

# plot plots together
combined_plot <- wrap_plots(plots, ncol = 1)
combined_plot

# save the rmse for each model in a data frame
results <- data.frame(
  variable = forecast_variables,
  rmse_bvar = numeric(length(forecast_variables)),
  rmse_var  = numeric(length(forecast_variables)),
  rmse_ar1  = numeric(length(forecast_variables))
)
for (i in seq_along(forecast_variables)) {
  var <- forecast_variables[i]
  valid_indices_bvar <- which(!is.na(pred_q50[, var]))
  valid_indices_var  <- which(!is.na(pred_q50_var[, var]))
  valid_indices_ar1  <- which(!is.na(pred_q50_ar1[, var]))
  
  rmse_bvar <- sqrt(mean((pred_q50[valid_indices_bvar, var] - data[valid_indices_bvar, var])^2))
  rmse_var  <- sqrt(mean((pred_q50_var[valid_indices_var, var] - data[valid_indices_var, var])^2))
  rmse_ar1  <- sqrt(mean((pred_q50_ar1[valid_indices_ar1, var] - data[valid_indices_ar1, var])^2))
  
  results$rmse_bvar[i] <- rmse_bvar
  results$rmse_var[i]  <- rmse_var
  results$rmse_ar1[i]  <- rmse_ar1
}
print(results)

saveRDS(results, file = "output/rmse_comparison_horizon1.rds")

