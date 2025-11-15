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
output_folder <- "output/plots/main"
# --------------------- data preparation -----------------#

# import data
df <- utils::read.csv("data/data_quarterly.csv")

window_size <- 40
horizon <- 1
lag_number <- 1

#selected variables for BVAR model
# in rmse.R we justify this choice (best results)
selected_variables <- c("gdp", "inflation", "wkfreuro")

#format date
df$date <- as.Date(paste0(df$date, "-01")) # format date

# filter data until 2019-01-01 for training
df <- df %>% filter(date <= as.Date("2019-01-01")) 

# compute inflation rate and remove first NA row
df$inflation <- ((df$cpi - dplyr::lag(df$cpi, 1)) / dplyr::lag(df$cpi, 1) ) * 100
df <- df %>% filter(!is.na(inflation)) 


# ----------------------  transformations ---------------------#
rate_variables <- c("inflation", "urilo", "srate", "srate_ge")
forecast_variables <- c("gdp", "inflation", "wkfreuro")

# -------------------- apply log + growth transformations --------------------
for (var in names(df)) {
  if (!(var %in% rate_variables) && var != "date") {
    #log difference  
    df[[var]] <-  (log(df[[var]]) - log(dplyr::lag(df[[var]], 1))) *100
  }
}

#remove first row with NA
df <- df %>% filter(!is.na(gdp)) 

# ---------------------- exploratory plots ---------------------#
# plot with the timeseries of the forecast variables 
df_long <- df %>%
  dplyr::select(date, all_of(forecast_variables)) %>%
  pivot_longer(-date, names_to = "variable", values_to = "rate")

ggplot(df_long, aes(x = date, y = rate, color = variable)) +
  geom_line() +
  labs(title = "Forecast Variables", x = "Date", y = "rate")+
  theme_bw()

# save plot
ggsave(paste0(output_folder, "/forecast_variables_timeseries.png"), width = 8, height = 6, dpi = 300)

# ------------------------------ correlation matrix ---------------------#
# useless for the forecast byt interesitng to see 
correlations <- df %>%
  dplyr::select(all_of(selected_variables)) %>%
  cor()
print("Correlations between selected variables:\n")
print(correlations) 

#correlation matrix plot
correlation_matrix <- as.data.frame(as.table(correlations))
ggplot(correlation_matrix, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       limit = c(-1, 1), name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed() +
  labs(title = "Correlation Matrix of Selected Variables", x = "", y = "")

ggsave(paste0(output_folder, "/correlation_matrix.png"), width = 8, height = 6, dpi = 300)

# ---------------------- priors parameters ---------------------#
mn_mode <- 0.4
mn_sd <- 0.5
mn_min <- 0.001
mn_max <- 5

soc_mode <- 1
soc_sd <- 0.5


# --------------------  BVAR ----------------------
# --------------------rolling window forecast --------------------------------------
# settings
data <- df
n_obs <- nrow(data)

# for each quantity we build a matrix to save results
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
  
# set priors 
mn <- bv_minnesota(
  lambda = bv_lambda(mode = mn_mode, sd = mn_sd, min = mn_min, max = mn_max),
  alpha  = bv_alpha(mode = 3),
)

soc <- bv_soc(mode = soc_mode, sd = soc_sd)  

#  combination of priors
# minnesota regularizes the autoregressive coefficients, reducing the risk of overfitting.
# soc  imposes a constraint on the sum of the coefficients, ensuring the model captures persistence
priors <- bv_priors(
  hyper = c("lambda", "alpha", "psi"),
  mn = mn,
  soc = soc
)

# ------------------------- rolling window -----------------------------
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
      aes(x = date, ymin = lower2, ymax = upper2, fill = "95% CI"),
      alpha = 0.2
    ) +
    geom_ribbon(
      data = df_forecast,
      aes(x = date, ymin = lower1, ymax = upper1, fill = "68% CI"),
      alpha = 0.4
    ) +
    geom_line(
      data = df_actual,
      aes(x = date, y = actual, color = "Actual"),
      size = 0.9
    ) +
    geom_line(
      data = df_forecast, 
      aes(x = date, y = predicted, color = "Forecast"),
      size = 1
    ) +
    labs(
      title = paste("Forecast for", var),
      x = "Date",
      y = var,
      fill = "Confidence Interval",
      color = "Legend"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  plots[[var]] <- plot
  # save individual plot
  ggsave(paste0(output_folder, "/bvar_rolling_w", var, ".png"), 
         plot = plot, width = 8, height = 6, dpi = 300)
}

# plot plots together
combined_plot <- wrap_plots(plots, ncol = 1)
combined_plot

# save plot
ggsave(paste0(output_folder, "/bvar_rolling_w_combined.png"), 
       plot = combined_plot, width = 10, height = 12, dpi = 300)


#--------------------- end of BVAR rolling window forecast --------------------
#------------------------------------------------------------------------------
# in the next section, we compare BVAR with VAR and AR models
# -------------------- VAR model (OLS) rolling window forecast ----------------

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

# evaluation
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
    geom_line(aes(y = actual, color = "Actual"), linetype = "dashed", size = 0.9) +
    geom_line(aes(y = predicted_bvar, color = "BVAR Forecast"), size = 1) +
    geom_line(aes(y = predicted_var, color = "VAR Forecast"), size = 1) +
    labs(
      title = paste("Forecast Comparison for", var, "| BVAR vs VAR"),
      x = "Date",
      y = var,
      color = "Legend"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom"
    )
  print(plot)
  # save individual plot
  ggsave(paste0(output_folder, "/var_vs_bvar_rolling_w_",var, ".png"), 
         plot = plot, width = 8, height = 6, dpi = 300) 

}
#------------------------------------------------------------------------------

# for each of forecast variables AR(1) model as a benchmark

# allocate containers for AR(1) quantile forecasts
pred_q50_ar1  <- matrix(NA_real_, nrow = n_obs, ncol = length(forecast_variables),
                        dimnames = list(NULL, forecast_variables))
pred_q16_ar1  <- pred_q50_ar1
pred_q84_ar1  <- pred_q50_ar1
pred_q025_ar1 <- pred_q50_ar1
pred_q975_ar1 <- pred_q50_ar1

# rolling window forecast with AR(1)
for (var in forecast_variables) {
  for (i in seq(from = window_size + 1, to = n_obs - horizon)) {
    # rolling window
    train_start <- i - window_size + 1
    train_end   <- i
    y_train <- data[train_start:train_end, var, drop = TRUE]
    
    ar_model <- Arima(y_train, order = c(1, 0, 0), include.constant = TRUE)
    fc <- forecast(ar_model, h = horizon, level = c(68, 95))
    
    # extract quantiles
    idx68 <- match(68, fc$level)
    idx95 <- match(95, fc$level)
    
    #save forecasts
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
      aes(x = date, ymin = lower2, ymax = upper2, fill = "95% CI"),
      alpha = 0.2
    ) +
    geom_ribbon(
      data = df_forecast,
      aes(x = date, ymin = lower1, ymax = upper1, fill = "68% CI"),
      alpha = 0.4
    ) +
    geom_line(
      data = df_actual,
      aes(x = date, y = actual, color = "Actual"),
      size = 0.9
    ) +
    geom_line(
      data = df_forecast, 
      aes(x = date, y = predicted, color = "Forecast"),
      size = 1
    ) +
    labs(
      title = paste("Forecast for", var),
      x = "Date",
      y = var,
      fill = "Confidence Interval",
      color = "Legend"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  plots[[var]] <- plot

  # save individual plot
  ggsave(paste0(output_folder, "/ar1_rolling_w_", var, ".png"), 
         plot = plot, width = 8, height = 6, dpi = 300)
}

# plot plots together
combined_plot <- wrap_plots(plots, ncol = 1)
# save plot
ggsave(paste0(output_folder, "/ar1_rolling_w_combined.png"), 
       plot = combined_plot, width = 10, height = 12, dpi = 300)


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
write.csv(results, file = "output/rmse_comparison_horizon1.csv", row.names = FALSE)


### ---------- combined predictions -----------------------------
# Combine predictions into one data frame
combined_predictions <- data.frame(
  date = df$date,  # Use the original dates
  actual_gdp = data$gdp,
  actual_inflation = data$inflation,
  actual_wkfreuro = data$wkfreuro,
  bvar_gdp = pred_q50[, "gdp"],
  bvar_inflation = pred_q50[, "inflation"],
  bvar_wkfreuro = pred_q50[, "wkfreuro"],
  var_gdp = pred_q50_var[, "gdp"],
  var_inflation = pred_q50_var[, "inflation"],
  var_wkfreuro = pred_q50_var[, "wkfreuro"],
  ar1_gdp = pred_q50_ar1[, "gdp"],
  ar1_inflation = pred_q50_ar1[, "inflation"],
  ar1_wkfreuro = pred_q50_ar1[, "wkfreuro"]
)

# Remove rows with all NA predictions (e.g., before the rolling window starts)
combined_predictions <- combined_predictions %>%
  filter(!is.na(bvar_gdp) | !is.na(var_gdp) | !is.na(ar1_gdp))

# Save the combined data frame as a CSV
write.csv(combined_predictions, file = "output/combined_predictions.csv", row.names = FALSE)