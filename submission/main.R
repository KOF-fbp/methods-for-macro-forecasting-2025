renv::status()
# Example R script to test the Docker setup
library(ggplot2)
library(dplyr)
library(tidyr)
library(BVAR)

# set seed 
set.seed(42)

df <- utils::read.csv("data/data_quarterly.csv")
head(df)

# date is in format YYYY-MM -> tranformation to date type
df$date <- as.Date(paste0(df$date, "-01"))

# create inflation variable from cpi data
df$inflation <- 100 * (log(df$cpi) - dplyr::lag(log(df$cpi), 1))

# remove first 4 rows
df <- df %>% filter(!is.na(inflation)) 

# define the variables to be forecasted
forecast_variables <- c("gdp", "inflation", "wkfreuro")

# define the rate variables
rate_variables <- c("inflation", "wkfreuro", "urilo", "srate", "srate_ge")

# log transform all variables which are not in rate_variables
for (var in names(df)) {
  if (!(var %in% rate_variables) & var != "date") {
    df[[var]] <- log(df[[var]])
  }
}
# to growth rate
for (var in names(df)) {
  if (!(var %in% rate_variables) & var != "date") {
    df[[var]] <- 100 * (df[[var]] - dplyr::lag(df[[var]], 1))
  }
}

# remove nans
df <- df %>% filter(!is.na(gdp))

# plot with the timeseries of the forecast variables 

# reshape to long format for easy plotting
df_long <- df %>%
  select(date, all_of(forecast_variables)) %>%
  pivot_longer(-date, names_to = "variable", values_to = "rate")

# plot
ggplot(df_long, aes(x = date, y = rate, color = variable)) +
  geom_line() +
  labs(title = "Forecast Variables", x = "Date", y = "rate")+
  theme_bw()

# get the correlation between variables

correlations <- df %>%
  select(all_of(forecast_variables)) %>%
  cor()

print(correlations) 
# -> inflation seems to influence / is influenced the exchange rate as well as gpd
# -> gdp has low correlation with exchange rate

# first simple BVAR model

data <- df %>% select(all_of(forecast_variables))
data <- as.matrix(data)
# first 50 rows
train_data <- data[1:50, ]
test_data <- data[51:nrow(data), ]

model <- bvar(
  data,
  lags = 1,
  n_draw = 10000,
  n_burn = 2500,
  n_thin = 1 #thinning
) 

# rolling forecast --------------------------------------

# settings
window_size <- 80
horizon <- 1
n_obs <- nrow(data)
lag_number <- 4

# building a matrix to save results of q50
pred_q50 <- matrix(NA_real_, nrow = n_obs, ncol = length(forecast_variables),
                   dimnames = list(NULL, forecast_variables))
# building a matrix to save results of q16
pred_q16 <- matrix(NA_real_, nrow = n_obs, ncol = length(forecast_variables),
                   dimnames = list(NULL, forecast_variables))
# building a matrix to save results of q84
pred_q84 <- matrix(NA_real_, nrow = n_obs, ncol = length(forecast_variables),
                   dimnames = list(NULL, forecast_variables))

for (i in seq(from = window_size + lags, to = n_obs - horizon)){
  
  print(i)
  train_start <- i - window_size + 1
  train_end <- i
  
  y_train <- data[train_start:train_end, ]
  
  trained_model <- bvar(
    y_train %>% select(all_of(forecast_variables)),
    lags = lag_number,
    n_draw = 10000,
    n_burn = 2500,
    n_thin = 1
  )
  
  y_true <- data[train_end + horizon, ]
  
  prediction <- predict(trained_model, horizon = horizon)
  
  # extract quantiles from the predictions
  pred_quants <- prediction$quants

  mat <- matrix(NA_real_, nrow = length(vars), ncol = length(quantile_bands),
                dimnames = list(vars, quantile_bands))
  
  for (j in seq_along(vars)) {
    mat[j, ] <- pred_quants[ , 1, j]  # all quantile bands, horizon=1, variable j
  }
  
  results <- as_tibble(mat, rownames = "variable") %>%
    rename(q16 = `16%`, q50 = `50%`, q84 = `84%`)
  
  pred_q50[i+1,] <- results$q50
  pred_q16[i+1,] <- results$q16
  pred_q84[i+1,] <- results$q84
  
}

# take the values from q50 that are not NA to calculate the RMSE and MAE
for (i in seq_along(forecast_variables)) {
  var <- forecast_variables[i]
  cat("Evaluating variable:", var, "\n")
  
  valid_indices <- which(!is.na(pred_q50[, var]))
  rmse <- sqrt(mean((pred_q50[valid_indices, var] - data[valid_indices, var])^2))
  mae <- mean(abs(pred_q50[valid_indices, var] - data[valid_indices, var]))
  
  cat("RMSE for", var, ":", rmse, "\n")
  cat("MAE for", var, ":", mae, "\n")
  
}

# plot the forecasts of the different variables

for (var in forecast_variables) {
  df_plot <- data.frame(
    date = df$date,
    actual = data[, var],
    predicted = pred_q50[, var],
    lower = pred_q16[, var],
    upper = pred_q84[, var]
  ) %>% filter(!is.na(predicted)) # to filter out the first x that are not used due to the rolling window
  
  plot <- ggplot(df_plot, aes(x = date)) +
    geom_line(aes(y = actual, color = "Actual")) +
    geom_line(aes(y = predicted, color = "Predicted")) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) +
    labs(title = paste("Forecast vs Actual for", var),
         y = var,
         color = "Legend") +
    theme_bw()
  
  print(plot)
}

plot(model)
summary(model)

# model 2 with more lags

model2 <- bvar(
  df %>% select(all_of(forecast_variables)),
  lags = 12,
  n_draw = 10000,
  n_burn = 2500,
  n_thin = 1 #thinning
) 

plot(model2)
summary(model2)


predict(model2) <- predict(model2, 
                          horizon = 4 ,
                          conf_bands = c(0.05, 0.16)
)
irf(model) <- irf(
  model2,
  horizon = 4,
  identification = TRUE
)

# Plot forecasts and impulse responses
plot(predict(model2))
plot(irf(model), vars_impulse = c("inflation"),
     vars_response = c("gdp", "inflation", "wkfreuro"))
