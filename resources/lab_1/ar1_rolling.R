## ------------- Lab 1: Forecasting GDP growth using AR models -----------------
# Rolling window approach using AR(1) model estimated via OLS.

## Housekeeping ----------------------------------------------------------------

setwd("...")

library(tidyverse)
library(stats)

## Data prep -------------------------------------------------------------------

data_raw <- readxl::read_excel("swiss_gdp.xlsx", sheet = "real_q")

data <- data_raw %>%
  drop_na() %>%
  arrange(date) %>%
  mutate(date = as.Date(date)) %>%
  mutate(y = as.numeric(qoq_gdp))

ggplot(data, aes(x = date, qoq_gdp)) +
  geom_line() +
  theme_classic()

## Forecasting Exercise --------------------------------------------------------

# AR(1) model: y_{t} = c + phi * y_{t-1} + epsilon_t
# y : gdp growth
# goal: make 1-step ahead forecasts
# evaluate out-of-sample fit via rmsfe

# define parameters

window_size = 80 # we always train on the last 20 years -> rolling window (alternatively use expanding window, see Bayesian estimation)
h = 1 # forecast horizon
num_obs = nrow(data)
p = 1 # order of AR model
num_forecasts <- nrow(data) - window_size - p # number of individual forecasts we will make (needed for efficient initialisation)

# initialisation to store results

date_vec <- as.Date(rep(NA, num_forecasts))
predicted_vec <- numeric(num_forecasts)
true_value_vec <- numeric(num_forecasts)

# forecasting

for (ii in seq(from = window_size, to = num_obs - h)) {
  print(ii)
  train_start = ii - window_size + 1
  train_end = ii

  y_train <- ts(data$y[train_start:train_end], frequency = 4)

  trained_model <- stats::ar(
    y_train,
    order = p,
    method = "ols",
    demean = FALSE,
    intercept = TRUE
  )
  y_true <- ts(data$y[train_end + 1], frequency = 4)

  predict <- stats::predict(trained_model, n.ahead = 1)
  
  # store results
  date_vec[ii- window_size] <- data$date[train_end + 1]
  predicted_vec[ii- window_size] <- predict$pred
  true_value_vec[ii - window_size] <- y_true

}

results <- data.frame(
  date = date_vec,
  predicted = predicted_vec,
  true_value = true_value_vec
)

results_long <- results %>% 
  pivot_longer(cols = c(predicted, true_value),
               names_to = "type",
               values_to = "value")
# plotting

ggplot(results_long, aes(x = date, y = value, colour = type)) +
  geom_line() +
  labs(x = "Date",
       y = "GDP Growth") +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.justification = "center")

# Evaluation -------------------------------------------------------------------

# We can use evaluation metrics to assess how well the model performs.
# There are a couple of different evaluation metrics. We'll look at the RMSE

results <- results %>% 
  mutate(squared_dev = (predicted - true_value)^2)

rmsfe <- sqrt(mean(results$squared_dev))


