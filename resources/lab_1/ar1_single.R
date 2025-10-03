## ------------- Lab 1: Forecasting GDP growth using AR models -----------------
# Single forecast using AR(1) process estimated via OLS.

## Housekeeping ----------------------------------------------------------------

setwd("...")

library(tidyverse)
library(stats)

## Data prep -------------------------------------------------------------------

data_raw <- readxl::read_excel("swiss_gdp.xlsx", sheet = "real_q")

data <- data_raw %>%
  drop_na() %>%
  arrange(date) %>%
  filter(date >= "2000-01-01" & date < "2010-04-01") %>% 
  mutate(date = as.Date(date)) %>%
  mutate(y = as.numeric(qoq_gdp))

ggplot(data, aes(x = date, qoq_gdp)) +
  geom_line() +
  theme_classic()

## Forecasting Exercise --------------------------------------------------------

# AR(1) model: y_{t} = c + phi * y_{t-1} + epsilon_t
# y : gdp growth
# goal: make a single forecast one-step ahead forecast

y_train <- ts(data$y[1:(nrow(data)-1)], frequency = 4)

trained_model <- stats::ar(
  y_train,
  order = 1,
  method = "ols",
  demean = FALSE,
  intercept = TRUE
)

forecast <- predict(trained_model, n.ahead = 1)

## Interpret results ----------------------------------------------------------

# predicted value for Q1 2010: 

gdp_10q1_forecast <- as.numeric(forecast$pred)
print(paste0("forecast for 2010 Q1: ", round(gdp_10q1_forecast, 2)))
gdp_10q1_true <- as.numeric(data[data$date == as.Date("2010-01-01"), "y"])
print(paste0("true value for 2010 Q1: ", round(gdp_10q1_true, 2)))

# let's look at the estimated model:

# phi:
phi <- trained_model$ar
c <- trained_model$x.intercept

# remember: abs(phi) < 1: geometric decay, 
# phi = 1: permanent effect, 
# abs(phi) > 1: explosive,
# phi < 0: alternates in sign.
