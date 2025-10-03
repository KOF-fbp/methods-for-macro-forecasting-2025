## -------------------------- Lab 2: BVARs -------------------------------------
# BVAR estimation: bvar package US data
# example adjusted from bvar package
# task: implement with swiss data -> we will discuss afterwards

## Housekeeping ----------------------------------------------------------------

library(tidyverse)
library(BVAR)

## Prep data -------------------------------------------------------------------

data <- fred_qd[, c("GDPC1", "CPIAUCSL", "UNRATE", "FEDFUNDS")] %>% 
  filter(as.Date(rownames(.)) < as.Date("2020-03-01"))
  
data <- fred_transform(data, codes = c(5, 5, 5, 1), lag = 4) # transform to be stationary

## Estimate model --------------------------------------------------------------
# bayesian VAR(1): y_t = c + phi_1* y_{t-1} + epsilon_t
# with y_t = [gdp, cpi, unempl, fedfunds]'
# parameters: (1+4)*4 = 20 coefficients

set.seed(1)

model <- bvar(
  data,
  lags = 1,
  n_draw = 10000,
  n_burn = 2500,
  n_thin = 1 #thinning
) 

plot(model)

## Look at results -------------------------------------------------------------

# Calculate and store forecasts and impulse responses
predict(model) <- predict(model, 
                          horizon = 20 
                          #conf_bands = c(0.05, 0.16)
                          )
irf(model) <- irf(
  model,
  horizon = 16,
  identification = TRUE
)

# Plot forecasts and impulse responses
plot(predict(model))
plot(irf(model), vars_impulse = c("FEDFUNDS"),
     vars_response = c("GDPC1", "CPIAUCSL", "UNRATE"))

## Conditional Forecast --------------------------------------------------------

path <- c(2.25, 3, 4, 5.5, 6.75, 4.25, 2.75, 2, 2, 2)
predict(model) <- predict(model, 
                          horizon = 16,
                          cond_path = path, 
                          cond_var = "FEDFUNDS")
plot(predict(model), t_back = 16)

## Notes -----------------------------------------------------------------------

# Minnesota prior (example):
mn <- bv_minnesota(
  lambda = bv_lambda(mode = 0.2, sd = 0.4, min = 0.0001, max = 5),
  alpha = bv_alpha(mode = 2))

priors <- bv_priors(hyper = "lambda", mn = mn)

model <- bvar(
  data,
  lags = 1,
  n_draw = 10000,
  n_burn = 2500,
  n_thin = 1,
  priors = priors,
  verbose = TRUE
)

