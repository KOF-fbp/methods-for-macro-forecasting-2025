## ------------- Lab 1: Forecasting GDP growth using AR models -----------------
# Bayesian estimation of an AR(1) process: out-of-sample expanding window estimation

## Housekeeping ----------------------------------------------------------------

setwd("...")

library(tidyverse)
library(MASS)

source("Lab1/fun_bayesian_arp.R")

## Data prep -------------------------------------------------------------------

data_raw <- readxl::read_excel("swiss_gdp.xlsx", sheet = "real_q")

data <- data_raw %>%
  drop_na() %>%
  arrange(date) %>%
  mutate(date = as.Date(date)) %>%
  mutate(y = as.numeric(qoq_gdp))

## Forecasting exercise --------------------------------------------------------

# Out-of-sample

# Define parameters and training data
y <- data$y
p <- 1 # we're estimating and AR(1)
Tt <- length(y) - p

oos_length <- 40 # number of oos predictions (10 years)

# Define mean and precision of independent normal-gamma priors 
beta0 <- rep(0, p+1) # Prior mean
Q0 <- diag(rep(100,p+1)) # Prior on variance
a0 <- 3 # Prior shape for IG distribution
b0 <- 100 # Prior for the rate (reciprocal of scale) of IG distribution

# Number of draws and length of burn-in
burn_in <- 500 # Number of draws to be left out
R <- 5000 # Number of Gibbs iterations

# In this exercise we are only interested in point forecasts (i.e. mean, which is optimal under squared error loss function)
yhat_mean <- matrix(NA, nrow = oos_length, ncol = R-burn_in)
# Estimate model for each subsample (trainings windows grows by 1 observation each step)
for(ii in 1:oos_length){
  y <- data$y
  y <- y[1:(length(y)-oos_length+ii)] # cut sample
  Tt <- length(y) - p # length
  X <- cbind(matrix(1, Tt, 1), sapply(seq(1, p), function(x){y[seq((p + 1 - x), length(y) - x)]})) # X matrix
  y <- y[-seq(1, p)] # y
  
  # Draw from posterior via Gibbs sampler
  posterior <- independent_normal_regression(y, X, R, beta0, Q0, a0, b0)
  
  # Discard burn-in
  betas <- posterior$betas[,(burn_in+1):R]
  
  # One-step ahead mean forecast
  for(g in 1:(R-burn_in)){
    yhat_mean[ii,g] <- c(1,y[seq(Tt,Tt+1-p)])%*%betas[,g] # E[y_T+1|y_T,beta,sigma^2]
  }
}

# gather results

yhat_mean <- ts(apply(yhat_mean,1,mean), frequency = 4) # Take mean
y_all <- data$y
y_true <- y_all[(length(y_all)-oos_length+1):length(y_all)]
y_dates <- data$date[(length(y_all)-oos_length+1):length(y_all)]
results <- data.frame(date = y_dates,
                      predicted = as.numeric(yhat_mean),
                      true_value = as.numeric(y_true))


# plotting

results_long <- results %>% 
  pivot_longer(cols = c(predicted, true_value),
               names_to = "type",
               values_to = "value")

ggplot(results_long, aes(x = date, y = value, colour = type)) +
  geom_line() +
  labs(x = "Date",
       y = "GDP Growth") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  scale_color_manual(values = c("predicted" = "#215CAF",
                                "true_value" = "#C07766")) +
  theme_minimal() +
  theme(legend.position = "top",
        legend.justification = "center")

