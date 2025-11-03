# install.packages("remotes")
remotes::install_github("ankargren/mfbvar")

library(mfbvar)

# 1) Set up a prior + model spec (monthly + quarterly demo data)
prior <- set_prior(
  Y       = mf_usa,     # built-in mixed-frequency dataset
  n_lags  = 4,
  n_reps  = 5000,       # total MCMC draws
  n_burnin= 2000,       # burn-in
  n_fcst  = 4,          # forecast 4 periods ahead
  aggregation = "average"  # or "triangular"
)

# 2) Estimate the model (homoskedastic NIW error by default)
fit <- estimate_mfbvar(prior, variance = "iw")

# optional: with factor stochastic volatility (choose number of factors)
# fit <- estimate_mfbvar(prior, variance = "fsv", n_fac = 1)

# 3) Inspect + forecast
summary(fit)
fcst <- predict(fit, aggregate_fcst = TRUE, pred_bands = 0.8)
plot(fcst)