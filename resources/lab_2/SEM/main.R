# renv::install("TimothyMerlin/koma")
# https://github.com/TimothyMerlin/koma
# Documentation: https://timothymerlin.github.io/koma/
library(koma)

## Equations Syntax ############################################################
# Stochastic equations:
# With default intercept:
"consumption ~ gdp + consumption.L(1)"
# Without intercept:
"consumption ~ gdp + consumption.L(1) - 1"
# Explicit intercept:
"consumption ~ 1 + gdp + consumption.L(1)"

# Identity equations:
"gdp == 0.7*consumption + 0.2*investment + 0.2*government - 0.1*net_exports"

# Weights:
"gdp == (nom_cons/nom_gdp) * cons"

# Lag Notation:
"x.L(1)"
"lag(x, 1)"

# Range of lags:
"x.L(1:3)"
"lag(x, 1:3)"

# Mix range and specific lags:
"x.L(1:3, 5)"

# Priors
"{1, 0.1} x3"

# Equation-specific tau
"consumption ~ constant + gdp + consumption.L(1) + consumption.L(2) [tau = 1.2]"

## extended time series (ets) class ############################################
v <- ets(data = 1:10, start = c(2019, 1), frequency = 4)
print(v)

w <- ets(
  data = 1:10, start = c(2019, 1), frequency = 4,
  series_type = "level", value_type = "real", method = "diff_log"
)
print(w)

ts_obj <- stats::ts(1:10, start = c(2019, 1), frequency = 4)

x <- as_ets(
  ts_obj,
  series_type = "level",
  value_type = "real",
  method = "diff_log"
)

rate(x)
stats::window(x, start = c(2019, 4))
level(stats::window(rate(x), start = c(2019, 4)))

stats::window(x, start = 2018, extend = TRUE)

stats::window(rate(x), start = 2018, extend = TRUE)

na.omit(window(x, start = 2018, extend = TRUE))

tempdisagg::ta(x, conversion = "sum", to = "annual")

stats::window(rate(x), start = 2020)
attr(rate(x), "anker")

rebase(x, start = c(2020, 1), end = c(2020, 1))
rebase(x, start = c(2020, 1), end = c(2020, 4))

lag(rate(x), k = -1)

log(x)
diff(x)
x * 10
x[1:2]
x[3:5]
x[4:3]
x / x
x * x
x + x
x - x

##### Create system of equations ###############################################
# Equation syntax:
# https://timothymerlin.github.io/koma/articles/equations.html
equations <-
  "consp ~ domdemoi + consp.L(1),
ifix ~ srate + ifix.L(1),
srate ~ srate_ge + srate.L(1),
pconsp ~ wkfreuro + poilusd + pconsp.L(1),
pifix ~ pifix.L(1),
domdemoi == (nconsp/ndomdemoi)*consp + (nconsg/ndomdemoi)*consg + (nifix/ndomdemoi)*ifix,
nconsp == 1*consp + 1*pconsp,
nifix == 1*ifix + 1*pifix"

## Vector of exogenous variables
exogenous_variables <- c("srate_ge", "wkfreuro", "poilusd", "consg")

##### Define dates for estimation ##############################################
dates <- list(
  estimation = list(start = c(1996, 1), end = c(2019, 4)),
  dynamic_weights = list(start = c(1996, 1), end = c(2024, 4)),
  forecast = list(start = c(2025, 4), end = c(2027, 4))
)

## Extract all relevant information from the string vector
sys_eq <- koma::system_of_equations(equations, exogenous_variables)
sys_eq
sys_eq$endogenous_variables
sys_eq$stochastic_equations
sys_eq$character_gamma_matrix
sys_eq$character_beta_matrix

# Access documentation
?koma::system_of_equations()

##### Load data ################################################################
df <- read.csv("../../../submission/data/data_quarterly.csv")
# remove data column
df$date <- NULL
# convert to time series
raw_data <- lapply(seq_len(ncol(df)), function(i) {
  ts(df[i], start = c(1992, 1), end = c(2027, 4), frequency = 4)
})
names(raw_data) <- colnames(df)
raw_data <- raw_data[c(sys_eq$endogenous_variables, sys_eq$exogenous_variables, "nconsg", "ndomdemoi")]
# shorten endogenous series to before forecast start
raw_data[sys_eq$endogenous_variables] <- lapply(sys_eq$endogenous_variables, function(x) {
  window(raw_data[[x]], end = c(2025, 3))
})

#### Compute growth rates for forecasts ########################################
ts_data <- lapply(names(raw_data), function(x) {
  series_type <- "level"
  if (x %in% c("srate", "srate_ge")) {
    series_type <- "rate"
    method <- "none"
  } else {
    method <- "diff_log"
  }

  koma::as_ets(raw_data[[x]],
    series_type = series_type,
    method = method
  )
})
names(ts_data) <- names(raw_data)

##### Estimate model ###########################################################
estimates <- koma::estimate(
  ts_data, sys_eq, dates,
  options = list(ndraws = 200)
)

## Analyze and summarize posterior estimates
print(estimates) # System of equations with posterior means
summary(estimates, variables = "ifix") # Posterior mean and 90% error bands
summary(estimates, variables = c("consp", "ifix")) # Posterior means and 90% error bands


##### Conditional Forecasting (empty) ##########################################
# Specify restrictions
restrictions <- list() # no conditional forecasts

##### Forecasting ##############################################################
# Compute forecasts (default is point forecast and posterior mean)
forecasts <- koma::forecast(estimates, dates, restrictions = restrictions) # , point_forecast = list(active=TRUE) )

## Print forecasts
print(forecasts)
print(forecasts, variables = c("consp"))
print(forecasts, variables = c("consp", "ifix"))

## Compute levels
# level(forecasts$mean$consp)

## Plot forecasts
plot(forecasts, variables = c("ifix"))

##### Conditional Forecasting ##################################################
# Specify restrictions on ifix
restrictions <- list(
  ifix = list(horizon = c(1, 2), value = c(-2, -2))
)

# Compute forecasts (default is point forecast and posterior mean)
forecasts <- koma::forecast(estimates, dates, restrictions = restrictions) # , point_forecast = list(active=TRUE) )

## Print forecasts
print(forecasts)
print(forecasts, variables = c("ifix"))

##### Informative prior ########################################################
equations <-
  "consp ~ domdemoi + consp.L(1),
ifix ~ {0,1000}constant + {0,0.00001}srate + {0,0.00001}ifix.L(1) + {3,0.0001},
srate ~ srate_ge + srate.L(1),
pconsp ~ wkfreuro + poilusd + pconsp.L(1),
pifix ~ pifix.L(1),
domdemoi == (nconsp/ndomdemoi)*consp + (nconsg/ndomdemoi)*consg + (nifix/ndomdemoi)*ifix,
nconsp == 1*consp + 1*pconsp"
sys_eq <- koma::system_of_equations(equations, exogenous_variables)

estimates_informative <- koma::estimate(
  ts_data, sys_eq, dates,
  options = list(ndraws = 200)
)

## Analyze and summarize posterior estimates
print(estimates_informative) # System of equations with posterior means
summary(estimates_informative, variables = "ifix") # Posterior mean and 90% error bands

##### Re-estimate single equations ##############################################
equations <-
  "consp ~ domdemoi + consp.L(1),
ifix ~ srate + ifix.L(1) + ifix.L(2),
srate ~ srate_ge + srate.L(1),
pconsp ~ wkfreuro + poilusd + pconsp.L(1),
pifix ~ pifix.L(1),
domdemoi == (nconsp/ndomdemoi)*consp + (nconsg/ndomdemoi)*consg + (nifix/ndomdemoi)*ifix,
nconsp == 1*consp + 1*pconsp"
sys_eq <- koma::system_of_equations(equations, exogenous_variables)

estimates_ifix <- koma::estimate(
  estimates = estimates, ts_data, sys_eq, dates,
  options = list(ndraws = 200)
)

## Analyze and summarize posterior estimates
print(estimates_ifix) # System of equations with posterior means
# ?summary.koma_estimate
summary(estimates_ifix, variables = "ifix") # Posterior mean and 90% error bands
