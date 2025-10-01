# renv::install("TimothyMerlin/koma")
# https://github.com/TimothyMerlin/koma
# Documentation: https://timothymerlin.github.io/koma/
library(koma)

##### Create system of equations ###############################################
equations <- 
"consp ~ domdemoi + consp.L(1),
ifix ~ srate + ifix.L(1),
srate ~ srate_ge + srate.L(1),
pconsp ~ wkfreuro + poilusd + pconsp.L(1),
pifix ~ pifix.L(1),
domdemoi == (nconsp/ndomdemoi)*consp + (nconsg/ndomdemoi)*consg + (nifix/ndomdemoi)*ifix,
nconsp == 1*consp + 1*pconsp,
nifix == 1*ifix + 1*pifix,
ndomdemoi == 1*nconsp + 1*nifix"

## Vector of exogenous variables
exogenous_variables <- c("srate_ge","wkfreuro","poilusd","consg", "nconsg")

##### Define dates for estimation #######################
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

##### Load data ################################################################
df <- read.csv("../../../submission/data/data_quarterly.csv") 
# shorten variable names
colnames(df) <- gsub("ch.kof.vja.koma.", "", colnames(df))
# remove data column
df$date <- NULL
# convert to time series
raw_data <- lapply(seq_len(ncol(df)), function(i) {
 ts(df[i], start = c(1992,1), end = c(2027,4), frequency = 4)
 })
names(raw_data) <- colnames(df)
raw_data <- raw_data[c(sys_eq$endogenous_variables, sys_eq$exogenous_variables)]
# shorten endogenous series to before forecast start
raw_data[sys_eq$endogenous_variables] <- lapply(sys_eq$endogenous_variables, function(x){
  window(raw_data[[x]], end=c(2025,3))
})

#### Compute growth rates for forecasts ########################################
ts_data <- lapply(names(raw_data), function(x) {
  # Check for non-numeric data
  if (!is.numeric(raw_data[[x]])) {
    stop(paste("Non-numeric data encountered in variable:", x))
  }

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
print(estimates)  # System of equations with posterior means
summary(estimates, variables = "ifix")  # Posterior mean and 90% error bands
summary(estimates, variables = c("consp", "ifix"))  # Posterior means and 90% error bands


##### Conditional Forecasting (empty) ##########################################
# Specify restrictions
restrictions <- list() # no conditional forecasts

##### Forecasting ##############################################################
# Compute forecasts (default is point forecast and posterior mean)
forecasts <- koma::forecast(estimates, dates, restrictions = restrictions)#, point_forecast = list(active=TRUE) )

## Print forecasts
print(forecasts)
print(forecasts, variables = c("consp"))
print(forecasts, variables = c("consp", "ifix"))

## Compute levels
#level(forecasts$mean$consp)

## Plot forecasts
plot(forecasts, variables = c("ifix"))

##### Conditional Forecasting ##################################################
# Specify restrictions on ifix
restrictions <- list(
  ifix = list(horizon = c(1, 2), value = c(-2, -2))
)

# Compute forecasts (default is point forecast and posterior mean)
forecasts <- koma::forecast(estimates, dates, restrictions = restrictions)#, point_forecast = list(active=TRUE) )

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
nconsp == 1*consp + 1*pconsp,
nifix == 1*ifix + 1*pifix"
sys_eq <- koma::system_of_equations(equations, exogenous_variables)

estimates_informative <- koma::estimate(
  ts_data, sys_eq, dates,
  options = list(ndraws = 200)
)

## Analyze and summarize posterior estimates
print(estimates_informative)  # System of equations with posterior means
summary(estimates_informative, variables = "ifix")  # Posterior mean and 90% error bands

##### Re-estimate single equations ##############################################
equations <- 
"consp ~ domdemoi + consp.L(1),
ifix ~ srate + ifix.L(1) + ifix.L(2),
srate ~ srate_ge + srate.L(1),
pconsp ~ wkfreuro + poilusd + pconsp.L(1),
pifix ~ pifix.L(1),
domdemoi == (nconsp/ndomdemoi)*consp + (nconsg/ndomdemoi)*consg + (nifix/ndomdemoi)*ifix,
nconsp == 1*consp + 1*pconsp,
nifix == 1*ifix + 1*pifix"
sys_eq <- koma::system_of_equations(equations, exogenous_variables)

estimates_ifix <- koma::estimate(
  estimates=estimates,ts_data,sys_eq, dates,
  options = list(ndraws = 200)
)

## Analyze and summarize posterior estimates
print(estimates_ifix)  # System of equations with posterior means
summary(estimates_ifix, variables = "ifix")  # Posterior mean and 90% error bands
