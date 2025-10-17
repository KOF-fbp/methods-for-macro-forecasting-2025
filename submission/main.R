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

model <- bvar(
  df %>% select(all_of(forecast_variables)),
  lags = 1,
  n_draw = 10000,
  n_burn = 2500,
  n_thin = 1 #thinning
) 

plot(model)

predict(model) <- predict(model, 
                          horizon = 20 ,
                          conf_bands = c(0.05, 0.16)
)
irf(model) <- irf(
  model,
  horizon = 16,
  identification = TRUE
)

# Plot forecasts and impulse responses
plot(predict(model))
plot(irf(model), vars_impulse = c("inflation"),
     vars_response = c("gdp", "inflation", "wkfreuro"))


