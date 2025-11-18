library(vars)
library(dplyr)

df <- utils::read.csv("data/data_quarterly.csv")
df$date <- as.Date(paste0(df$date, "-01"))

selected_variables <- c("gdp", "wkfreuro", "inflation")

df <- df %>%
  filter(!is.na(gdp), !is.na(wkfreuro), !is.na(inflation)) %>%
  select(date, all_of(selected_variables))

lag_selection <- VARselect(df %>% select(-date), lag.max = 8, type = "const")
optimal_lag <- lag_selection$selection["AIC(n)"]

var_model <- VAR(df %>% select(-date), p = optimal_lag, type = "const")

forecast_horizon <- 4
var_forecast <- predict(var_model, n.ahead = forecast_horizon, ci = 0.95)


# Load required library
library(ggplot2)

# Define the function for the number of coefficients
num_coefficients <- function(n, p) {
  n * (n * p + 1)
}

# Create a grid of variables (n) and lags (p)
n_vars <- 1:10  # Number of variables (1 to 10)
p_lags <- 1:10  # Number of lags (1 to 10)

# Generate a data frame with all combinations of n and p
grid <- expand.grid(n = n_vars, p = p_lags)

# Calculate the number of coefficients for each combination
grid$num_coeff <- mapply(num_coefficients, grid$n, grid$p)

# Plot the results
ggplot(grid, aes(x = p, y = num_coeff, color = as.factor(n), group = n)) +
  geom_line(size = 1) +
  labs(
    title = "Growth of Parameters in VAR Model",
    x = "Number of Lags (p)",
    y = "Number of Coefficients",
    color = "Number of Variables (n)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "right"
  )