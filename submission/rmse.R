# in this file, we will test BVAR model with different priors, selected
#variables and lags to compute the RMSE in each case.

#results in this file will be used to compare different
# models and select the best one to use in main.R

#--------------------- libraries ---------------------#
library(ggplot2)
library(dplyr)
library(tidyr)
library(BVAR)
library(lubridate)
library(vars)  
library(forecast)
library(tseries)

# --------------------- set up ------------------------#

# set seed for reproducibility
set.seed(42)

# load data and data manipulation
df <- utils::read.csv("data/data_quarterly.csv")

df$date <- as.Date(paste0(df$date, "-01")) # format date
df <- df %>% filter(date <= as.Date("2019-01-01")) # until 01.10.2025


# inflate CPI to get inflation rate and remove first NA row
df$inflation <- ((df$cpi - dplyr::lag(df$cpi, 1)) / dplyr::lag(df$cpi, 1) ) * 100
df <- df %>% filter(!is.na(inflation)) 


# -------------------- transformations --------------------
rate_variables <- c("inflation", "urilo", "srate", "srate_ge")
forecast_variables <- c("gdp", "inflation", "wkfreuro")

#log difference
for (var in names(df)) {
  if (!(var %in% rate_variables) && var != "date") {
    df[[var]] <-  (log(df[[var]]) - log(dplyr::lag(df[[var]], 1))) *100
  }
}

df <- df %>% filter(!is.na(gdp)) 


# ---------------------- lags ---------------------#

# possible lags: 1, 4 (one year)
lags <- c(1,4)


# ------------------ selected variables ------------------#
# we will test three sets of variables to see how the forecast
# we use RMSE to compare them

selected_variables_0 <- c("gdp", "inflation", "wkfreuro")

selected_variables_1 <- c("gdp", "inflation", "wkfreuro", "consg",  "ltot", 
                          "wage","poilusd", "pcioecd")

selected_variables_2 <- c("gdp", "inflation", "wkfreuro", "consg", "ifix", 
                        "exc1", "imc1", "ltot", "uroff", "wage", "srate", 
                          "poilusd", "pcioecd")

# List of variable sets to test
variable_sets <- list(
  set_0 = selected_variables_0,
  set_1 = selected_variables_1,
  set_2 = selected_variables_2
)

#--------------------- priors ---------------------#
# ------------------- Minnesota prior ------------------- #
mn <- bv_minnesota(
  lambda = bv_lambda(mode = 0.5, sd = 0.1, min = 0.001, max = 5),
  alpha  = bv_alpha(mode = 4),
)

# ------------------- Sum-of-coefficients prior ------------------- #
soc <- bv_soc(mode = 1, sd = 0.5)   

# ------------------- Prior configurations ------------------- #
mn_prior_bv <- bv_priors(
  hyper = c("lambda", "alpha"),
  mn = mn
)

soc_prior_bv <- bv_priors(
  hyper = c("lambda", "alpha"),
  mn = mn,
  soc = soc
)

# List of prior configurations to test
prior_configs <- list( 
  mn      = mn_prior_bv,
  soc     = soc_prior_bv
)

# ---------------------- rolling window size ---------------------#
rolling_window_size <- 40  # e.g., 40 quarters (10 years)

#--------------------------------------------------#


#-------------------- BVAR/VAR RMSE computation ---------------------#
# Function to compute RMSE for BVAR model with rolling window
compute_bvar_rmse <- function(data, variables, lags, prior, window_size, prior_name){
    
  print(paste("Computing RMSE for prior:", prior_name, "with lags:", lags, "and variables:", paste(variables, collapse = ", ")))
  horizon <- 1
  n_obs <- nrow(data)

  forecast_variables <- c("gdp", "inflation", "wkfreuro")

  # initialize matrix to store predictions (50% quantile => median)
  # each row is an observation, each column a variable
  pred_q50 <- matrix(NA_real_, nrow = n_obs, ncol = length(variables),
                     dimnames = list(NULL, variables))

  
  # rolling window forecasting
  for (i in seq(from = window_size + lags, to = n_obs - horizon)) {
    
    train_start <- i - window_size + 1
    train_end <- i
    
    # select the data for the rolling window
    y_train <- data[train_start:train_end, ]
    
    # fitting model
    invisible(trained_model <- bvar(
      y_train %>% dplyr::select(all_of(variables)),
      lags = lags,
      n_draw = 10000,
      n_burn = 2500,
      n_thin = 1,
      priors = prior,
      verbose = FALSE
    ))
    
    # make prediction
    prediction <- predict(trained_model, horizon = horizon)
    pred_q50[i + horizon, ] <- prediction$quants["50%", 1, ]
  }

  # create a data frame with results (one column for variable, one for rmse one
  res <- data.frame(
    variable = forecast_variables,
    rmse = numeric(length(forecast_variables))
  )
  
  # iterate over forecasted variables to compute RMSE
  for (i in seq_along(forecast_variables)) {
    
    var <- forecast_variables[i]


    cat(var)
    valid_indices <- which(!is.na(pred_q50[, var]))
    rmse <- sqrt(mean((pred_q50[valid_indices, var] - data[valid_indices, var])^2))
    mae <- mean(abs(pred_q50[valid_indices, var] - data[valid_indices, var]))

    cat(" RMSE: ", rmse, " MAE: ", mae, "\n")
    
    res$rmse[i] <- rmse
  }
  
  output_folder <- "output/plots/forecast_vs_actual"
  # Plot actual vs forecast for each variable
  for (i in seq_along(forecast_variables)) {

    
    var <- forecast_variables[i]

    plot_data <- data.frame(
      date = data$date,
      actual = data[[var]],
      forecast = pred_q50[, var]
    )
    
    p <- ggplot(plot_data, aes(x = date)) +
      geom_line(aes(y = actual, color = "Actual")) +
      geom_line(aes(y = forecast, color = "Forecast")) +
      labs(title = paste("BVAR Forecast vs Actual for", var),
           subtitle = paste("Prior:", prior_name, "(lags:", lags, ")"),
           caption = paste("Rolling window size:", window_size, " | Variables:", paste(variables, collapse = ", ")),
           y = var,
           color = "Legend") +
      theme_bw()
    print(p)
    # save plots
      ggsave(
    filename = paste0(output_folder, "/", var, "_", prior_name, "_lags", lags, "w", window_size, "_v", paste(variables, collapse = "_"), ".png"),
    plot = p,
    width = 8, height = 6, dpi = 300
  )
    
  }
  
  return(res)
}

#–-------------------- BVAR/VAR RMSE computation ---------------------#

results_df <- data.frame(
  variable = character(),
  rmse = numeric(),
  lags = integer(),
  variable_set = character(),
  prior = character(),
  stringsAsFactors = FALSE
)

for (lag in lags) {
 
  for (var_set_name in names(variable_sets)) {

    selected_vars <- variable_sets[[var_set_name]]
    
    for (prior_name in names(prior_configs)) {
      cat("testing: ", lag, " ", var_set_name, " ",prior_name, "\n")
      prior_config <- prior_configs[[prior_name]]
      
      temp_results <- compute_bvar_rmse(
        data = df,
        variables = selected_vars,
        lags = lag,
        prior = prior_config,
        window_size = rolling_window_size,
        prior_name = prior_name
      )
      temp_results$lags <- lag
      temp_results$variable_set <- var_set_name
      temp_results$prior <- prior_name
      results_df <- rbind(results_df, temp_results)
    }
  }
}

results_df

# save results to the output folder
save(results_df, file = "output/results_df.RData")
write.csv(results_df, file = "output/results_df.csv", row.names = FALSE)

# displaying results a bit differently

results_long <- results_df %>%
  pivot_longer(
    cols = starts_with("rmse"),     
    names_to = "forecasted_variable",  
    names_prefix = "RMSE_",            
    values_to = "RMSE_value"        
  )

# sort by name of variable
results_long <- results_long %>%
  arrange(forecasted_variable)
print(results_long)

results_sorted_structured <- results_long %>%
  arrange(variable, variable_set, lags, prior)

print(results_sorted_structured, n = 40)
