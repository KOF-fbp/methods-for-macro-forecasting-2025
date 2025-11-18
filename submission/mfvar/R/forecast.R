#' Produce forecasts from MFVAR posterior draws
#'
#' @param posterior List returned by gibbs_mfvar
#' @param horizons_months Maximum forecast horizon in months
#' @param nsim Number of simulations per posterior draw
#' @param seed Random seed
#' @return List with predictive means and intervals for monthly states
#' @export
predict_mfvar <- function(posterior, horizons_months = 12, nsim = 1000, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  n <- posterior$n
  p <- posterior$p
  n_draws <- posterior$n_draws
  T_obs <- dim(posterior$y_latent_draws)[1]
  
  # Storage for forecasts
  # For each posterior draw, simulate nsim paths forward
  y_forecasts <- array(0, dim = c(horizons_months, n, n_draws))
  
  cli::cli_alert_info("Generating forecasts for {horizons_months} months ahead")
  
  for (d in 1:n_draws) {
    
    if (d %% 100 == 0) {
      cli::cli_alert_info("Processing draw {d}/{n_draws}")
    }
    
    # Extract slices - A_d should be (n*p+1) x n
    A_d <- posterior$A_draws[, , d]
    Sigma_d <- posterior$Sigma_draws[, , d]
    y_latent_d <- posterior$y_latent_draws[, , d]
    
    # Check/transpose if needed (when extracting from 3D array, dims might swap)
    if (nrow(A_d) != (n*p+1)) {
      A_d <- t(A_d)
    }
    
    # Extract A matrices
    A_list_d <- unpack_A(A_d, n, p)
    
    # Initial conditions: last p observations
    y_init <- y_latent_d[(T_obs - p + 1):T_obs, , drop = FALSE]
    
    # Simulate forward
    y_sim <- matrix(0, horizons_months, n)
    
    for (h in 1:horizons_months) {
      # Compute forecast: intercept + A_1*y_{t-1} + ... + A_p*y_{t-p}
      y_forecast <- as.vector(A_list_d[[p + 1]])  # Intercept (convert 1xn to vector)
      
      for (lag in 1:p) {
        if (h > lag) {
          # Use simulated value
          y_lag <- y_sim[h - lag, ]  # Vector (n)
        } else {
          # Use initial condition: the (p - lag + 1)-th row from the end
          idx <- p - lag + 1  # Index into y_init (1 to p)
          y_lag <- y_init[idx, ]  # Vector (n)
        }
        # A_list_d[[lag]] is n x n, y_lag is vector length n
        y_forecast <- y_forecast + as.vector(A_list_d[[lag]] %*% y_lag)
      }
      
      # Add shock
      shock <- mvtnorm::rmvnorm(1, mean = rep(0, n), sigma = Sigma_d)
      y_sim[h, ] <- y_forecast + as.vector(shock)
    }
    
    # Average over simulations (for this implementation, nsim is implicit)
    # Store the simulated path
    y_forecasts[, , d] <- y_sim
  }
  
  # Compute predictive statistics
  y_mean <- apply(y_forecasts, c(1, 2), mean)
  y_lower <- apply(y_forecasts, c(1, 2), quantile, 0.05)
  y_upper <- apply(y_forecasts, c(1, 2), quantile, 0.95)
  
  colnames(y_mean) <- colnames(y_lower) <- colnames(y_upper) <- posterior$series_names
  
  cli::cli_alert_success("Forecasts generated")
  
  return(list(
    y_mean = y_mean,
    y_lower = y_lower,
    y_upper = y_upper,
    y_forecasts = y_forecasts,
    horizons_months = horizons_months
  ))
}

#' Extract target forecasts from latent monthly predictions
#'
#' @param pred_obj List returned by predict_mfvar
#' @param meta Metadata list
#' @param growth_def List with definitions: gdp, infl, exr
#' @param horizons Vector of horizons to report (in months)
#' @return data.table with point forecasts for each target and horizon
#' @export
targets_from_latent <- function(pred_obj, meta, 
                                 growth_def = list(gdp = "qoq_ann", infl = "yoy", exr = "monthly"),
                                 horizons = c(1, 12)) {
  
  y_mean <- pred_obj$y_mean
  n <- ncol(y_mean)
  series_names <- colnames(y_mean)
  
  # Initialize results
  results <- list()
  
  # GDP growth
  if (!is.null(meta$gdp_var)) {
    gdp_idx <- which(series_names == meta$gdp_var)
    
    if (length(gdp_idx) > 0) {
      gdp_forecast <- y_mean[, gdp_idx]
      
      for (h in horizons) {
        if (h <= length(gdp_forecast)) {
          
          if (growth_def$gdp == "qoq_ann") {
            # Quarter-over-quarter annualized
            # Need quarterly values at h and h-3
            # Quarterly value = average of 3 monthly log levels
            
            if (h >= 4) {
              # Current quarter average (months h-2, h-1, h)
              q_current <- mean(gdp_forecast[max(1, h-2):h])
              # Previous quarter average (months h-5, h-4, h-3)
              q_prev <- mean(gdp_forecast[max(1, h-5):max(1, h-3)])
              growth <- 4 * 100 * (q_current - q_prev)
            } else {
              # Use available data
              growth <- NA
            }
            
          } else if (growth_def$gdp == "yoy") {
            # Year-over-year
            if (h >= 13) {
              growth <- 100 * (gdp_forecast[h] - gdp_forecast[h - 12])
            } else {
              growth <- NA
            }
          }
          
          results[[length(results) + 1]] <- list(
            target = "GDP_growth",
            horizon = h,
            value = growth
          )
        }
      }
    }
  }
  
  # Inflation
  if (!is.null(meta$price_vars) && length(meta$price_vars) > 0) {
    cpi_var <- meta$price_vars[1]  # Use first price variable
    cpi_idx <- which(series_names == cpi_var)
    
    if (length(cpi_idx) > 0) {
      cpi_forecast <- y_mean[, cpi_idx]
      
      for (h in horizons) {
        if (h <= length(cpi_forecast)) {
          
          if (growth_def$infl == "yoy") {
            # Year-over-year
            if (h >= 13) {
              inflation <- 100 * (cpi_forecast[h] - cpi_forecast[h - 12])
            } else {
              inflation <- NA
            }
            
          } else if (growth_def$infl == "monthly_ann") {
            # Monthly annualized
            if (h >= 2) {
              inflation <- 12 * 100 * (cpi_forecast[h] - cpi_forecast[h - 1])
            } else {
              inflation <- NA
            }
          }
          
          results[[length(results) + 1]] <- list(
            target = "Inflation",
            horizon = h,
            value = inflation
          )
        }
      }
    }
  }
  
  # Exchange rate
  if (!is.null(meta$exrate_vars) && length(meta$exrate_vars) > 0) {
    exr_var <- meta$exrate_vars[1]
    exr_idx <- which(series_names == exr_var)
    
    if (length(exr_idx) > 0) {
      exr_forecast <- y_mean[, exr_idx]
      
      for (h in horizons) {
        if (h <= length(exr_forecast)) {
          
          if (growth_def$exr == "monthly") {
            # Monthly log difference
            if (h >= 2) {
              exr_growth <- 100 * (exr_forecast[h] - exr_forecast[h - 1])
            } else {
              exr_growth <- NA
            }
            
          } else if (growth_def$exr == "yoy") {
            # 12-month cumulative
            if (h >= 13) {
              exr_growth <- 100 * (exr_forecast[h] - exr_forecast[h - 12])
            } else {
              exr_growth <- NA
            }
          }
          
          results[[length(results) + 1]] <- list(
            target = "ExRate_growth",
            horizon = h,
            value = exr_growth
          )
        }
      }
    }
  }
  
  # Convert to data.table
  dt <- data.table::rbindlist(results)
  
  return(dt)
}
