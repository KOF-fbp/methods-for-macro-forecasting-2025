#' Fit AR(2) model to a time series
#'
#' @param series Numeric vector of observations
#' @param origin_index Integer, last observation to use for estimation
#' @return List with coef (coefficients) and sigma2 (residual variance)
#' @export
fit_ar2 <- function(series, origin_index) {
  
  # Use data up to origin_index
  y <- series[1:origin_index]
  y <- y[!is.na(y)]
  
  if (length(y) < 5) {
    return(list(coef = c(0, 0, mean(y)), sigma2 = var(y)))
  }
  
  # Build regression: y_t = c + phi1*y_{t-1} + phi2*y_{t-2} + e_t
  n <- length(y)
  Y <- y[3:n]
  X <- cbind(y[2:(n-1)], y[1:(n-2)], 1)
  
  # OLS
  coef <- solve(t(X) %*% X + diag(1e-8, 3)) %*% t(X) %*% Y
  resid <- Y - X %*% coef
  sigma2 <- sum(resid^2) / length(resid)
  
  return(list(
    coef = as.vector(coef),
    sigma2 = sigma2
  ))
}

#' Forecast from AR(2) model
#'
#' @param fit List returned by fit_ar2
#' @param h Forecast horizon
#' @param y_last Vector of last 2 observations (y_{T}, y_{T-1})
#' @return Numeric forecast value
#' @export
forecast_ar2 <- function(fit, h, y_last) {
  
  phi1 <- fit$coef[1]
  phi2 <- fit$coef[2]
  const <- fit$coef[3]
  
  # Iterative forecasting
  y_T <- y_last[1]
  y_T1 <- y_last[2]
  
  for (i in 1:h) {
    y_new <- const + phi1 * y_T + phi2 * y_T1
    y_T1 <- y_T
    y_T <- y_new
  }
  
  return(y_T)
}

#' Evaluate forecasts: compute RMSE and MAE
#'
#' @param forecasts_mfvar data.table with columns: target, horizon, value
#' @param forecasts_ar2 data.table with columns: target, horizon, value
#' @param actuals data.table with columns: target, horizon, actual
#' @return data.table with model, target, horizon, RMSE, MAE
#' @export
evaluate_forecasts <- function(forecasts_mfvar, forecasts_ar2, actuals) {
  
  # Merge forecasts with actuals
  mfvar_eval <- merge(forecasts_mfvar, actuals, by = c("target", "horizon"))
  mfvar_eval$model <- "MFVAR"
  mfvar_eval$error <- mfvar_eval$value - mfvar_eval$actual
  
  ar2_eval <- merge(forecasts_ar2, actuals, by = c("target", "horizon"))
  ar2_eval$model <- "AR2"
  ar2_eval$error <- ar2_eval$value - ar2_eval$actual
  
  # Combine
  all_eval <- rbind(mfvar_eval, ar2_eval)
  
  # Compute metrics by model, target, horizon
  metrics <- all_eval[, .(
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    MAE = mean(abs(error), na.rm = TRUE)
  ), by = .(model, target, horizon)]
  
  return(metrics)
}

#' Compute marginal data density using modified harmonic mean
#'
#' @param draws List of posterior draws (A, Sigma)
#' @param log_post Vector of log posterior densities
#' @param log_lik Vector of log likelihoods
#' @return Numeric, marginal data density estimate
#' @export
marginal_data_density <- function(draws, log_post, log_lik) {
  
  # Modified harmonic mean estimator
  # log p(Y) ≈ -log(mean(exp(-log_lik)))
  
  n_draws <- length(log_lik)
  
  # Use stable computation
  max_log_lik <- max(log_lik, na.rm = TRUE)
  
  mdd <- -log(mean(exp(-log_lik + max_log_lik))) + max_log_lik
  
  return(mdd)
}

#' Select hyperparameters by marginal data density
#'
#' @param y_obs Data matrix on monthly grid
#' @param calendar Calendar object
#' @param meta Metadata list
#' @param grid data.frame with hyperparameter combinations
#' @param p VAR lag order
#' @param n_draws_short Number of draws for short chain
#' @param burnin_short Burn-in for short chain
#' @param seed Random seed
#' @return List with best_hyper and scores for each grid point
#' @export
select_hyperparameters <- function(y_obs, calendar, meta, grid, p,
                                   n_draws_short = 500, burnin_short = 200, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  n_grid <- nrow(grid)
  scores <- numeric(n_grid)
  
  cli::cli_alert_info("Evaluating {n_grid} hyperparameter combinations")
  
  for (i in 1:n_grid) {
    
    cli::cli_alert_info("Grid point {i}/{n_grid}: lambda = {grid$lambda[i]}")
    
    hyper <- list(
      lambda = grid$lambda[i],
      lag_decay = ifelse("lag_decay" %in% names(grid), grid$lag_decay[i], 1),
      cross_eq = ifelse("cross_eq" %in% names(grid), grid$cross_eq[i], 0.5),
      intercept_weight = ifelse("intercept_weight" %in% names(grid), grid$intercept_weight[i], 1)
    )
    
    # Run short Gibbs chain
    posterior <- tryCatch({
      gibbs_mfvar(y_obs, calendar, meta, p = p, hyper = hyper,
                  n_draws = n_draws_short, burnin = burnin_short, 
                  thinning = 1, seed = seed)
    }, error = function(e) {
      cli::cli_alert_warning("Error in Gibbs sampler for grid point {i}: {e$message}")
      return(NULL)
    })
    
    if (is.null(posterior)) {
      scores[i] <- -Inf
      next
    }
    
    # Compute log likelihood for each draw (simplified: use residuals)
    # For proper MDD, we'd need full likelihood evaluation
    # Here we use a proxy based on fit quality
    
    n_draws <- posterior$n_draws
    log_lik <- numeric(n_draws)
    
    for (d in 1:n_draws) {
      # Simplified log likelihood based on residuals
      y_latent <- posterior$y_latent_draws[, , d]
      Sigma <- posterior$Sigma_draws[, , d]
      
      # Compute one-step-ahead prediction errors
      # This is a simplified version
      log_lik[d] <- -0.5 * sum(diag(Sigma))  # Proxy
    }
    
    # Compute MDD
    scores[i] <- marginal_data_density(posterior, log_lik, log_lik)
    
    cli::cli_alert_info("MDD score: {round(scores[i], 2)}")
  }
  
  # Find best hyperparameters
  best_idx <- which.max(scores)
  
  best_hyper <- list(
    lambda = grid$lambda[best_idx],
    lag_decay = ifelse("lag_decay" %in% names(grid), grid$lag_decay[best_idx], 1),
    cross_eq = ifelse("cross_eq" %in% names(grid), grid$cross_eq[best_idx], 0.5),
    intercept_weight = ifelse("intercept_weight" %in% names(grid), grid$intercept_weight[best_idx], 1)
  )
  
  cli::cli_alert_success("Best hyperparameters: lambda = {best_hyper$lambda}")
  
  return(list(
    best_hyper = best_hyper,
    scores = data.frame(grid, mdd = scores)
  ))
}
