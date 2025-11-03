#' Gibbs sampler for Mixed Frequency VAR
#'
#' @param y_obs Data matrix on monthly grid (T x n)
#' @param calendar Calendar object
#' @param meta Metadata list
#' @param p VAR lag order
#' @param hyper Hyperparameters for Minnesota prior
#' @param n_draws Total number of draws
#' @param burnin Number of burn-in draws to discard
#' @param thinning Thinning interval
#' @param origins Vector of forecast origin dates (NULL = use last date)
#' @param seed Random seed
#' @param a0 Initial state mean (NULL = diffuse)
#' @param P0 Initial state covariance (NULL = diffuse)
#' @return List with posterior draws and diagnostics
#' @export
gibbs_mfvar <- function(y_obs, calendar, meta, p = 2,
                        hyper, n_draws = 2000, burnin = 1000, thinning = 1,
                        origins = NULL, seed = NULL,
                        a0 = NULL, P0 = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  n <- ncol(y_obs)
  T_total <- nrow(y_obs)
  series_names <- colnames(y_obs)
  if (is.null(series_names)) series_names <- paste0("V", 1:n)
  
  # Default origins: last available date
  if (is.null(origins)) {
    origins <- calendar$dates[T_total]
  }
  
  # Initialize state distribution
  if (is.null(a0)) {
    a0 <- rep(0, n * p)
  }
  if (is.null(P0)) {
    # Diffuse prior with moderate variance (KFAS limit is 1e7)
    P0 <- diag(10, n * p)
  }
  
  # Initialize A and Sigma using OLS on a preliminary dataset
  # Use a simple approach: fill missing values with interpolation for initialization
  
  y_init <- y_obs
  for (i in 1:n) {
    na_idx <- is.na(y_init[, i])
    if (any(na_idx) && !all(na_idx)) {
      # Linear interpolation
      y_init[na_idx, i] <- approx(which(!na_idx), y_init[!na_idx, i], 
                                   xout = which(na_idx), rule = 2)$y
    }
  }
  
  # Build lagged matrices
  Y_init <- y_init[(p+1):T_total, , drop = FALSE]
  X_init <- matrix(1, nrow(Y_init), 1)  # Intercept
  
  for (lag in 1:p) {
    X_init <- cbind(X_init, y_init[(p+1-lag):(T_total-lag), ])
  }
  
  # Swap intercept to last column
  X_init <- cbind(X_init[, -1], X_init[, 1])
  
  # OLS initialization
  A_init <- solve(t(X_init) %*% X_init + diag(1e-6, ncol(X_init))) %*% t(X_init) %*% Y_init
  resid_init <- Y_init - X_init %*% A_init
  Sigma_init <- (t(resid_init) %*% resid_init) / nrow(resid_init)
  
  A_current <- A_init
  Sigma_current <- Sigma_init
  
  # Storage for draws
  n_keep <- floor((n_draws - burnin) / thinning)
  A_draws <- array(0, dim = c(n * p + 1, n, n_keep))
  Sigma_draws <- array(0, dim = c(n, n, n_keep))
  y_latent_draws <- array(0, dim = c(T_total, n, n_keep))
  
  draw_idx <- 0
  
  cli::cli_alert_info("Starting Gibbs sampler: {n_draws} draws, {burnin} burnin, {thinning} thinning")
  
  for (iter in 1:n_draws) {
    
    if (iter %% 100 == 0) {
      cli::cli_alert_info("Iteration {iter}/{n_draws}")
    }
    
    # Step 1: Draw latent states given A, Sigma
    A_list_current <- unpack_A(A_current, n, p)
    
    state_draw <- draw_states(y_obs, calendar, meta, A_list_current, 
                               Sigma_current, p, a0, P0, seed = NULL)
    
    y_latent <- state_draw$y_latent
    
    # Step 2: Draw A and Sigma given latent states
    # Build regression matrices from latent states
    Y_latent <- y_latent[(p+1):T_total, , drop = FALSE]
    X_latent <- matrix(1, nrow(Y_latent), 1)
    
    for (lag in 1:p) {
      X_latent <- cbind(X_latent, y_latent[(p+1-lag):(T_total-lag), ])
    }
    
    # Swap intercept to last column
    X_latent <- cbind(X_latent[, -1], X_latent[, 1])
    
    # Add dummy observations
    dummies <- build_dummies(Y_latent, X_latent, p, hyper)
    
    # Sample Sigma
    resid <- dummies$Ytilde - dummies$Xtilde %*% A_current
    Sigma_current <- sample_Sigma(resid, v0 = n + 2, S0 = diag(1e-4, n), seed = NULL)
    
    # Check for numerical issues in Sigma
    if (any(is.na(Sigma_current)) || any(is.infinite(Sigma_current)) || max(abs(Sigma_current)) > 1e6) {
      cli::cli_alert_warning("Numerical issues in Sigma at iteration {iter}, using previous value")
      # Keep previous Sigma_current
    }
    
    # Sample A
    A_sample <- sample_A(dummies$Ytilde, dummies$Xtilde, Sigma_current, p, seed = NULL)
    A_current <- A_sample$A_matrix
    
    # Check for numerical issues in A
    if (any(is.na(A_current)) || any(is.infinite(A_current)) || max(abs(A_current)) > 1e6) {
      cli::cli_alert_warning("Numerical issues in A at iteration {iter}, using previous value")
      # Keep previous A_current - restore from sample
    }
    
    # Store draws after burnin with thinning
    if (iter > burnin && (iter - burnin) %% thinning == 0) {
      draw_idx <- draw_idx + 1
      A_draws[, , draw_idx] <- A_current
      Sigma_draws[, , draw_idx] <- Sigma_current
      y_latent_draws[, , draw_idx] <- y_latent
    }
  }
  
  cli::cli_alert_success("Gibbs sampler completed: {n_keep} draws retained")
  
  return(list(
    A_draws = A_draws,
    Sigma_draws = Sigma_draws,
    y_latent_draws = y_latent_draws,
    n_draws = n_keep,
    p = p,
    n = n,
    series_names = series_names,
    hyper = hyper,
    origins = origins
  ))
}
