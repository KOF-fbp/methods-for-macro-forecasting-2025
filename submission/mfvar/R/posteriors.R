#' Sample covariance matrix Sigma from inverse Wishart posterior
#'
#' @param residuals Matrix of residuals (T x n)
#' @param v0 Prior degrees of freedom
#' @param S0 Prior scale matrix
#' @param seed Random seed
#' @return Sampled covariance matrix (n x n)
#' @export
sample_Sigma <- function(residuals, v0 = NULL, S0 = NULL, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  n <- ncol(residuals)
  T_eff <- nrow(residuals)
  
  # Default prior: non-informative
  if (is.null(v0)) v0 <- n + 2
  if (is.null(S0)) S0 <- diag(1e-4, n)
  
  # Posterior parameters
  v_post <- v0 + T_eff
  S_post <- S0 + t(residuals) %*% residuals
  
  # Sample from inverse Wishart
  # IW(v, S) is equivalent to inverse of Wishart(v, S^{-1})
  
  # Sample from Wishart
  S_post_inv <- solve(S_post)
  W <- rWishart(1, v_post, S_post_inv)[, , 1]
  
  Sigma <- solve(W)
  
  return(Sigma)
}

#' Sample VAR coefficients A from Normal posterior conditional on Sigma
#'
#' @param Ytilde Augmented Y with dummies (Ttilde x n)
#' @param Xtilde Augmented X with dummies (Ttilde x k)
#' @param Sigma Covariance matrix (n x n)
#' @param p VAR lag order
#' @param seed Random seed
#' @return List with A_list (list of A_1, ..., A_p matrices) and A_matrix (full coefficient matrix)
#' @export
sample_A <- function(Ytilde, Xtilde, Sigma, p, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  n <- ncol(Ytilde)
  k <- ncol(Xtilde)
  
  # Posterior mean (OLS with augmented data)
  # A_hat = (X'X)^{-1} X'Y
  
  XtX <- t(Xtilde) %*% Xtilde
  XtY <- t(Xtilde) %*% Ytilde
  
  # Add ridge for numerical stability (larger if needed)
  ridge <- max(1e-4, 1e-6 * max(diag(XtX)))
  XtX_inv <- solve(XtX + diag(ridge, k))
  
  A_hat <- XtX_inv %*% XtY  # k x n
  
  # Posterior covariance: Sigma ⊗ (X'X)^{-1}
  # Sample vec(A) from N(vec(A_hat), Sigma ⊗ (X'X)^{-1})
  
  # Using vec trick: vec(A) ~ N(vec(A_hat), Sigma ⊗ (X'X)^{-1})
  # Sample: vec(A) = vec(A_hat) + chol(Sigma ⊗ (X'X)^{-1}) * z
  
  # Efficient sampling: A = A_hat + (X'X)^{-1} * Z * chol(Sigma)'
  # where Z ~ N(0, I_k x n)
  
  Z <- matrix(rnorm(k * n), k, n)
  Sigma_chol <- t(chol(Sigma))
  
  A_draw <- A_hat + XtX_inv %*% Z %*% Sigma_chol
  
  # Convert to list of lag matrices
  A_list <- unpack_A(A_draw, n, p)
  
  return(list(
    A_list = A_list,
    A_matrix = A_draw
  ))
}

#' Pack list of coefficient matrices into a single matrix
#'
#' @param A_list List of n x n matrices (A_1, ..., A_p) and intercept
#' @return Matrix of coefficients (k x n) where k = n*p + 1
#' @export
pack_A <- function(A_list) {
  
  p <- length(A_list) - 1  # Last element is intercept
  n <- nrow(A_list[[1]])
  
  A_matrix <- matrix(0, n * p + 1, n)
  
  for (i in 1:p) {
    A_matrix[((i-1)*n + 1):(i*n), ] <- A_list[[i]]
  }
  
  # Intercept in last row
  A_matrix[n * p + 1, ] <- A_list[[p + 1]]
  
  return(A_matrix)
}

#' Unpack coefficient matrix into list of lag matrices
#'
#' @param A_matrix Matrix of coefficients (k x n) where k = n*p + 1
#' @param n Number of variables
#' @param p VAR lag order
#' @return List with A_1, ..., A_p (each n x n) and intercept (1 x n)
#' @export
unpack_A <- function(A_matrix, n, p) {
  
  A_list <- list()
  
  for (i in 1:p) {
    A_list[[i]] <- A_matrix[((i-1)*n + 1):(i*n), , drop = FALSE]
  }
  
  # Intercept
  if (nrow(A_matrix) >= n * p + 1) {
    A_list[[p + 1]] <- A_matrix[n * p + 1, , drop = FALSE]
  } else {
    A_list[[p + 1]] <- matrix(0, 1, n)
  }
  
  return(A_list)
}

#' Draw latent monthly states using simulation smoother
#'
#' @param y_obs Data matrix on monthly grid (T x n), with NAs for unobserved
#' @param calendar Calendar object
#' @param meta Metadata list
#' @param A_list List of VAR coefficient matrices
#' @param Sigma VAR innovation covariance
#' @param p VAR lag order
#' @param a0 Initial state mean
#' @param P0 Initial state covariance
#' @param seed Random seed
#' @return List with alpha (states) and y_latent (extracted monthly levels)
#' @export
draw_states <- function(y_obs, calendar, meta, A_list, Sigma, p, a0, P0, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  n <- ncol(y_obs)
  T_total <- nrow(y_obs)
  series_names <- colnames(y_obs)
  if (is.null(series_names)) series_names <- paste0("V", 1:n)
  
  # Build companion form
  comp <- companion_blocks(A_list[1:p], Sigma)
  
  # Build Z_t and H_t for each time period
  # For state space model with missing data, we need to specify which observations are available
  
  # Use a simplified approach: treat all observations as potentially available,
  # and use NA in the observation vector to indicate missing
  
  # Build fixed Z and H assuming all observations present
  Z_fixed <- matrix(0, n, n * p)
  Z_fixed[1:n, 1:n] <- diag(n)  # Observe first block of state (y_t)
  
  H_fixed <- diag(1e-10, n)  # Near-zero measurement error
  
  # Adjust Z for quarterly variables (need averaging)
  month_index <- 1:T_total
  
  # For quarterly variables, we need time-varying Z
  # This is complex with KFAS; use a different approach
  
  # Simplified: Build observation equation that handles quarterly averaging
  # by expanding the observation vector
  
  # Alternative: use the fact that quarterly = average of 3 monthly
  # Construct Z_t for each t individually
  
  Z_list <- list()
  H_list <- list()
  y_vec_list <- list()
  
  for (t in 1:T_total) {
    Z_t <- matrix(0, 0, n * p)
    y_t <- numeric(0)
    n_obs_t <- 0
    
    for (i in 1:n) {
      v <- series_names[i]
      
      if (!is.na(y_obs[t, i])) {
        if (meta$freq[[v]] == "monthly") {
          # Add monthly observation
          z_row <- rep(0, n * p)
          z_row[i] <- 1
          Z_t <- rbind(Z_t, z_row)
          y_t <- c(y_t, y_obs[t, i])
          n_obs_t <- n_obs_t + 1
          
        } else if (meta$freq[[v]] == "quarterly") {
          # Add quarterly observation (average of 3 months)
          z_row <- rep(0, n * p)
          z_row[i] <- 1/3
          if (p >= 2) z_row[n + i] <- 1/3
          if (p >= 3) z_row[2*n + i] <- 1/3
          
          # Adjust if p < 3
          if (p == 1) z_row[i] <- 1
          if (p == 2) {
            z_row[i] <- 0.5
            z_row[n + i] <- 0.5
          }
          
          Z_t <- rbind(Z_t, z_row)
          y_t <- c(y_t, y_obs[t, i])
          n_obs_t <- n_obs_t + 1
        }
      }
    }
    
    # Build H_t based on number of observations
    if (n_obs_t > 0) {
      H_t <- diag(1e-10, n_obs_t)
    } else {
      H_t <- matrix(1, 1, 1)
    }
    
    if (length(y_t) == 0) {
      # No observations at time t
      Z_t <- matrix(0, 1, n * p)
      H_t <- matrix(1, 1, 1)
      y_t <- NA
    }
    
    Z_list[[t]] <- Z_t
    H_list[[t]] <- H_t
    y_vec_list[[t]] <- y_t
  }
  
  # Build KFAS model - this is complex with time-varying dimensions
  # Use simulation smoother with custom model
  
  # Simplified approach: use filtering and simulation
  # Since KFAS requires fixed dimensions, pad observations
  
  max_obs <- max(sapply(Z_list, nrow))
  
  Z_array <- array(0, dim = c(max_obs, n * p, T_total))
  H_array <- array(1e6, dim = c(max_obs, max_obs, T_total))  # Large but under KFAS limit of 1e7
  y_matrix <- matrix(NA, T_total, max_obs)
  
  for (t in 1:T_total) {
    n_obs_t <- length(y_vec_list[[t]])
    if (n_obs_t > 0 && !any(is.na(y_vec_list[[t]]))) {
      Z_array[1:n_obs_t, , t] <- Z_list[[t]]
      H_array[1:n_obs_t, 1:n_obs_t, t] <- H_list[[t]]
      y_matrix[t, 1:n_obs_t] <- y_vec_list[[t]]
    }
  }
  
  # Build SSModel
  model <- KFAS::SSModel(
    y_matrix ~ -1 + SSMcustom(
      Z = Z_array,
      T = comp$Tmat,
      R = comp$Rmat,
      Q = comp$Qmat,
      a1 = a0,
      P1 = P0
    ),
    H = H_array
  )
  
  # Simulate states
  alpha <- sim_smoother_states(model, nsim = 1, seed = seed)[, , 1]
  
  # Extract y_latent (first n components of state at each t)
  y_latent <- t(alpha[1:n, ])
  colnames(y_latent) <- series_names
  
  return(list(
    alpha = alpha,
    y_latent = y_latent
  ))
}
