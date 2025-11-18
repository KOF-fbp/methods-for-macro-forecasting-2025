#' Build companion form matrices for a VAR(p)
#'
#' @param A_list List of coefficient matrices A_1, ..., A_p (each n x n)
#' @param Sigma_u Covariance matrix of innovations (n x n)
#' @return List with Tmat (transition), Rmat (selection), Qmat (state innovation cov)
#' @export
companion_blocks <- function(A_list, Sigma_u) {
  
  p <- length(A_list)
  n <- nrow(A_list[[1]])
  
  # Companion form: state is (y_t, y_{t-1}, ..., y_{t-p+1})'
  # Dimension: n*p x 1
  
  # Transition matrix T
  # Top block: [A_1, A_2, ..., A_p]
  # Lower blocks: identity and zeros for lagged states
  
  Tmat <- matrix(0, n * p, n * p)
  
  # Fill top row blocks
  for (i in 1:p) {
    Tmat[1:n, ((i-1)*n + 1):(i*n)] <- A_list[[i]]
  }
  
  # Fill lower identity blocks
  if (p > 1) {
    for (i in 1:(p-1)) {
      Tmat[(i*n + 1):((i+1)*n), ((i-1)*n + 1):(i*n)] <- diag(n)
    }
  }
  
  # Selection matrix R: innovations only affect y_t, not lagged terms
  Rmat <- matrix(0, n * p, n)
  Rmat[1:n, 1:n] <- diag(n)
  
  # State innovation covariance Q
  Qmat <- Sigma_u
  
  return(list(
    Tmat = Tmat,
    Rmat = Rmat,
    Qmat = Qmat
  ))
}

#' Build KFAS state space model
#'
#' @param Z_list List of observation matrices Z_t (can vary by time)
#' @param H_list List of measurement error covariance matrices H_t
#' @param Tmat Transition matrix (n_state x n_state)
#' @param Rmat Selection matrix (n_state x n_shock)
#' @param Qmat State innovation covariance (n_shock x n_shock)
#' @param a0 Initial state mean
#' @param P0 Initial state covariance
#' @return SSModel object from KFAS
#' @export
kf_build_model <- function(Z_list, H_list, Tmat, Rmat, Qmat, a0, P0) {
  
  # KFAS requires specific array structures
  # For time-varying Z and H, we need arrays
  
  T_obs <- length(Z_list)
  n_state <- nrow(Tmat)
  
  # Determine observation dimension (can vary by time, use max)
  n_obs_vec <- sapply(Z_list, nrow)
  n_obs_max <- max(n_obs_vec)
  
  # Build arrays
  # Z: (n_obs_max, n_state, T_obs)
  # H: (n_obs_max, n_obs_max, T_obs)
  
  Z_array <- array(0, dim = c(n_obs_max, n_state, T_obs))
  H_array <- array(0, dim = c(n_obs_max, n_obs_max, T_obs))
  
  for (t in 1:T_obs) {
    n_t <- nrow(Z_list[[t]])
    if (n_t > 0) {
      Z_array[1:n_t, , t] <- Z_list[[t]]
      H_array[1:n_t, 1:n_t, t] <- H_list[[t]]
    }
  }
  
  # Create observation vector (will be filled later)
  y <- matrix(NA, T_obs, n_obs_max)
  
  # Build model using KFAS
  # Note: KFAS uses a different parameterization
  # We'll construct a custom model
  
  model <- KFAS::SSModel(
    y ~ -1 + SSMcustom(
      Z = Z_array,
      T = Tmat,
      R = Rmat,
      Q = Qmat,
      a1 = a0,
      P1 = P0,
      P1inf = diag(0, n_state)
    ),
    H = H_array
  )
  
  return(model)
}

#' Run Kalman filter and smoother
#'
#' @param model SSModel object
#' @return List with filtered and smoothed states
#' @export
kf_filter_smooth <- function(model) {
  
  # Run Kalman filter and smoother
  kfs_out <- KFAS::KFS(model, filtering = "state", smoothing = "state")
  
  return(list(
    filtered = list(
      a = kfs_out$a,      # Filtered states
      P = kfs_out$P,      # Filtered covariances
      logLik = kfs_out$logLik
    ),
    smoothed = list(
      alphahat = kfs_out$alphahat,  # Smoothed states
      V = kfs_out$V                  # Smoothed covariances
    )
  ))
}

#' Simulate states using simulation smoother
#'
#' @param model SSModel object
#' @param nsim Number of simulations
#' @param seed Random seed
#' @return Array of simulated states (n_state x T x nsim)
#' @export
sim_smoother_states <- function(model, nsim = 1, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # Use KFAS simulation smoother
  sims <- KFAS::simulateSSM(model, type = "states", nsim = nsim)
  
  # Handle different output formats
  if (is.null(dim(sims))) {
    # Vector output - shouldn't happen but handle it
    sims <- matrix(sims, ncol = 1)
  }
  
  if (nsim == 1 && length(dim(sims)) == 2) {
    # sims is (T, n_state) - convert to (n_state, T, 1)
    return(array(t(sims), dim = c(ncol(sims), nrow(sims), 1)))
  } else if (length(dim(sims)) == 3) {
    # sims is (T, n_state, nsim) - rearrange to (n_state, T, nsim)
    return(aperm(sims, c(2, 1, 3)))
  } else {
    # Fallback
    return(array(t(as.matrix(sims)), dim = c(ncol(as.matrix(sims)), nrow(as.matrix(sims)), 1)))
  }
}
