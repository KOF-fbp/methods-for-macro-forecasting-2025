#' Build Minnesota prior dummy observations
#'
#' @param Y Matrix of dependent variables (T x n)
#' @param X Matrix of regressors (T x k) where k = n*p + 1 (with intercept)
#' @param p VAR lag order
#' @param hyper List with hyperparameters: lambda (tightness), lag_decay, cross_eq, intercept_weight
#' @return List with Ytilde and Xtilde (augmented data with dummies)
#' @export
build_dummies <- function(Y, X, p, hyper) {
  
  # Extract hyperparameters with defaults
  lambda <- hyper$lambda
  lag_decay <- ifelse(is.null(hyper$lag_decay), 1, hyper$lag_decay)
  cross_eq <- ifelse(is.null(hyper$cross_eq), 0.5, hyper$cross_eq)
  intercept_weight <- ifelse(is.null(hyper$intercept_weight), 1, hyper$intercept_weight)
  
  n <- ncol(Y)
  
  # Estimate scale of each variable (using sample variance)
  sigma <- apply(Y, 2, sd, na.rm = TRUE)
  sigma[sigma == 0] <- 1  # Avoid division by zero
  
  # Sample mean for intercept prior
  y_mean <- apply(Y, 2, mean, na.rm = TRUE)
  
  # Dummy observations for Minnesota prior
  # Following Banbura, Giannone, Reichlin (2010)
  
  # 1. Dummy for own lags (random walk prior)
  # Y_d1 = diag(sigma / lambda), X_d1 = [diag(sigma / lambda), 0, ..., 0]
  
  Y_d1 <- diag(sigma / lambda, n)
  X_d1 <- matrix(0, n, n * p + 1)
  X_d1[1:n, 1:n] <- diag(sigma / lambda, n)
  
  # 2. Dummies for cross-equation shrinkage
  # For each lag l = 1, ..., p
  
  Y_d2_list <- list()
  X_d2_list <- list()
  
  for (l in 1:p) {
    # Shrinkage factor for lag l
    weight_l <- (lambda * cross_eq) / (l^lag_decay)
    
    Y_d2_l <- matrix(0, n, n)
    X_d2_l <- matrix(0, n, n * p + 1)
    
    # Diagonal block for lag l
    X_d2_l[1:n, ((l-1)*n + 1):(l*n)] <- diag(sigma * weight_l, n)
    
    Y_d2_list[[l]] <- Y_d2_l
    X_d2_list[[l]] <- X_d2_l
  }
  
  Y_d2 <- do.call(rbind, Y_d2_list)
  X_d2 <- do.call(rbind, X_d2_list)
  
  # 3. Dummy for intercept (prior centered at sample mean)
  # Y_d3 = y_mean * intercept_weight
  # X_d3 = [0, ..., 0, intercept_weight]
  
  Y_d3 <- matrix(y_mean * intercept_weight, 1, n)
  X_d3 <- matrix(0, 1, n * p + 1)
  X_d3[1, n * p + 1] <- intercept_weight
  
  # Combine all dummies
  Ytilde <- rbind(Y_d1, Y_d2, Y_d3, Y)
  Xtilde <- rbind(X_d1, X_d2, X_d3, X)
  
  return(list(
    Ytilde = Ytilde,
    Xtilde = Xtilde,
    n_dummies = nrow(Y_d1) + nrow(Y_d2) + nrow(Y_d3)
  ))
}
