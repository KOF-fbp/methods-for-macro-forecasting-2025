# function to draw from the posterior
independent_normal_regression <- function(y, X, draws, beta0, Q0, a0, b0){
  # This function returns the posterior for the coefficient vector beta as well as the 
  # variance sigma^2 for the linear regression model y = Xb + e with an independent 
  # normal prior on the coefficients and an inverse-gamma prior on the variance. The prior 
  # specification comes from "beta0, Q0, nu0 and s0". "y" is the data vector , "X" the 
  # regressor matrix, and "draws" denotes how many draws from the posterior are drawn.
  
  # Number of draws
  R <- draws
  
  # Time series length
  Tt <- length(y)
  
  # Pre-allocate space
  betas <- matrix(0, length(beta0), R)
  sigma2 <- rep(0, R)
  
  # Starting value
  sigma2[1] <- 1
  
  # Start the Gibbs sampler
  for(r in seq(2, R)){ 
    # Posterior mean and variance of conditional distribution for beta
    Q1 <- solve(t(X) %*% X/sigma2[r-1] + solve(Q0)) 
    beta1 <- t(X) %*% y/sigma2[r-1] + solve(Q0) %*% beta0
    
    # Draw beta from conditional normal distribution
    betas[,r] <- mvrnorm(1, Q1 %*% beta1, Q1) # multivariate
    
    # Posterior shape and scale 
    a1 <- Tt/2 + a0
    b1 <- 1/b0 + t(y - X %*% betas[,r]) %*% (y - X %*% betas[,r])/2
    
    # Draw sigma^2 from conditional inverse gamma distribution (we actually draw first from gamma and then take the inverse)
    sigma2[r] <- 1/rgamma(1, shape = a1, rate = b1)
  }
  
  
  # Return posterior draws as list
  return(list("betas" = betas, "sigma2" = sigma2))
}