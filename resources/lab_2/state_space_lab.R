
# State space models

rm(list=ls()) # ensure empty workspace

# Loading packages
library(MASS) # package MASS has a function for drawing from the multivariate normal
library(scales) # allows for shaded polygon in plots
library(BVAR) # for FRED data

set.seed(1)

# First we define the forward-filtering-backward-sampling procedure which allows
# us to draw from the conditional posterior of the states

ffbs <- function(ydata,r,m,Hh,Ff,R,Q,const,alpha00,P00){

  # measurement equation: y(t) = Hh alpha(t) + w(t), w ~ N(0,R)
  # state equation:       alpha(t+1) = c + Ff alpha(t) + v(t),  v ~ N(0,Q)

  ## Input
  # - ydata: TxN observation vector
  # - r: number of states
  # - m: size of state vector (m = r*p, where p is the number of lags in state equation)
  # - Parameters: Hh (Nxr), Ff (rxrp or rx(rp+1)), R, Q, const (1 or 0, indicates whether first column of Ff are intercepts)
  # - Starting values: alpha00, P00

  ## Output
  # - alpha(t) estimates, i.e. Txr matrix

  # Preparation --------------------------------------------------------
  # Bring parameters into state space form
  Tt <- dim(ydata)[1] # Time
  N <- dim(ydata)[2] # Number of observable variables
  p <- m/r # lags in state equation

  if(const==1){
    cc <- Ff[,1]
    ccComp <- c(cc,rep(0,m-r))
    Ff <- Ff[,-1]
  } else if(const==0 & m>1) {
    cc <- rep(0, r)
    ccComp <- c(cc,rep(0,m-r))
  } else {
    cc <- 0
    ccComp <- 0
  }

  HhComp <- cbind(Hh, matrix(0, ncol = m-r, nrow = N))
  FfComp <- rbind(Ff, cbind(diag(1, nrow = m-r), matrix(0, nrow = m-r, ncol = r)))
  Qcomp <- matrix(0, ncol = m, nrow = m)
  Qcomp[1:r,1:r] <- Q


  # Preallocate alphatt, Ptt, and Kalman gain
  alphatt <- vector("list", length = Tt+1)
  alphatt[[1]] <- alpha00
  Ptt <- vector("list", length = Tt+1)
  Ptt[[1]] <- P00

  # Forward-filtering (Kalman filter) ---------------------------------------
  for(t in 1:Tt){
    # Prediction
    alpha10 <- FfComp%*%alphatt[[t]] + ccComp
    P10 <- FfComp%*%Ptt[[t]]%*%t(FfComp) + Qcomp
    nu10 <- ydata[t,]-HhComp%*%alpha10 # prediction error
    nu10_var <- HhComp%*%P10%*%t(HhComp) + R # covariance matrix of prediction error
    # Updating
    KG <- P10%*%t(HhComp)%*%solve(nu10_var) # Kalman gain
    alphatt[[t+1]] <- alpha10 + KG%*%nu10
    Ptt[[t+1]] <- P10 - KG%*%HhComp%*%P10
  }

  # Backward sampling -------------------------------------------------------
  # Here we leave the companion form such that we can use Q (which is positive definite), and not Qcomp
  Ff <- FfComp[1:m,]

  alphat <- vector("list", length = Tt) # For storing final factors
  alphat[[Tt]] <- as.matrix(mvrnorm(n = 1, mu = alphatt[[Tt+1]], Sigma = Ptt[[Tt+1]])) # Drawing the factor for T
  # Now we draw factors for t = T-1,...,1
  for(t in (Tt-1):1){
    # First we define the mean vector and the covariance matrices to draw from
    KGtt <- Ptt[[t+1]]%*%t(Ff)%*%solve(Ff%*%Ptt[[t+1]]%*%t(Ff)+Q)
    Ptt_alpha <- Ptt[[t+1]] - KGtt %*% Ff%*%Ptt[[t+1]]
    alphatt_alpha <- alphatt[[t+1]] + KGtt %*% (alphat[[t+1]][1:m]-Ff%*%alphatt[[t+1]]-cc) # Here only the first m elements of alphat[[t+1]] needs to be taken
    # Draw factors
    alphat[[t]] <- as.matrix(mvrnorm(n = 1, mu = alphatt_alpha, Sigma = Ptt_alpha))
  }

  # Output ------------------------------------------------------------------
  # Convert factors to matrix and Keep the first m rows
  alphat <- do.call(cbind, alphat)
  alphat <- t(matrix(alphat[1:m,], nrow = m))

  return(alphat)
}



#### Example 1: Dynamic factor model


## First we simulate data
set.seed(42)
Tt <- 200
n  <- 6
phi_true <- 0.8
q_true   <- 0.5^2
sig_i_true <- runif(n, 0.1, 0.4)^2        # variances
lambda_true <- c(1.0, 0.9, 0.6, -0.4, 0.3, -0.2)  # lambda_1 fixed to 1 for ident.
c_true <- seq(-0.5, 0.5, length.out = n)

f <- numeric(Tt)
f[1] <- rnorm(1, 0, sqrt(q_true/(1-phi_true^2)))
for (t in 2:Tt) f[t] <- phi_true*f[t-1] + rnorm(1, 0, sqrt(q_true))

Y <- sapply(1:n, function(i) c_true[i] + lambda_true[i]*f + rnorm(Tt, 0, sqrt(sig_i_true[i])))
colnames(Y) <- paste0("y", 1:n)

##  Priors
b0 <- c(0, 0)
B0 <- diag(c(10, 10))
# For i=1: c1 ~ N(m0_c1, V0_c1)
m0_c1 <- 0; V0_c1 <- 10

# Idiosyncratic variances
a0_sig <- 3
b0_sig <- 100   # vague

# Factor AR(1)
m0_phi <- 0
V0_phi <- 1
a0_q <- 3
b0_q <- 0.1

## Initialization
# Start with PCA for a crude factor and loadings (demean per series)
Y_dm <- scale(Y, center=TRUE, scale=FALSE)
pc <- prcomp(Y_dm, rank.=1)
f_draw <- as.numeric(pc$x[,1])
# scale so that loading on series 1 is 1
scale_fac <- pc$rotation[1,1]
f_draw <- f_draw * scale_fac

# Regress each series on factor to initialize c_i and lambda_i
ols <- sapply(1:n, function(i) coef(lm(Y[,i] ~ f_draw)))
c_draw <- ols[1,]
lambda_draw <- ols[2,]
lambda_draw[1] <- 1.0  # enforce identification
# Idiosyncratic variances
e <- Y - (matrix(c_draw, Tt, n, byrow=TRUE) + outer(f_draw, lambda_draw))
sig2_draw <- apply(e, 2, var)

# AR parameters
phi_draw <- coef(lm(f_draw[-1] ~ 0 + f_draw[-Tt]))[1]
q_draw   <- var(f_draw[-1] - phi_draw*f_draw[-Tt])

# FFBS settings
alpha00 <- matrix(0,1,1)
P00     <- matrix(1e6,1,1)   # diffuse

#### Gibbs settings
draws   <- 2000
burn_in <- 500

# Storage
f_store       <- matrix(NA, Tt, draws)
c_store       <- matrix(NA, n, draws)
lambda_store  <- matrix(NA, n, draws)
sig2_store    <- matrix(NA, n, draws)
phi_store     <- numeric(draws)
q_store       <- numeric(draws)

# Initialize stores
f_store[,1]      <- f_draw
c_store[,1]      <- c_draw
lambda_store[,1] <- lambda_draw
sig2_store[,1]   <- sig2_draw
phi_store[1]     <- phi_draw
q_store[1]       <- q_draw

## Sample truncated normal for phi
rnorm_trunc_phi <- function(mean, sd){
  # Simple rejection to enforce |phi|<1 (works fine here)
  val <- rnorm(1, mean, sd)
  while(abs(val) >= 1) val <- rnorm(1, mean, sd)
  val
}

## Gibbs loop
pb <- txtProgressBar(min=1, max=draws, style=3)
for (it in 2:draws){

  # Draw factor f_{1:T} | Y, c, lambda, phi, q, sigma^2
  y_tilde <- Y - matrix(c_draw, Tt, n, byrow=TRUE)  # remove intercepts to match obs eq with no constant
  Hh <- matrix(lambda_draw, nrow=n, ncol=1)
  Ff <- matrix(phi_draw, 1, 1)
  R  <- diag(sig2_draw, n, n)
  Q  <- matrix(q_draw, 1, 1)
  f_draw <- ffbs(ydata = y_tilde, r=1, m=1, Hh=Hh, Ff=Ff, R=R, Q=Q,
                 const=0, alpha00=alpha00, P00=P00)[,1]

  # Draw (c1) | f, sigma1^2  (lambda1 fixed = 1)
  X1 <- matrix(1, nrow=Tt, ncol=1)
  y1 <- Y[,1] - lambda_draw[1]*f_draw
  Vn1 <- 1 / (1/V0_c1 + sum(X1^2)/sig2_draw[1])
  mn1 <- Vn1 * (m0_c1/V0_c1 + sum(X1*y1)/sig2_draw[1])
  c_draw[1] <- rnorm(1, mn1, sqrt(Vn1))

  # Draw (c_i, lambda_i) | f, sigma_i^2 for i = 2..n
  X <- cbind(1, f_draw)
  B0_inv <- solve(B0)
  XtX <- crossprod(X)
  Xt <- t(X)
  for (i in 2:n){
    yi <- Y[,i]
    SigInv <- 1/sig2_draw[i]
    Vn <- solve(B0_inv + SigInv*XtX)
    mn <- Vn %*% (B0_inv %*% b0 + SigInv*Xt %*% yi)
    beta_i <- as.numeric(mvrnorm(1, mn, Vn))
    c_draw[i] <- beta_i[1]
    lambda_draw[i] <- beta_i[2]
  }
  lambda_draw[1] <- 1.0  # keep identification

  # Draw sigma_i^2 | residuals (Inverse-Gamma) for all i
  for (i in 1:n){
    mu_i <- c_draw[i] + lambda_draw[i]*f_draw
    resid <- Y[,i] - mu_i
    a1 <- Tt/2 + a0_sig
    b1 <- 1/b0_sig + sum(resid^2)/2
    sig2_draw[i] <- 1/rgamma(1, shape=a1, rate=b1)
  }

  # Draw (phi, q) | f (AR(1) regression with conjugate priors)
  f_lag <- f_draw[-Tt]
  f_cur <- f_draw[-1]
  # q | phi, f
  a_q <- a0_q + (Tt-1)/2
  b_q <- b0_q + 0.5*sum((f_cur - phi_draw*f_lag)^2)
  q_draw <- 1/rgamma(1, shape=a_q, rate=b_q)
  # phi | q, f  (Normal, then truncate to |phi|<1)
  Vn_phi <- 1 / (1/V0_phi + sum(f_lag^2)/q_draw)
  mn_phi <- Vn_phi * (m0_phi/V0_phi + sum(f_lag*f_cur)/q_draw)
  phi_draw <- rnorm_trunc_phi(mn_phi, sqrt(Vn_phi))


  # Store
  f_store[,it]      <- f_draw
  c_store[,it]      <- c_draw
  lambda_store[,it] <- lambda_draw
  sig2_store[,it]   <- sig2_draw
  phi_store[it]     <- phi_draw
  q_store[it]       <- q_draw

  setTxtProgressBar(pb, it)
}
close(pb)

## Posterior summaries
keep <- (burn_in+1):draws
post_med <- function(x) apply(x[,keep,drop=FALSE], 1, median)

lambda_med <- post_med(lambda_store)
c_med      <- post_med(c_store)
sig2_med   <- post_med(sig2_store)
phi_med    <- median(phi_store[keep])
q_med      <- median(q_store[keep])
f_med      <- apply(f_store[,keep,drop=FALSE], 1, median)

cat("\n--- Posterior medians (vs. true) ---\n")
print(round(cbind(
  lambda_est=lambda_med,
  lambda_true=lambda_true
), 3))
print(round(cbind(
  c_est=c_med,
  c_true=c_true
), 3))
print(round(c(phi_est=phi_med, phi_true=phi_true), 3))
print(round(c(q_est=q_med, q_true=q_true), 3))
print(round(cbind(sig2_est=sig2_med, sig2_true=sig_i_true), 3))

## Quick plots
par(mfrow=c(2,1), mar=rep(2,4))
plot(f, type="l", lwd=2, main="Factor: true (black) vs posterior median (blue)")
lines(f_med, lwd=2, lty=2, col="#215CAF")
legend("topleft", c("True f_t","Posterior median"), lty=c(1,2), lwd=2, col=c("black","#215CAF"))

matplot(Y, type="l", main="Observed panel (simulated)")



### Example 2: TVP regression

# First we simulate a TVP-AR(p)
set.seed(1)
p <- 2
Q_sim <- diag(0.002, p+1)
sigma2_sim <- 0.01
Tt <- 200+p

y <- rep(0,Tt)
X <- matrix(0,ncol=p+1,nrow=Tt)
beta_sim <- matrix(0.1,nrow=Tt,ncol=p+1)
for(t in (p+1):Tt){
  # coefficients follow a random-walk
  beta_sim[t,] <- beta_sim[t-1,] + mvrnorm(1,rep(0,p+1),Q_sim)
  # simulate y
  X[t,] <- c(1,rev(y[(t-p):(t-1)]))
  y[t] <- beta_sim[t,]%*%X[t,] + rnorm(1,0,sqrt(sigma2_sim))
}

par(mfrow = c(2,1), mar=rep(2,4))
matplot(beta_sim, type = "l", lty = 1, col = 1:ncol(beta_sim),
        main = "Time-varying coefficients")
# build legend labels
labels <- c("Constant", paste0("Beta", 1:(ncol(beta_sim)-1)))
legend("topright", legend = labels,
       col = 1:ncol(beta_sim), lty = 1, cex = 0.8, bty = "n")
plot(y, type = "l", main = "Observed")



# Cut the initial values out
y <- y[(p+1):Tt]
X <- X[(p+1):Tt,]
Tt <- Tt-p

# Define the model specification
Ff <- diag(p)

# Define priors for inverse Gamma
a0 <- 3
b0 <- 100

# Define draws and burn-in
draws <- 2000
burn_in <- 100

# Define initial values
alpha00 <- rep(0,p+1)
P00 <- diag(1,p+1)
sigma200 <- 1
Q00 <- diag(p+1)

# Preallocate
alpha <- array(0, dim=c(Tt,p+1,draws))
sigma2 <- rep(0, draws)
Q <- array(0,dim=c(p+1,p+1,draws))

Q[,,1] <- Q00
sigma2[1] <- sigma200

# Forward filtering backward sampling for (univariate) time-varying coefficients
# that follow a  random-walk
ffbs_tvp <- function(ydata,x,sigma2,Q,alpha00,P00){
  # measurement equation: y(t) = x(t)'alpha(t) + w(t), w ~ N(0,sigma2)
  # state equation:       alpha(t+1) = alpha(t) + v(t),  v ~ N(0,Q)

  ## Input
  # - ydata: TxN observation vector
  # - Parameters: sigma2, Q
  # - Starting values: alpha00, P00

  ## Output
  # - alpha(t) estimates, i.e. Txr matrix

  # Preallocate alphatt, Ptt, and Kalman gain
  alphatt <- vector("list", length = Tt+1)
  alphatt[[1]] <- alpha00
  Ptt <- vector("list", length = Tt+1)
  Ptt[[1]] <- P00

  # Forward-filtering (Kalman filter)
  for(t in 1:Tt){
    # Prediction
    alpha10 <- alphatt[[t]]
    P10 <- Ptt[[t]] + Q
    nu10 <- y[t] - t(x[t,])%*%alpha10 # prediction error
    nu10_var <- t(x[t,])%*%P10%*%t(t(x[t,])) + sigma2 # covariance matrix of prediction error
    # Updating
    KG <- P10%*%t(t(x[t,]))%*%solve(nu10_var) # Kalman gain
    alphatt[[t+1]] <- alpha10 + KG%*%nu10
    Ptt[[t+1]] <- P10 - KG%*%t(x[t,])%*%P10
  }

  ### Backward sampling
  alphat <- vector("list", length = Tt) # For storing final factors
  alphat[[Tt]] <- as.matrix(mvrnorm(n = 1, mu = alphatt[[Tt+1]], Sigma = Ptt[[Tt+1]])) # Drawing the factor for T
  # Now we draw factors for t = T-1,...,1
  for(t in (Tt-1):1){
    # First we define the mean vector and the covariance matrices to draw from
    KGtt <- Ptt[[t+1]]%*%solve(Ptt[[t+1]] + Q)
    Ptt_alpha <- Ptt[[t+1]] - KGtt%*%Ptt[[t+1]]
    alphatt_alpha <- alphatt[[t+1]] + KGtt%*%(alphat[[t+1]]-alphatt[[t+1]]) # Here only the first m elements of alphat[[t+1]] needs to be taken

    # Draw states
    alphat[[t]] <- as.matrix(mvrnorm(n = 1, mu = alphatt_alpha, Sigma = Ptt_alpha))
  }

  # Convert factors to matrix
  alphat <- do.call(cbind, alphat)
  return(t(alphat))

}

# Gibbs sampler
pb <- txtProgressBar(min = 1, max = draws, style = 3) # shows progress
for(i in 2:draws){

  # draw states conditional on y, sigma2, Q
  alpha[,,i] <- ffbs_tvp(y, X, sigma2[i-1], Q = Q[,,i-1], alpha00, P00)

  # draw sigma2 conditional on y, Q, alpha
  a1 <- Tt/2 + a0
  b1 <- 1/b0 + sum(sapply(1:Tt, function(t) (y[t]-t(X[t,])%*%alpha[t,,i])^2))/2
  sigma2[i] <- 1/rgamma(1, shape = a1, rate = b1)

  # draw diagonal elements of Q separately conditional on y, sigma2, alpha
  # (using the same prior values for the inverse gamma as for drawing sigma2)
  for(k in 1:(p+1)){
    c1 <- Tt/2 + a0
    d1 <- 1/b0 + t(alpha[2:Tt,k,i] - alpha[1:(Tt-1),k,i]) %*% (alpha[2:Tt,k,i] - alpha[1:(Tt-1),k,i])/2
    Q[k,k,i] <- 1/rgamma(1, shape = c1, rate = d1)
  }

  # Show progress
  setTxtProgressBar(pb, i)
}

# Discard burn-in
alpha <- alpha[,,burn_in:draws]
Q <- Q[,,burn_in:draws]
sigma2 <- sigma2[burn_in:draws]

# Take percentiles
alpha_perc <- apply(alpha, 1:2, quantile, c(0.05,0.5,0.95))
sigma2_perc <- quantile(sigma2, c(0.05,0.5,0.95))
Q_perc <- apply(Q, 1:2, quantile, c(0.05,0.5,0.95))

# Compare median of diagonal elements of Q to simulated values
diag(Q_perc[2,,]) # estimated
diag(Q_sim) # simulated

# Compare median of sigma2 to simulated values
sigma2_perc[2] # estimated
sigma2_sim # simulated

# Compare estimated and simulated time-varying parameters (first is intercept, then autoregressive coefficients)
par(mfrow = c(3,1), mar=rep(2,4))
for(i in 1:(p+1)){
  ylim <- c(min(alpha_perc[1,,i], beta_sim[,i]),max(alpha_perc[3,,i], beta_sim[,i]))
  matplot(t(alpha_perc[,,i]), type = "l", lwd = c(1,2,1), col = rep("blue",3), lty = c(2,1,2), ylim = ylim)
  lines(beta_sim[,i], lwd = 2, col = "black")
  if(i == 1){
    legend("topright", legend = c("Estimated", "Simulated"), lwd = 2, col = c("blue", "black"))
  }
}

# Conclusion: The simulated TVP-AR(p) model seems to be correctly estimated.

