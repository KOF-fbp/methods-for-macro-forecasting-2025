
# # ============================================
# # STATE SPACE: Dynamic Factor Model Initialization (PCA/SVD → F0, Lambda0, R0, A0, Q0)
# # Compatible with KFAS: x_t = Λ f_t + e_t ; f_t = A f_{t-1} + u_t
# # ============================================
# #In our workflow, we currently have roughly these main parts:
# 
# # Data loading + cleaning
# 
# # Transformations (logdiff, diff, etc.)
# 
# # Standardization (df_train_std, vars_X, vars_Y)
# 
# # Number of factors selection (Bai–Ng, ICp3) → gives r_fixed
# 
# # now we switch to STATE SPACE MODEL
# 
# 
# 
# dfm_init <- function(df_train_std, vars_X, r_fixed, ar_cap = 0.98) {
#   stopifnot("date" %in% names(df_train_std))
#   X_std <- as.matrix(df_train_std[, vars_X, drop = FALSE])
#   Tn <- nrow(X_std); Nn <- ncol(X_std)
#   if (Tn < 5 || Nn < 1) stop("X_std has insufficient dimensions.")
#   
#   # --- 1) Prepare matrix for SVD: replace NA with 0 (mean=0 after standardization) ---
#   X_svd <- X_std
#   X_svd[!is.finite(X_svd)] <- 0
#   
#   # --- 2) SVD on standardized X (already zero mean) ---
#   #     Decomposition: X ≈ U D V'
#   #     Factors and loadings (Stock–Watson convention):
#   #     F0 = sqrt(T) * U_r         ;  Lambda0 = V_r * (D_r / sqrt(T))
#   sv <- svd(X_svd)  # X_svd = U D V'
#   r <- max(1, min(r_fixed, length(sv$d), Nn, floor(0.5 * min(Tn, Nn))))
#   
#   U_r <- sv$u[, 1:r, drop = FALSE]
#   D_r <- sv$d[1:r]
#   V_r <- sv$v[, 1:r, drop = FALSE]
#   
#   F0       <- sqrt(Tn) * U_r                            # T x r
#   Lambda0  <- V_r %*% diag(D_r / sqrt(Tn), nrow = r)    # N x r
#   
#   # --- 3) Idiosyncratic variances (R0, diagonal) from residuals X - F0 %*% t(Lambda0) ---
#   E0 <- X_svd - F0 %*% t(Lambda0)
#   R_diag <- colMeans(E0^2)  # N x 1
#   R_diag[!is.finite(R_diag)] <- 1e-4
#   R_diag <- pmax(R_diag, 1e-4)
#   R0 <- diag(R_diag, nrow = Nn)
#   
#   # --- 4) Factor dynamics: independent AR(1) for each factor ---
#   phi <- rep(NA_real_, r)
#   sig_u <- rep(NA_real_, r)
#   for (j in seq_len(r)) {
#     y  <- F0[-1, j]
#     x  <- F0[-Tn, j]
#     # OLS without intercept: f_t = phi * f_{t-1} + u_t
#     phi_j <- sum(x * y) / sum(x * x)
#     if (!is.finite(phi_j)) phi_j <- 0
#     phi_j <- max(-ar_cap, min(ar_cap, phi_j))  # stability cap
#     u_j   <- y - phi_j * x
#     s2_j  <- mean(u_j^2)
#     if (!is.finite(s2_j) || s2_j <= 0) s2_j <- 1e-4
#     phi[j]  <- phi_j
#     sig_u[j] <- s2_j
#   }
#   A0 <- diag(phi, nrow = r)                 # r x r transition matrix
#   Q0 <- diag(pmax(sig_u, 1e-4), nrow = r)   # r x r state noise covariance
#   
#   # --- 5) Build the state-space model for KFAS ---
#   # Measurement eq: x_t = Λ f_t + ε_t,  ε_t ~ N(0, R0)
#   # Transition eq:  f_t = A f_{t-1} + u_t,  u_t ~ N(0, Q0)
#   Zt <- array(0, dim = c(Nn, r, 1)); Zt[, , 1] <- Lambda0
#   Tt <- array(A0, dim = c(r, r, 1))
#   Rt <- diag(r)
#   
#   model <- SSModel(
#     X_std ~ -1 + SSMcustom(
#       Z = Zt, T = Tt, R = Rt,
#       Q = Q0, a1 = rep(0, r), P1 = diag(10, r),
#       state_names = paste0("F", 1:r)
#     ),
#     H = R0
#   )
#   
#   list(
#     X_std   = X_std,          # standardized data used in measurement equation
#     r       = r,
#     F0      = F0,             # initial factors (T x r)
#     Lambda0 = Lambda0,        # initial loadings (N x r)
#     R0      = R0,             # idiosyncratic variance matrix
#     A0      = A0,             # AR(1) transition matrix
#     Q0      = Q0,             # factor innovation variance
#     model   = model           # SSModel ready for EM / Kalman
#   )
# }
# 
# # ====== USAGE EXAMPLE ======
# # Assuming df_train_std, vars_X, r_fixed already exist:
# init <- dfm_init(df_train_std, vars_X, r_fixed = r_fixed)
# 
# # Optionally: first smoothing pass with initial parameters
# ks0 <- KFS(init$model, smoothing = c("state", "mean"))
# F_smooth0 <- ks0$alphahat            # T x r smoothed factors
# X_fitted0 <- signal(init$model)$signal  # fitted measurements
# 
# # Optional: run EM to optimize H and Q (and optionally Z, T if parameterized)
# # fit <- fitSSM(init$model,
# #               inits = c(log(diag(init$model$H)), log(diag(init$model$Q))),
# #               method = "BFGS")
# # mod_hat <- fit$model
# # ks_hat  <- KFS(mod_hat, smoothing = c("state", "mean"))
# 
# # ============================================================
# # ML estimation (quasi-ML) of H (idiosyncratic), Q (factor shocks), A (AR(1))
# # Λ (loadings) kept fixed to PCA for identification.
# # ============================================================
# 
# library(KFAS)
# 
# ssm0   <- init$model
# Nn     <- nrow(ssm0$y)             # number of series (N)
# r      <- dim(ssm0$Z)[2]           # number of factors (r)
# phi_cap <- 0.98                     # stability cap for AR(1)
# 
# # --- Parameterization & constraints ---
# # par = [ log(diag(H))_(N) , log(diag(Q))_(r) , atanh(phi/phi_cap)_(r) ]
# par0 <- c(
#   log(diag(ssm0$H)),                        # H diagonal (positive)
#   log(diag(ssm0$Q)),                        # Q diagonal (positive)
#   atanh(diag(ssm0$T[,,1]) / phi_cap)        # AR(1) φ_j ∈ (-phi_cap, phi_cap)
# )
# 
# # Map parameter vector -> model (H, Q, A updated; Λ fixed)
# updatefn <- function(par, model) {
#   idxH <- 1:Nn
#   idxQ <- (Nn + 1):(Nn + r)
#   idxA <- (Nn + r + 1):(Nn + 2*r)
#   
#   Hdiag <- exp(par[idxH])                    # ensure > 0
#   Qdiag <- exp(par[idxQ])                    # ensure > 0
#   phi   <- phi_cap * tanh(par[idxA])         # ensure |phi| < phi_cap
#   
#   model$H[,]     <- diag(Hdiag, nrow = Nn)
#   model$Q[,]     <- diag(Qdiag, nrow = r)
#   model$T[,,1]   <- diag(phi, nrow = r)      # AR(1) diagonal transition
#   # Z (loadings) and R (identity) are kept as-in (from PCA init)
#   model
# }
# 
# # --- Fit by BFGS (quasi-ML) ---
# fit <- fitSSM(
#   inits    = par0,
#   model    = ssm0,
#   updatefn = updatefn,
#   method   = "BFGS",
#   control  = list(maxit = 300, reltol = 1e-8)
# )
# 
# mod_hat <- fit$model
# 
# # --- Smoothing with estimated parameters ---
# ks_hat <- KFS(mod_hat, smoothing = c("state", "mean"))
# F_smooth <- ks_hat$alphahat               # T x r smoothed factors
# X_fitted <- signal(mod_hat)$signal        # T x N fitted measurements (in standardized units)
# 
# # --- Basic diagnostics ---
# ll    <- as.numeric(logLik(mod_hat))
# k_par <- length(par0)
# n_obs <- sum(is.finite(mod_hat$y))        # total observed measurements
# AIC   <- -2*ll + 2*k_par
# BIC   <- -2*ll + k_par*log(n_obs)
# 
# cat(sprintf("\n=== Quasi-ML fit ===\nlogLik = %.2f | AIC = %.2f | BIC = %.2f | k = %d | n = %d\n",
#             ll, AIC, BIC, k_par, n_obs))
# 
# # Standardized recursive residuals (one-step-ahead)
# res_std <- residuals(mod_hat, type = "recursive", standardized = TRUE)
# # You can inspect series-wise residual distribution / autocorrelation as needed.
# 
# # --- Extract estimated components (useful for reporting) ---
# H_hat   <- mod_hat$H[,]                   # diagonal idiosyncratic variances (N x N)
# Q_hat   <- mod_hat$Q[,]                   # diagonal factor shock variances (r x r)
# A_hat   <- mod_hat$T[,,1]                 # AR(1) coefficients (r x r, diagonal)
# Lambda  <- mod_hat$Z[,,1]                 # loadings (N x r) — fixed from PCA init
# 
# # (Optional) Reconstruct idiosyncratic residuals in standardized units:
# # E_hat = y - Λ * f |t (use smoothed states for f)
# Y_std_hat <- t(Lambda %*% t(F_smooth))    # T x N
# E_hat <- mod_hat$y - t(Y_std_hat)         # N x T in KFAS, so transpose back if needed
# # NOTE: KFAS stores y as N x T; signal() returns T x N; be mindful of dims if you use these.
# 
# # ============================================================
# # Forecasting h steps ahead (in transformed/standardized units)
# # ============================================================
# 
# h_steps <- 4L
# 
# # Extend the model with h rows of NA to obtain prediction for the measurements
# mod_fc <- mod_hat
# Y_obs  <- mod_fc$y                         # N x T
# Y_aug  <- cbind(Y_obs, matrix(NA_real_, nrow = Nn, ncol = h_steps))
# mod_fc$y <- Y_aug
# 
# # Filter forward to get one-step-ahead predictions
# kf_fc <- KFS(mod_fc, filtering = "state", smoothing = "none")
# 
# # Predicted signals (measurement means) for the whole augmented sample:
# sig_aug <- signal(mod_fc)$signal           # (T + h) x N  in standardized units
# 
# # Grab only the last h rows (forecasts)
# pred_transformed <- sig_aug[(nrow(sig_aug) - h_steps + 1):nrow(sig_aug), , drop = FALSE]
# colnames(pred_transformed) <- colnames(init$X_std)  # same measurement names (vars_X order)
# 
# # ============================================================
# # Map forecasts to your target variables and back-transform to levels (optional)
# # ============================================================
# # pred_transformed is in the *measurement space* (standardized transformed units).
# # If your targets vars_Y are among the measurement variables, subset and de-standardize using
# # the moments you use in your pipeline (mean/sd from the training window you prefer).
# # Otherwise, build a Y from the factors via a separate mapping (like your OLS step).
# 
# # Example: if vars_Y ⊆ colnames(pred_transformed) and you want to de-standardize with
# # full-history moments (Y_mean, Y_sd) as you already do for nowcasts:
# 
# pred_Y_tr <- pred_transformed[, vars_Y, drop = FALSE]  # transformed (Δlog*100 or diff), std units
# # De-standardize back to transformed units:
# # (Assumes you have Y_mean and Y_sd consistent with how you standardized Y in your pipeline.)
# pred_Y_transformed <- sweep(pred_Y_tr, 2, Y_sd, `*`)
# pred_Y_transformed <- sweep(pred_Y_transformed, 2, Y_mean, `+`)
# 
# # If you want back to *levels*, reuse your back-transform logic per variable:
# #   - logdiff:  Level_{t+h} = Base_{t+h-1} * exp( y_{t+h} / 100 )
# #   - diff:     Level_{t+h} = Base_{t+h-1} + y_{t+h}
# # where Base depends on the horizon (for h=1 use L_t, for h=4 use L_{t+3}).
# # You already implemented this in your PCA+VAR part; plug the same helper here.
# 
# # (Optional) Assemble a tidy forecast table
# fc_dates <- seq(from = max(df_train_std$date), by = "quarter", length.out = h_steps)
# now_table_transformed_ssm <- data.frame(
#   date = fc_dates,
#   pred_Y_transformed,
#   row.names = NULL,
#   check.names = FALSE
# )
# 
# print(head(now_table_transformed_ssm, 4))

# ============================================================================
# Enhanced State-Space Dynamic Factor Model Implementation
# Following analyst's detailed specification for KFAS implementation
# ============================================================================

cat("\n=== Enhanced State-Space Dynamic Factor Model (KFAS) ===\n")

# ============================================================================
# Data Preparation for State-Space Model
# Use all series (targets + predictors) for factor extraction
# ============================================================================

# Create the full measurement matrix Y_mat (T x m) including both targets and predictors
Y_cols <- c(vars_Y, vars_X)  # targets first, then predictors
Y_mat <- as.matrix(df_train_std[, Y_cols, drop = FALSE])
m <- ncol(Y_mat)  # number of series
r <- r_fixed      # number of factors from Bai-Ng selection

cat(sprintf("Building state-space model with m=%d series and r=%d factors\n", m, r))

# ============================================================================
# State-Space Model Construction with KFAS
# Observation equation: y_t = Λ f_t + ε_t
# State equation: f_t = Φ f_{t-1} + u_t
# ============================================================================

# Initialize system matrices with appropriate dimensions
Zt <- matrix(NA, m, r)          # loading matrix (m x r) with NAs for estimation
Tt <- matrix(NA, nrow = r, ncol = r) # AR(2) factor transition (r x r) with NAs on diagonal
Rt <- diag(1, r)                # identity matrix (r x r)
Qt <- diag(1, r)                # identity matrix (r x r) - fixed for identification
Ht <- diag(NA, m)               # diagonal observation covariance (m x m) with NAs

# Initial state setup for diffuse initialization
a1 <- rep(0, r)                 # initial state means
P1 <- matrix(0, r, r)           # diffuse part (will use P1inf for actual diffuse)
P1inf <- diag(1, r)             # diffuse initial covariance flags

# Build the SSModel object
model_ss <- SSModel(
  Y_mat ~ -1 + SSMcustom(
    Z = Zt, 
    T = Tt, 
    R = Rt, 
    Q = Qt,
    a1 = a1, 
    P1 = P1, 
    P1inf = P1inf,
    state_names = paste0("Factor", 1:r),
    index = 1:m
  ),
  H = Ht
)

cat("State-space model structure created\n")

# ============================================================================
# Parameter Estimation Setup
# Parameters to estimate: loadings (Λ), observation variances (H), AR coefficients (φ)
# ============================================================================

# Define parameter vector lengths
nZ   <- m * r            # number of loadings
nH   <- m                # number of unique H variances (diagonal)
nPhi <- r                # number of AR coefficients

# Update function to map parameter vector to model matrices
updatefn_ss <- function(par, model) {
  stopifnot(length(par) == nZ + nH + nPhi)
  
  # 1. Factor loadings (Z matrix) - fill by row-major order
  model$Z[,,1] <- matrix(par[1:nZ], nrow = m, ncol = r)
  
  # 2. Observation variances (H matrix diagonal) - exponential transformation
  h_vals <- exp(par[(nZ+1):(nZ+nH)])
  model$H[,,1] <- diag(h_vals, m)
  
  # 3. AR(1) coefficients (T matrix diagonal) - tanh transformation for stationarity
  phi_pars <- par[(nZ+nH+1):(nZ+nH+nPhi)]
  phi_vals <- tanh(phi_pars)  # maps to (-1, 1)
  model$T[,,1] <- diag(phi_vals, r)
  
  return(model)
}

# ============================================================================
# Initialize parameters using PCA results
# ============================================================================

cat("Initializing parameters using PCA...\n")

# Use PCA for initial loading estimates
Y_pca <- Y_mat
Y_pca[!is.finite(Y_pca)] <- 0  # replace NAs with 0 for PCA

pca_result <- prcomp(Y_pca, center = FALSE, scale. = FALSE)  # already standardized
init_loadings_pca <- pca_result$rotation[, 1:r, drop = FALSE]  # m x r

# Scale loadings to match our Q=I convention
eigenvals <- pca_result$sdev[1:r]^2
init_loadings_scaled <- init_loadings_pca %*% diag(sqrt(eigenvals), r)

# Initial parameter vector
init_loadings <- as.vector(init_loadings_scaled)  # vectorize by row-major order
init_H <- rep(log(0.5), nH)                      # log-variance initial guess
init_phi <- rep(0, nPhi)                         # start with no persistence

init_params <- c(init_loadings, init_H, init_phi)

cat(sprintf("Parameter vector length: %d (loadings: %d, H: %d, phi: %d)\n", 
            length(init_params), nZ, nH, nPhi))

# ============================================================================
# Maximum Likelihood Estimation
# ============================================================================

cat("Starting maximum likelihood estimation...\n")

# Fit the model using BFGS optimization
fit_ss <- fitSSM(
  model = model_ss, 
  inits = init_params, 
  updatefn = updatefn_ss, 
  method = "BFGS",
  control = list(maxit = 500, reltol = 1e-6, trace = 1)
)

# Check convergence
if (fit_ss$optim.out$convergence != 0) {
  cat("Warning: Optimization did not converge properly. Code:", fit_ss$optim.out$convergence, "\n")
} else {
  cat("Optimization converged successfully\n")
}

model_fit_ss <- fit_ss$model
logLik_val_ss <- logLik(model_fit_ss)

cat(sprintf("Final log-likelihood: %.2f\n", as.numeric(logLik_val_ss)))

# Extract estimated parameters
Lambda_hat <- model_fit_ss$Z[,,1]     # estimated loadings (m x r)
H_hat_ss <- model_fit_ss$H[,,1]       # estimated observation variances (m x m)
phi_hat <- diag(model_fit_ss$T[,,1])  # estimated AR coefficients (r x 1)

cat("Estimated AR(1) coefficients for factors:\n")
print(round(phi_hat, 3))

# ============================================================================
# Kalman Filtering and Smoothing
# ============================================================================

cat("Running Kalman filter and smoother...\n")

# Run Kalman filter and smoother
kfs_result <- KFS(model_fit_ss, smoothing = c("state", "mean"))

# Extract smoothed factors and fitted values
factors_smooth <- kfs_result$alphahat     # smoothed factors (T x r)
Y_fitted_ss <- fitted(model_fit_ss)       # fitted observations (T x m)

cat(sprintf("Extracted %d factors over %d time periods\n", ncol(factors_smooth), nrow(factors_smooth)))

# ============================================================================
# Forecasting with State-Space Model
# Generate nowcast (h=1) and 4-quarters ahead (h=4) forecasts
# ============================================================================

cat("Generating forecasts...\n")

# For KFAS, we need to extend the data matrix with NAs and use KFS for forecasting
h_forecast <- 4
T_obs <- nrow(Y_mat)

# Create extended data matrix with NAs for forecast periods
Y_extended <- rbind(Y_mat, matrix(NA, nrow = h_forecast, ncol = m))

# Create extended model
model_extended <- SSModel(
  Y_extended ~ -1 + SSMcustom(
    Z = model_fit_ss$Z, 
    T = model_fit_ss$T, 
    R = model_fit_ss$R, 
    Q = model_fit_ss$Q,
    a1 = model_fit_ss$a1, 
    P1 = model_fit_ss$P1, 
    P1inf = model_fit_ss$P1inf,
    state_names = paste0("Factor", 1:r),
    index = 1:m
  ),
  H = model_fit_ss$H
)

# Run Kalman filter on extended model to get forecasts
kfs_forecast <- KFS(model_extended, filtering = "state", smoothing = "none")

# Extract forecasts from the filtered estimates
# The forecasts are the fitted values for the NA periods
Y_forecast <- fitted(model_extended)[(T_obs + 1):(T_obs + h_forecast), , drop = FALSE]

# Extract forecasts for target variables
target_idx_ss <- match(vars_Y, colnames(Y_mat))

# Extract specific horizons
fc_h1_ss <- Y_forecast[1, target_idx_ss]  # nowcast (h=1)
fc_h4_ss <- Y_forecast[4, target_idx_ss]  # 4-quarters ahead (h=4)

# Create forecast results table (transformed units)
nowcast_results_ss <- data.frame(
  variable = vars_Y,
  last_obs_date = max(df_train_clean$date),
  nowcast_h1_date = seq(max(df_train_clean$date), by = "quarter", length.out = 2)[2],
  forecast_h1_tr = as.numeric(fc_h1_ss),
  forecast_h4_date = seq(max(df_train_clean$date), by = "quarter", length.out = 5)[5],
  forecast_h4_tr = as.numeric(fc_h4_ss)
)

cat("\n=== State-Space Model Forecasts (Transformed Units) ===\n")
print(nowcast_results_ss)

# ============================================================================
# Back-transformation to Levels (Optional)
# Convert forecasts from transformed units back to original levels
# ============================================================================

cat("\nBack-transforming forecasts to levels...\n")

# Get last observed values in original levels
last_levels_ss <- df_train[df_train$date == max(df_train_clean$date), vars_Y]
L_t_ss <- as.numeric(last_levels_ss)
names(L_t_ss) <- vars_Y

# Get transformation types from metadata
transform_type_ss <- setNames(metadata$transform[match(vars_Y, metadata$ts_key)], vars_Y)

# Back-transform forecasts
L_h1_ss <- numeric(length(vars_Y))
L_h4_ss <- numeric(length(vars_Y))

for (j in seq_along(vars_Y)) {
  v <- vars_Y[j]
  tr <- tolower(transform_type_ss[j])
  fh1 <- fc_h1_ss[j]  # h=1 forecast in transformed scale
  fh4 <- fc_h4_ss[j]  # h=4 forecast in transformed scale
  
  if (tr == "logdiff") {
    # For log-difference transformation: Level = last_level * exp(forecast/100)
    L_h1_ss[j] <- L_t_ss[v] * exp(fh1 / 100)
    L_h4_ss[j] <- L_t_ss[v] * exp(fh4 / 100)
  } else if (tr == "diff") {
    # For simple difference: Level = last_level + forecast
    L_h1_ss[j] <- L_t_ss[v] + fh1
    L_h4_ss[j] <- L_t_ss[v] + fh4
  } else {
    # No transformation case
    L_h1_ss[j] <- fh1
    L_h4_ss[j] <- fh4
  }
}

# Create back-transformed results table
backtransformed_results_ss <- data.frame(
  variable = vars_Y,
  nowcast_h1_date = nowcast_results_ss$nowcast_h1_date,
  forecast_h1_level = L_h1_ss,
  forecast_h4_date = nowcast_results_ss$forecast_h4_date,
  forecast_h4_level = L_h4_ss
)

cat("\n=== State-Space Model Forecasts (Level Units) ===\n")
print(backtransformed_results_ss)

# ============================================================================
# Model Diagnostics and Summary
# ============================================================================

cat("\n=== State-Space Model Diagnostics ===\n")

# Calculate information criteria
k_params_ss <- nZ + nH + nPhi
n_obs_ss <- sum(is.finite(model_fit_ss$y))
AIC_ss <- -2 * as.numeric(logLik_val_ss) + 2 * k_params_ss
BIC_ss <- -2 * as.numeric(logLik_val_ss) + k_params_ss * log(n_obs_ss)

cat(sprintf("Log-likelihood: %.2f\n", as.numeric(logLik_val_ss)))
cat(sprintf("AIC: %.2f\n", AIC_ss))
cat(sprintf("BIC: %.2f\n", BIC_ss))
cat(sprintf("Number of parameters: %d\n", k_params_ss))
cat(sprintf("Number of observations: %d\n", n_obs_ss))

# Residual diagnostics
# residuals_ss <- residuals(model_fit_ss, type = "recursive", standardized = TRUE)
# residual_mean <- apply(residuals_ss, 2, function(x) mean(x, na.rm = TRUE))
# residual_sd <- apply(residuals_ss, 2, function(x) sd(x, na.rm = TRUE))
# 
# cat(sprintf("\nResidual diagnostics - Mean absolute residual: %.4f\n", 
#             mean(abs(residual_mean), na.rm = TRUE)))
# cat(sprintf("Residual diagnostics - Average std deviation: %.4f\n", 
#             mean(residual_sd, na.rm = TRUE)))
# 
# # Factor loadings summary for target variables
# cat("\n=== Factor Loadings for Target Variables ===\n")
# loadings_targets <- Lambda_hat[target_idx_ss, , drop = FALSE]
# rownames(loadings_targets) <- vars_Y
# colnames(loadings_targets) <- paste0("Factor", 1:r)
# print(round(loadings_targets, 3))

# ============================================================================
# Comparison Summary
# Create summary for comparison with other methods
# ============================================================================

# Store state-space results for potential plotting/comparison
ssm_forecast_summary <- data.frame(
  variable = rep(vars_Y, 2),
  date = c(rep(nowcast_results_ss$nowcast_h1_date[1], length(vars_Y)),
           rep(nowcast_results_ss$forecast_h4_date[1], length(vars_Y))),
  value = c(L_h1_ss, L_h4_ss),
  horizon = rep(c("h1", "h4"), each = length(vars_Y)),
  source = "StateSpaceDFM"
)

cat("\n=== State-Space Dynamic Factor Model Implementation Complete ===\n")
cat("Key outputs:\n")
cat("- nowcast_results_ss: Forecasts in transformed units\n")
cat("- backtransformed_results_ss: Forecasts in level units\n")
cat("- factors_smooth: Extracted common factors\n")
cat("- Lambda_hat: Estimated factor loadings\n")
cat("- ssm_forecast_summary: Results formatted for comparison\n")

# ============================================================================
# State-Space Model vs KOF Forecast Comparison
# ============================================================================

cat("\n=== Comparing State-Space Model with KOF Forecasts ===\n")

# Get forecast dates from the state space model
h1_date_ss <- backtransformed_results_ss$nowcast_h1_date[1]
h4_date_ss <- backtransformed_results_ss$forecast_h4_date[1]

# Create state space forecast points for comparison
ss_pts <- bind_rows(
  tibble(variable = vars_Y, date = h1_date_ss,
         value = backtransformed_results_ss$forecast_h1_level,
         horizon = "h1", source = "StateSpace"),
  tibble(variable = vars_Y, date = h4_date_ss,
         value = backtransformed_results_ss$forecast_h4_level,
         horizon = "h4", source = "StateSpace")
)

# Get corresponding KOF forecasts for same dates
kof_pts_ss <- bind_rows(
  tibble(variable = vars_Y, date = h1_date_ss,
         value = sapply(vars_Y, function(v) get_kof_value(h1_date_ss, v)),
         horizon = "h1", source = "KOF"),
  tibble(variable = vars_Y, date = h4_date_ss,
         value = sapply(vars_Y, function(v) get_kof_value(h4_date_ss, v)),
         horizon = "h4", source = "KOF")
) %>% dplyr::filter(is.finite(value))

# Calculate differences (State Space - KOF)
differences_ss <- merge(
  ss_pts %>% dplyr::select(variable, date, horizon, value) %>% dplyr::rename(ss_value = value),
  kof_pts_ss %>% dplyr::select(variable, date, horizon, value) %>% dplyr::rename(kof_value = value),
  by = c("variable", "date", "horizon"),
  all.x = TRUE
) %>%
  dplyr::mutate(
    difference = ss_value - kof_value,
    abs_difference = abs(difference),
    relative_diff_pct = 100 * difference / abs(kof_value)
  )

cat("\n=== State-Space vs KOF Forecast Differences ===\n")
print(differences_ss)

# Summary statistics of differences
cat("\n=== Summary of Forecast Differences ===\n")
summary_stats_ss <- differences_ss %>%
  dplyr::group_by(horizon) %>%
  dplyr::summarise(
    mean_abs_diff = mean(abs_difference, na.rm = TRUE),
    median_abs_diff = median(abs_difference, na.rm = TRUE),
    max_abs_diff = max(abs_difference, na.rm = TRUE),
    mean_relative_diff = mean(abs(relative_diff_pct), na.rm = TRUE),
    .groups = 'drop'
  )
print(summary_stats_ss)

# Detailed comparison by variable
cat("\n=== Detailed Comparison by Variable ===\n")
detailed_comparison_ss <- differences_ss %>%
  dplyr::select(variable, horizon, ss_value, kof_value, difference, relative_diff_pct) %>%
  dplyr::arrange(variable, horizon)
print(detailed_comparison_ss)

# ============================================================================
# Visualization: State-Space Model vs KOF Forecasts
# ============================================================================

# Color palette for state space comparison
pal_colors_ss <- c("StateSpace" = "#2ca02c", "KOF" = "#ff7f0e")
pal_shapes_ss <- c("StateSpace" = 18, "KOF" = 17)

# Ensure dates are properly formatted
ss_pts$date <- as.Date(ss_pts$date)
kof_pts_ss$date <- as.Date(kof_pts_ss$date)

# Create comparison plot
p_ss_comparison <- ggplot() +
  geom_line(data = levels_recent,
            aes(x = date, y = value),
            linewidth = 0.6, color = "grey40") +
  geom_vline(xintercept = as.numeric(last_date),
             linetype = "solid", color = "grey50") +
  geom_point(data = bind_rows(ss_pts, kof_pts_ss),
             aes(x = date, y = value, color = source, shape = source),
             size = 3.2) +
  facet_wrap(
    ~ variable,
    scales = "free_y",
    ncol = 1,
    labeller = as_labeller(facet_labels)
  ) +
  scale_color_manual(
    values = pal_colors_ss,
    breaks = c("StateSpace", "KOF"),
    labels = c("State-Space DFM", "KOF forecast"),
    name = ""
  ) +
  scale_shape_manual(
    values = pal_shapes_ss,
    breaks = c("StateSpace", "KOF"),
    labels = c("State-Space DFM", "KOF forecast"),
    name = ""
  ) +
  labs(title = "Forecast Comparison: State-Space Dynamic Factor Model vs KOF",
       subtitle = paste0("Historical until last observed value: ",
                         format(last_date, "%Y-%m-%d")),
       x = "Date", y = "Levels") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )

print(p_ss_comparison)

# ============================================================================
# Comparison of Both Models (PCA+VAR vs State-Space) vs KOF
# ============================================================================

cat("\n=== Three-Way Comparison: PCA+VAR vs State-Space vs KOF ===\n")

# Combine all forecasts for comparison
all_forecasts <- bind_rows(
  our_pts %>% mutate(source = "PCA_VAR"),  # Rename for clarity
  ss_pts,
  kof_pts_ss
)

# Calculate all pairwise differences
pca_vs_kof <- merge(
  our_pts %>% dplyr::select(variable, horizon, value) %>% dplyr::rename(pca_value = value),
  kof_pts_ss %>% dplyr::select(variable, horizon, value) %>% dplyr::rename(kof_value = value),
  by = c("variable", "horizon"), all.x = TRUE
) %>% dplyr::mutate(comparison = "PCA_VAR_vs_KOF", difference = pca_value - kof_value)

ss_vs_kof <- differences_ss %>% 
  dplyr::select(variable, horizon, difference) %>% 
  dplyr::mutate(comparison = "StateSpace_vs_KOF")

pca_vs_ss <- merge(
  our_pts %>% dplyr::select(variable, horizon, value) %>% dplyr::rename(pca_value = value),
  ss_pts %>% dplyr::select(variable, horizon, value) %>% dplyr::rename(ss_value = value),
  by = c("variable", "horizon"), all.x = TRUE
) %>% dplyr::mutate(comparison = "PCA_VAR_vs_StateSpace", difference = pca_value - ss_value)

all_differences <- dplyr::bind_rows(
  pca_vs_kof %>% dplyr::select(variable, horizon, comparison, difference),
  ss_vs_kof %>% dplyr::select(variable, horizon, comparison, difference),
  pca_vs_ss %>% dplyr::select(variable, horizon, comparison, difference)
)

cat("\n=== All Pairwise Differences ===\n")
print(all_differences)

# Summary of model performance vs KOF
performance_summary <- all_differences %>%
  dplyr::filter(grepl("_vs_KOF", comparison)) %>%
  dplyr::group_by(comparison, horizon) %>%
  dplyr::summarise(
    mean_abs_diff = mean(abs(difference), na.rm = TRUE),
    median_abs_diff = median(abs(difference), na.rm = TRUE),
    max_abs_diff = max(abs(difference), na.rm = TRUE),
    .groups = 'drop'
  )

cat("\n=== Model Performance Summary (vs KOF) ===\n")
print(performance_summary)
