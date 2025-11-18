# ================================
# 0. Initialize renv
# ================================
source("renv/activate.R")

# ================================
# 1. Setup, Load Packages and Data
# ================================

library(ggplot2)
library(dplyr)
library(vars)
library(tidyr)
library(urca)
library(rlang)
library(KFAS)
library(lubridate)
library(readxl)
kfilepath_data <- "C:/Users/kfree/OneDrive/Desktop/MASTER 3/MACRO FORECAST/DATASET/data_quarterly.csv"
kfilepath_metadata <- "C:/Users/kfree/OneDrive/Desktop/MASTER 3/MACRO FORECAST/DATASET/metadata_quarterly_en.csv"
nfilepath_data <- "~/Documents/school/methods-for-macro-forecasting-2025/submission/data/data_quarterly.csv"
nfilepath_metadata <- "~/Documents/school/methods-for-macro-forecasting-2025/submission/data/metadata_quarterly_en.csv"
kkof_path <- "C:/Users/kfree/OneDrive/Desktop/MASTER 3/MACRO FORECAST/DATASET/KOF_BAROMETER.xlsx"
nkof_path <- "~/Documents/school/methods-for-macro-forecasting-2025/submission/data/KOF_BAROMETER.xlsx"

# Adjust depending on environment
df <- utils::read.csv(nfilepath_data)
metadata <- utils::read.csv(nfilepath_metadata)
kof_raw <- read_excel(nkof_path, sheet = 1)
cutoff_pre_covid <- as.Date("2019-12-31")
# ================================
# 2. Data Preparation
# ================================
names(metadata) <- tolower(names(metadata))
if (!"ts_key" %in% names(metadata)) stop("Missing ts_key in metadata")

# Filter to data we know (not projections)
df_train_pre_covid <- df %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%        
  filter(date <= cutoff_pre_covid) %>%
  mutate(across(-date, as.numeric))

df_post <- df %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  filter(date > cutoff_pre_covid) %>%
  mutate(across(-date, as.numeric))

# Check to confirm no missing values
sapply(df_train_pre_covid, function(x) sum(is.na(x))) %>% sort(decreasing = TRUE)

# normalize caps
metadata$unit_low <- tolower(metadata$unit)

# We need to transform the raw values to make the series stationary
unit_to_transform <- c(
  # Real and nominal quantities (logs capture proportional growth)
  "real in millions of francs" = "logdiff",
  "in millions of francs at current prices" = "logdiff",
  "real in millions of us dollars" = "logdiff",
  "in us dollars" = "logdiff",
  "real in thousands of swiss francs" = "logdiff",
  "in thousands of swiss francs at current prices" = "logdiff",
  
  # Indexes and deflators (inflation-type measures)
  "index" = "logdiff",
  "price deflator" = "logdiff",
  
  # Productivity measures (real per-hour or per-FTE)
  "gross domestic product (adjusted for sporting events) per hour worked" = "logdiff",
  "gross domestic product (adjusted for sporting events) per full-time equivalent" = "logdiff",
  "real value of gross domestic product per full-time equivalent" = "logdiff",
  
  # Labor quantities (use diff if absolute, logdiff if proportional)
  "in thousands of persons" = "diff",
  
  # Rates and percentages (stationary in differences or demeaned)
  "in percent" = "diff",
  "rate" = "diff"
)
metadata$transform <- unit_to_transform[metadata$unit_low]

df_train_clean <- df_train_pre_covid

transform_variable <- function(x, transform_type) {
  if (transform_type == "logdiff") {
    y <- rep(NA_real_, length(x))
    pos <- x > 0
    y[pos] <- log(x[pos])             # niente warning
    return(c(NA_real_, diff(y) * 100))
  } else if (transform_type == "diff") {
    return(c(NA_real_, diff(x)))
  } else {
    return(x)
  }
}

# Apply transformations using ts_key 
for (i in seq_len(nrow(metadata))) {
  key <- metadata$ts_key[i]
  t_type <- metadata$transform[i]
  if (!is.na(t_type) && key %in% names(df_train_clean)) {
    df_train_clean[[key]] <- transform_variable(df_train_clean[[key]], t_type)
  }
}

# Remove first raw (diff) e drop problematic columns (all NA or 0 variance)
df_train_clean <- df_train_clean[-1, ]
is_all_na  <- vapply(df_train_clean, function(x) all(is.na(x)), logical(1))
df_train_clean <- df_train_clean[, !is_all_na, drop = FALSE]
keep_idx <- c(TRUE, sapply(df_train_clean[-1], function(x) sd(x, na.rm=TRUE) > 1e-8))
df_train_clean <- df_train_clean[, keep_idx, drop = FALSE]

# Standardization
df_train_std <- df_train_clean
df_train_std[-1] <- scale(df_train_clean[-1])

# Define targets (ts_key) than X
vars_Y <- c("rvgdp", "cpi", "wkfreuro")
growth_vars <- c("rvgdp")  # growth_vars are reported in percent growth (Δlog * 100), even though the underlying transform is logdiff. That’s why we skip back-transformation to levels for these.
level_vars  <- setdiff(vars_Y, growth_vars)
missing_y <- setdiff(vars_Y, names(df_train_std))
if (length(missing_y)) stop(paste("Missing targets:", paste(missing_y, collapse=", ")))
vars_X <- setdiff(names(df_train_std)[-1], vars_Y)

# Test Stationarity after transformation
adf_results <- sapply(df_train_clean[-1], function(x) {
  z <- na.omit(x)
  if (length(z) < 20) return(NA_real_)
  test <- tryCatch(ur.df(z, type = "drift", selectlags = "AIC"), error = function(e) NULL)
  if (is.null(test)) return(NA_real_)
  pval <- tryCatch(test@testreg$coefficients[2,4], error = function(e) NA_real_)
  return(pval)
})
adf_results <- sort(adf_results)

# Saving means and sd 
scaler_params <- data.frame(
  variable = names(df_train_clean)[-1],
  mean = apply(df_train_clean[-1], 2, mean, na.rm = TRUE),
  sd = apply(df_train_clean[-1], 2, sd, na.rm = TRUE)
)

# Sort by date (safety)
df_train_std <- df_train_std[order(df_train_std$date), , drop = FALSE]

# Dimensions and 80/20 split
T_total <- nrow(df_train_std)
t0_idx <- floor(0.8 * T_total)      # end index of initial training 
h_long <- 4                         # 1-year ahead (quarterly)
last_origin_idx <- T_total - h_long # last valid origin for h=4
# If 80% falls too close to the end, adjust to guarantee room for h=4
if (t0_idx > last_origin_idx) {
  t0_idx <- max(
    floor(0.75 * T_total),
    min(T_total - (h_long + 8),
        T_total - h_long - 1)
  )
}

# Drop variables with <30% of NA from training set for Bai-Ng criterion
na_frac <- colMeans(is.na(as.matrix(df_train_std[1:t0_idx, vars_X, drop = FALSE])))
vars_X <- setdiff(vars_X, names(na_frac[na_frac > 0.30]))

# Final guardrails
t0_idx <- max(1, t0_idx)
stopifnot(last_origin_idx >= t0_idx)

# Expanding-window origins: t = t0, t0+1, ..., T-4
oos_origin_idx <- seq(t0_idx, last_origin_idx)
stopifnot(length(oos_origin_idx) > 0)

# Useful dates (origins and targets)
origin_dates    <- df_train_std$date[oos_origin_idx]
target_h1_dates <- df_train_std$date[oos_origin_idx + 1]
target_h4_dates <- df_train_std$date[oos_origin_idx + h_long]

# Quick report
cat("\n=== OOS setup (expanding) ===\n")
cat("Usable observations (post-NA):  ", T_total, "\n")
cat("Initial training up to idx:     ", t0_idx, " => ", as.character(df_train_std$date[t0_idx]), "\n")
cat("Last OOS origin (idx):          ", last_origin_idx, " => ", as.character(df_train_std$date[last_origin_idx]), "\n")
cat("Number of OOS origins:          ", length(oos_origin_idx), "\n")
cat("First origin -> targets h1/h4:  ", as.character(origin_dates[1]), " -> ",
    as.character(target_h1_dates[1]), " / ", as.character(target_h4_dates[1]), "\n")
cat("Last origin -> targets h1/h4:   ", as.character(origin_dates[length(origin_dates)]), " -> ",
    as.character(target_h1_dates[length(target_h1_dates)]), " / ", as.character(target_h4_dates[length(target_h4_dates)]), "\n")
# --- De-duplicate highly collinear series (cluster on |corr|)
X_train_for_sel <- as.matrix(df_train_std[1:t0_idx, vars_X, drop = FALSE])

# Guard: need at least 2 cols and some non-NA variance
ok <- apply(X_train_for_sel, 2, function(x) sd(x, na.rm = TRUE) > 0)
X_train_for_sel <- X_train_for_sel[, ok, drop = FALSE]
vars_X <- colnames(X_train_for_sel)

C  <- cor(X_train_for_sel, use = "pairwise.complete.obs")
d  <- as.dist(1 - abs(C))
hc <- hclust(d, method = "average")

# Cut height: 0.1 ≈ keep 1 from any group with |ρ| > 0.9
groups <- cutree(hc, h = 0.1)

# Keep the column in each cluster with the fewest NAs 
keepers <- tapply(colnames(X_train_for_sel), groups, function(cols) {
  nafrac <- colMeans(is.na(X_train_for_sel[, cols, drop = FALSE]))
  cols[which.min(nafrac)]
})

vars_X <- unname(unlist(keepers))
# Prepare initial training matrices
X0 <- as.matrix(df_train_std[1:t0_idx, vars_X, drop = FALSE])
Y0 <- as.matrix(df_train_std[1:t0_idx, vars_Y,     drop = FALSE])

# How many columns are transformed withing the remaining ones?
common <- intersect(names(df_train_clean)[-1], names(df_train)[-1])
changed <- sum(sapply(common, function(nm) !identical(df_train_clean[[nm]], df_train[[nm]][-1])))

# Which columns are deleted (es. all NA after logdiff or sd≈0)?
dropped <- setdiff(names(df_train)[-1], names(df_train_clean)[-1])

# Dimension X/Y for safety
cat("Dim X0: ", paste(dim(X0), collapse=" x "), " | Dim Y0: ", paste(dim(Y0), collapse=" x "), "\n")

# Are there NA in X0?
X0 <- as.matrix(df_train_std[1:t0_idx, vars_X, drop = FALSE])

# ================================
# 3. Bai-Ng factors selection
# ================================

# Definition before applying 
bn_select_r_icp2 <- function(X, rmax = 10) {
  X <- as.matrix(X)
  Tn <- nrow(X); Nn <- ncol(X)
  rmax <- min(rmax, Nn - 1, floor(0.2 * min(Nn, Tn)))
  Xc <- scale(X, center = TRUE, scale = FALSE)
  sv <- svd(Xc)
  d2 <- sv$d^2
  tail_ss <- rev(cumsum(rev(d2)))
  rgrid <- 1:min(rmax, length(d2) - 1)
  SSR_vec <- tail_ss[rgrid + 1]
  V_r <- SSR_vec / (Nn * Tn)
  C_NT <- (Nn + Tn) / (Nn * Tn)
  Lmin <- log(min(Nn, Tn))
  ICp2 <- log(V_r) + rgrid * C_NT * Lmin
  r_sel <- rgrid[which.min(ICp2)]
  list(r = r_sel, IC = data.frame(r = rgrid, ICp2 = ICp2))
}

# Simple imputation ONLY for r selection 
X0_imp <- X0
X0_imp[!is.finite(X0_imp)] <- NA
for (j in seq_len(ncol(X0_imp))) {
  m <- mean(X0_imp[, j], na.rm = TRUE); if (!is.finite(m)) m <- 0
  X0_imp[is.na(X0_imp[, j]), j] <- m
}
bn <- bn_select_r_icp2(X0_imp, rmax = 10)
r_fixed <- bn$r
message("Bai–Ng ICp2 (cap=10) selected r = ", r_fixed)

pca_res <- prcomp(X0_imp, center = TRUE, scale. = TRUE)

# Extract variance explained
eigvals <- pca_res$sdev^2
var_explained <- eigvals / sum(eigvals)
cumvar_explained <- cumsum(var_explained)

df_pca <- data.frame(
  Component = seq_along(eigvals),
  Variance = var_explained,
  Cumulative = cumvar_explained
)
# 
# # Bai–Ng selection (if not yet run) 
# bn <- bn_select_r_icp2(X0_imp, rmax = 50)
# r_selected <- bn$r

# Plot cumulative variance explained 

ggplot(df_pca, aes(x = Component, y = Cumulative)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_vline(xintercept = r_selected, linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate(
    "text", 
    x = r_selected + 0.3, 
    y = cumvar_explained[r_selected], 
    label = paste0("Bai–Ng selected r = ", r_selected),
    color = "red", 
    hjust = 0, 
    vjust = -0.5, 
    size = 4
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Cumulative Variance Explained by Principal Components",
    
    x = "Number of Factors (Principal Components)",
    y = "Cumulative Variance Explained (%)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )


# ================================
# 4. Forecasting with PCA + VAR (Dynamic Factor Model)
# ================================

nY <- length(vars_Y)
nOrg <- length(oos_origin_idx)
h_long <- 4
p_fix <- 2
L_ols <- 1

pred_h1_real <- pred_h4_real <- real_h1_real <- real_h4_real <-
  matrix(NA, nrow = nOrg, ncol = nY, dimnames = list(NULL, vars_Y))

# new matrices for standardised version (for metrics)
real_h1_std <- real_h4_std <- pred_h1_std_mat <- pred_h4_std_mat <-
  matrix(NA, nrow = nOrg, ncol = nY, dimnames = list(NULL, vars_Y))

origin_dates <- df_train_std$date[oos_origin_idx]
h1_dates <- df_train_std$date[oos_origin_idx + 1]
h4_dates <- df_train_std$date[oos_origin_idx + h_long]

cat("\n=== Expanding-window forecasting ===\n")

k <- 0
for (tcut in oos_origin_idx) {
  k <- k + 1
  cat("Origin", k, "/", nOrg, "— fino a", as.character(df_train_std$date[tcut]), "\n")
  
  X_train <- as.matrix(df_train_std[1:tcut, vars_X, drop = FALSE])
  Y_train <- as.matrix(df_train_std[1:tcut, vars_Y, drop = FALSE])
  
  X_mean <- colMeans(X_train, na.rm = TRUE)
  X_sd <- apply(X_train, 2, sd, na.rm = TRUE); X_sd[X_sd == 0] <- 1
  X_std <- scale(X_train, center = X_mean, scale = X_sd)
  
  for (j in seq_len(ncol(X_std))) {
    m <- mean(X_std[, j], na.rm = TRUE)
    if (!is.finite(m)) m <- 0
    X_std[is.na(X_std[, j]), j] <- m
  }
  
  Y_mean <- colMeans(Y_train, na.rm = TRUE)
  Y_sd <- apply(Y_train, 2, sd, na.rm = TRUE); Y_sd[Y_sd == 0] <- 1
  Y_std <- scale(Y_train, center = Y_mean, scale = Y_sd)
  
  pca_fit <- prcomp(X_std, center = FALSE, scale. = FALSE)
  F_train <- pca_fit$x[, 1:r_fixed, drop = FALSE]
  colnames(F_train) <- paste0("F", seq_len(r_fixed))
  
  Tn <- nrow(F_train)
  if (Tn <= L_ols + 1) next
  
  F_curr <- F_train[(1+L_ols):Tn, , drop = FALSE]
  F_lag1 <- F_train[1:(Tn-L_ols), , drop = FALSE]
  X_ols <- cbind(F_curr, F_lag1)
  Y_ols <- Y_std[(1+L_ols):Tn, , drop = FALSE]
  
  beta_hat <- sapply(seq_len(ncol(Y_ols)), function(j) coef(lm(Y_ols[, j] ~ X_ols)))
  rownames(beta_hat) <- c("const", colnames(X_ols))
  colnames(beta_hat) <- colnames(Y_ols)
  
  var_fit <- VAR(as.data.frame(F_train), p = p_fix, type = "const")
  fc <- predict(var_fit, n.ahead = h_long)
  fac_names <- colnames(F_train)
  F_h1 <- sapply(fac_names, function(nm) fc$fcst[[nm]][1, "fcst"])
  F_h3 <- sapply(fac_names, function(nm) fc$fcst[[nm]][h_long-1, "fcst"])
  F_h4 <- sapply(fac_names, function(nm) fc$fcst[[nm]][h_long, "fcst"])
  
  F_t <- as.numeric(F_train[nrow(F_train), ])
  x_h1_const <- c(1, F_h1, F_t)
  x_h4_const <- c(1, F_h4, F_h3)
  
  # standardised scale forecasts (window)
  pred_h1_std <- as.numeric(x_h1_const %*% beta_hat)
  pred_h4_std <- as.numeric(x_h4_const %*% beta_hat)
  pred_h1_std_mat[k, ] <- pred_h1_std
  pred_h4_std_mat[k, ] <- pred_h4_std
  
  # return to real scala (diff/logdiff of window)
  pred_h1_real[k, ] <- pred_h1_std * Y_sd + Y_mean
  pred_h4_real[k, ] <- pred_h4_std * Y_sd + Y_mean
  
  # actuals at real scale (for pred_h*_real)
  real_h1_real[k, ] <- as.numeric(df_train_clean[tcut + 1, vars_Y])
  real_h4_real[k, ] <- as.numeric(df_train_clean[tcut + h_long, vars_Y])
  
  # actuals in standardized scale  (same mean/sd of window, for metrics)
  real_h1_std[k, ] <- (as.numeric(df_train_clean[tcut + 1, vars_Y]) - Y_mean) / Y_sd
  real_h4_std[k, ] <- (as.numeric(df_train_clean[tcut + h_long, vars_Y]) - Y_mean) / Y_sd
}

cat("\n=== Forecast loop completed ===\n")


# ================================
# 5) OOS metrics + Back-transformation to levels
# ================================

# Safety checks (avoid cryptic errors) 
req_objs <- c("vars_Y","metadata","df_train",
              "origin_dates","h1_dates","h4_dates","oos_origin_idx","h_long",
              "pred_h1_real","pred_h4_real","real_h1_real","real_h4_real")
miss <- req_objs[!vapply(req_objs, exists, logical(1))]
if (length(miss)) stop("Those objects are missing: ", paste(miss, collapse=", "))

# Helper: RMSE / MAE 
rmse <- function(e) sqrt(mean(e^2, na.rm = TRUE))
mae  <- function(e) mean(abs(e), na.rm = TRUE)

# Metrics in transformed scale 
rmse_h1_tr <- sapply(seq_len(ncol(real_h1_real)), function(j) rmse(real_h1_real[, j] - pred_h1_real[, j]))
rmse_h4_tr <- sapply(seq_len(ncol(real_h4_real)), function(j) rmse(real_h4_real[, j] - pred_h4_real[, j]))
mae_h1_tr  <- sapply(seq_len(ncol(real_h1_real)), function(j) mae(real_h1_real[, j] - pred_h1_real[, j]))
mae_h4_tr  <- sapply(seq_len(ncol(real_h4_real)), function(j) mae(real_h4_real[, j] - pred_h4_real[, j]))

metrics_transformed <- data.frame(
  variable = colnames(real_h1_real),
  RMSE_h1  = rmse_h1_tr,
  RMSE_h4  = rmse_h4_tr,
  MAE_h1   = mae_h1_tr,
  MAE_h4   = mae_h4_tr,
  row.names = NULL
)
cat("\n=== Forecast Evaluation (Transformed Units) ===\n")
print(metrics_transformed)

# Back-transform to levels (h=1 e h=4) 
get_transform <- function(var) {
  i <- match(var, metadata$ts_key)
  tr <- if (is.na(i)) NA_character_ else metadata$transform[i]
  ifelse(is.na(tr), "none", tolower(tr))
}

# historical levels (non transformed)
levels_df <- df_train[, c("date", vars_Y)]

# Pre-allocation of containers (lists -> data.frame)
pred_h1_level <- setNames(vector("list", length(vars_Y)), vars_Y)
pred_h4_level <- setNames(vector("list", length(vars_Y)), vars_Y)
real_h1_level <- setNames(vector("list", length(vars_Y)), vars_Y)
real_h4_level <- setNames(vector("list", length(vars_Y)), vars_Y)

for (v in vars_Y) {
  tr_v <- get_transform(v)
  base_h1 <- levels_df[[v]][oos_origin_idx]
  base_h4 <- levels_df[[v]][oos_origin_idx + (h_long - 1)]
  L_t1 <- levels_df[[v]][oos_origin_idx + 1]
  L_t4 <- levels_df[[v]][oos_origin_idx + h_long]
  yhat_h1 <- pred_h1_real[, v]
  yhat_h4 <- pred_h4_real[, v]
  
  if (v %in% growth_vars) {
    # Keep in Δlog×100 (QoQ %)
    pred_h1_level[[v]] <- yhat_h1
    pred_h4_level[[v]] <- yhat_h4
    real_h1_level[[v]] <- real_h1_real[, v]
    real_h4_level[[v]] <- real_h4_real[, v]
  } else if (tr_v == "logdiff") {
    # Back to levels (index / rate)
    pred_h1_level[[v]] <- base_h1 * exp(yhat_h1 / 100)
    pred_h4_level[[v]] <- base_h4 * exp(yhat_h4 / 100)
    real_h1_level[[v]] <- L_t1
    real_h4_level[[v]] <- L_t4
  } else if (tr_v == "diff") {
    pred_h1_level[[v]] <- base_h1 + yhat_h1
    pred_h4_level[[v]] <- base_h4 + yhat_h4
    real_h1_level[[v]] <- L_t1
    real_h4_level[[v]] <- L_t4
  } else {
    pred_h1_level[[v]] <- yhat_h1
    pred_h4_level[[v]] <- yhat_h4
    real_h1_level[[v]] <- L_t1
    real_h4_level[[v]] <- L_t4
  }
}

# for (v in vars_Y) {
#   tr_v <- get_transform(v)
#   # base for back-transform
#   base_h1 <- levels_df[[v]][oos_origin_idx]             # L_t (per h=1)
#   base_h4 <- levels_df[[v]][oos_origin_idx + (h_long-1)]# L_{t+3} (per h=4)
#   # reals (levels) a t+1 e t+4
#   L_t1 <- levels_df[[v]][oos_origin_idx + 1]
#   L_t4 <- levels_df[[v]][oos_origin_idx + h_long]
#   
#   # forecasts in transformed units
#   yhat_h1 <- pred_h1_real[, v]
#   yhat_h4 <- pred_h4_real[, v]
#   
#   # back-transform
#   if (tr_v == "logdiff") {
#     # y is 100 * Δlog -> multiplier exp(y/100)
#     pred_h1_level[[v]] <- base_h1 * exp(yhat_h1 / 100)
#     pred_h4_level[[v]] <- base_h4 * exp(yhat_h4 / 100)
#   } else if (tr_v == "diff") {
#     pred_h1_level[[v]] <- base_h1 + yhat_h1
#     pred_h4_level[[v]] <- base_h4 + yhat_h4
#   } else { # none
#     pred_h1_level[[v]] <- yhat_h1
#     pred_h4_level[[v]] <- yhat_h4
#   }
#   
#   # reals in levels
#   real_h1_level[[v]] <- L_t1
#   real_h4_level[[v]] <- L_t4
# }

# Lists -> data.frame
pred_h1_level <- as.data.frame(pred_h1_level, check.names = FALSE)
pred_h4_level <- as.data.frame(pred_h4_level, check.names = FALSE)
real_h1_level <- as.data.frame(real_h1_level, check.names = FALSE)
real_h4_level <- as.data.frame(real_h4_level, check.names = FALSE)

# ---- Metrics in levels ----
rmse_h1_lvl <- sapply(vars_Y, function(v) rmse(real_h1_level[[v]] - pred_h1_level[[v]]))
rmse_h4_lvl <- sapply(vars_Y, function(v) rmse(real_h4_level[[v]] - pred_h4_level[[v]]))
mae_h1_lvl  <- sapply(vars_Y, function(v) mae(real_h1_level[[v]] - pred_h1_level[[v]]))
mae_h4_lvl  <- sapply(vars_Y, function(v) mae(real_h4_level[[v]] - pred_h4_level[[v]]))

metrics_levels <- data.frame(
  variable = vars_Y,
  RMSE_h1  = as.numeric(rmse_h1_lvl),
  RMSE_h4  = as.numeric(rmse_h4_lvl),
  MAE_h1   = as.numeric(mae_h1_lvl),
  MAE_h4   = as.numeric(mae_h4_lvl),
  row.names = NULL
)
cat("\n=== Forecast Evaluation (Levels) ===\n")
print(metrics_levels)

# Build oos_results (trasformed + levels) 
oos_results <- data.frame(
  origin_date = origin_dates,
  h1_date = h1_dates,
  h4_date = h4_dates
)

# transformed 
oos_results <- cbind(
  oos_results,
  setNames(as.data.frame(pred_h1_real), paste0(vars_Y, "_pred_h1_tr")),
  setNames(as.data.frame(real_h1_real), paste0(vars_Y, "_real_h1_tr")),
  setNames(as.data.frame(pred_h4_real), paste0(vars_Y, "_pred_h4_tr")),
  setNames(as.data.frame(real_h4_real), paste0(vars_Y, "_real_h4_tr"))
)

# levels (back-transformed)
oos_results <- cbind(
  oos_results,
  setNames(pred_h1_level, paste0(vars_Y, "_pred_h1_lvl")),
  setNames(real_h1_level, paste0(vars_Y, "_real_h1_lvl")),
  setNames(pred_h4_level, paste0(vars_Y, "_pred_h4_lvl")),
  setNames(real_h4_level, paste0(vars_Y, "_real_h4_lvl"))
)

cat("\n=== OOS results (head) ===\n")
print(head(oos_results, 5))

# ================================
#  6. Plot — OOS transformed scale (model units)
# ================================
graphics.off()

plot_df_h1_tr <- oos_results %>%
  dplyr::select(h1_date, dplyr::ends_with("_pred_h1_tr"), dplyr::ends_with("_real_h1_tr")) %>%
  dplyr::rename(date = h1_date)

plot_df_h4_tr <- oos_results %>%
  dplyr::select(h4_date, dplyr::ends_with("_pred_h4_tr"), dplyr::ends_with("_real_h4_tr")) %>%
  dplyr::rename(date = h4_date)

long_h1_tr <- plot_df_h1_tr %>%
  pivot_longer(
    cols = -date,
    names_to = c("variable", "type"),
    names_pattern = "(.+)_(pred|real)_h1_tr",
    values_to = "value"
  ) %>% mutate(horizon = "h=1 (1Q ahead)")

long_h4_tr <- plot_df_h4_tr %>%
  pivot_longer(
    cols = -date,
    names_to = c("variable", "type"),
    names_pattern = "(.+)_(pred|real)_h4_tr",
    values_to = "value"
  ) %>% mutate(horizon = "h=4 (1Y ahead)")

long_all_tr <- bind_rows(long_h1_tr, long_h4_tr) %>%
  mutate(
    date = as.Date(date),
    type = factor(type, levels = c("real","pred")),
    horizon = factor(horizon, levels = c("h=1 (1Q ahead)", "h=4 (1Y ahead)"))
  )

long_all_tr$variable <- recode(long_all_tr$variable,
                               rvgdp    = "Real GDP Growth",
                               cpi  = "CPI",
                               wkfreuro = "Exchange Rate"
)

p_tr <- ggplot(long_all_tr, aes(x = date, y = value, color = type, linetype = type)) +
  geom_line(linewidth = 0.7) +
  facet_grid(variable ~ horizon, scales = "free_y") +
  scale_color_manual(values = c("real" = "black", "pred" = "steelblue")) +
  scale_linetype_manual(values = c("real" = "solid", "pred" = "dashed")) +
  labs(title = "OOS Forecasts — Transformed Units",
       x = "Date", y = "Transformed value", color = "", linetype = "") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", strip.text = element_text(face="bold"))
print(p_tr)

# ================================
#  6.1 Plot — OOS levels (back-transformed)
# ================================
plot_df_h1_lvl <- oos_results %>%
  dplyr::select(h1_date, dplyr::ends_with("_pred_h1_lvl"), dplyr::ends_with("_real_h1_lvl")) %>%
  dplyr::rename(date = h1_date)

plot_df_h4_lvl <- oos_results %>%
  dplyr::select(h4_date, dplyr::ends_with("_pred_h4_lvl"), dplyr::ends_with("_real_h4_lvl")) %>%
  dplyr::rename(date = h4_date)

long_h1_lvl <- plot_df_h1_lvl %>%
  pivot_longer(
    cols = -date,
    names_to = c("variable", "type"),
    names_pattern = "(.+)_(pred|real)_h1_lvl",
    values_to = "value"
  ) %>% mutate(horizon = "h=1 (1Q ahead)")

long_h4_lvl <- plot_df_h4_lvl %>%
  pivot_longer(
    cols = -date,
    names_to = c("variable", "type"),
    names_pattern = "(.+)_(pred|real)_h4_lvl",
    values_to = "value"
  ) %>% mutate(horizon = "h=4 (1Y ahead)")

long_all_lvl <- bind_rows(long_h1_lvl, long_h4_lvl) %>%
  mutate(
    date = as.Date(date),
    type = factor(type, levels = c("real","pred")),
    horizon = factor(horizon, levels = c("h=1 (1Q ahead)", "h=4 (1Y ahead)"))
  )

long_all_lvl$variable <- recode(long_all_lvl$variable,
                                rvgdp    = "Real GDP Growth",
                                cpi  = "CPI",
                                wkfreuro = "Exchange Rate"
)

graphics.off()
p_lvl <- ggplot(long_all_lvl, aes(x = date, y = value, color = type, linetype = type)) +
  geom_line(linewidth = 0.7) +
  facet_grid(variable ~ horizon, scales = "free_y") +
  scale_color_manual(values = c("real" = "black", "pred" = "steelblue")) +
  scale_linetype_manual(values = c("real" = "solid", "pred" = "dashed")) +
  labs(title = "OOS Forecasts — Back-transformed Levels",
       x = "Date", y = "Level", color = "", linetype = "") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", strip.text = element_text(face="bold"))
print(p_lvl)


# =========================
#   7. "NOW" FORECASTS from latest data (h=1 and h=4)
# =========================

# Keep only observed data up to today 
# Use full dataset up to today for the NOW forecast (post-training)
df_now_all <- df %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  mutate(across(-date, as.numeric)) %>%
  filter(date <= as.Date("2025-07-01"))

for (i in seq_len(nrow(metadata))) {
  key <- metadata$ts_key[i]
  t_type <- metadata$transform[i]
  if (!is.na(t_type) && key %in% names(df_now_all)) {
    df_now_all[[key]] <- transform_variable(df_now_all[[key]], t_type)
  }
}

# Remove first row after differencing
df_now_all <- df_now_all[-1, ]

# Standardize using the same approach as before
df_now_std <- df_now_all
df_now_std[-1] <- scale(df_now_all[-1])
# Decide predictor set for NOW (frozen on initial 80%)
X_now_names <- vars_X

# Build working frame
needed_cols <- c("date", vars_Y, X_now_names)
needed_cols <- intersect(needed_cols, names(df_now_all))
df_now <- df_now_all[, needed_cols, drop = FALSE]
df_now <- df_now[order(df_now$date), , drop = FALSE]
row.names(df_now) <- NULL

# Split into X and Y (full history up to last observed quarter)
X_all <- as.matrix(df_now[, X_now_names, drop = FALSE])
Y_all <- as.matrix(df_now[, vars_Y,      drop = FALSE])

# Standardize using full-history moments (we are producing NOW forecasts)
X_mean <- apply(X_all, 2, mean, na.rm = TRUE)
X_sd   <- apply(X_all, 2, sd,   na.rm = TRUE); X_sd[X_sd == 0] <- 1
X_std  <- scale(X_all, center = X_mean, scale = X_sd)

Y_mean <- apply(Y_all, 2, mean, na.rm = TRUE)
Y_sd   <- apply(Y_all, 2, sd,   na.rm = TRUE); Y_sd[Y_sd == 0] <- 1
Y_std  <- scale(Y_all, center = Y_mean, scale = Y_sd)

# Only impute standardised X values: NA -> 0 (which on a standard scale = mean of the variable)
X_std_imp <- X_std
for (j in seq_len(ncol(X_std_imp))) {
  X_std_imp[is.na(X_std_imp[, j]), j] <- 0
}

# Select number of factors r (reuse r_fixed if available; else choose on initial 80%)
if (exists("r_fixed")) {
  r_now <- r_fixed
  message("NOW: using r_fixed = ", r_now)
} else {
  t0_now <- max(1L, floor(0.8 * nrow(X_std)))
  X0_now <- X_std_imp[1:t0_now, , drop = FALSE]
  bn_now <- bn_select_r_icp2(X0_now, rmax = 10)
  # bn_now <- bn_select_r(X0_now, crit = "ICp3")
  r_now  <- max(1, bn_now$r)
  message("NOW: r selected by ICp3 on initial 80% = ", r_now)
}

# PCA on standardized X (full history)
pca_now <- prcomp(X_std_imp, center = FALSE, scale. = FALSE)

# Dimensional protection of r 
r_use <- min(r_now, ncol(pca_now$x))
F_all <- pca_now$x[, 1:r_use, drop = FALSE]
colnames(F_all) <- paste0("F", seq_len(r_use))

# OLS with factor lags (L=1): Y_std ~ [1, F_t, F_{t-1}]
L_ols <- 1
Tn <- nrow(F_all); r <- ncol(F_all)
stopifnot(Tn > L_ols)

F_curr <- F_all[(1+L_ols):Tn, , drop = FALSE]        # F_t
F_lag1 <- F_all[1:(Tn-L_ols), , drop = FALSE]        # F_{t-1}

# Design matrix with stable names [F1..Fr | L1_F1..L1_Fr]
X_ols <- cbind(F_curr, F_lag1)
colnames(X_ols) <- c(paste0("F", 1:r), paste0("L1_F", 1:r))
Xdf <- as.data.frame(X_ols)

Y_ols <- Y_std[(1+L_ols):Tn, , drop = FALSE]         # align Y

# --- Robust OLS to coefficients names---
X_ols_const_names <- c("(Intercept)", colnames(X_ols))
beta_hat_now <- array(
  NA_real_,
  dim = c(length(X_ols_const_names), ncol(Y_ols)),
  dimnames = list(X_ols_const_names, colnames(Y_ols))
)

for (j in seq_len(ncol(Y_ols))) {
  fit_j <- lm(Y_ols[, j] ~ ., data = Xdf)  # include intercept
  cf <- coef(fit_j)
  # reordnung/cleaning (if something missing, put 0)
  beta_vec <- setNames(numeric(length(X_ols_const_names)), X_ols_const_names)
  beta_vec[names(cf)] <- cf
  beta_hat_now[, j] <- beta_vec
}

# VAR on factors (levels) with fixed lag p=2 (more stable)
p_fix <- 2
var_now <- VAR(as.data.frame(F_all), p = p_fix, type = "const")

# Forecast factors to h=1 and h=4
h_long <- 4
fc_now <- predict(var_now, n.ahead = h_long)
fac_names <- colnames(F_all)

F_h1 <- sapply(fac_names, function(nm) fc_now$fcst[[nm]][1, "fcst"])  # F_{t+1}
F_h3 <- sapply(fac_names, function(nm) fc_now$fcst[[nm]][3, "fcst"])  # F_{t+3}
F_h4 <- sapply(fac_names, function(nm) fc_now$fcst[[nm]][4, "fcst"])  # F_{t+4}
F_t  <- as.numeric(F_all[nrow(F_all), , drop = FALSE])                # F_t

# Map factor forecasts to Y (std) using same ordnung of coefficients
x_h1 <- c("(Intercept)" = 1,
          setNames(F_h1, paste0("F", 1:r)),
          setNames(F_t,  paste0("L1_F", 1:r)))
x_h4 <- c("(Intercept)" = 1,
          setNames(F_h4, paste0("F", 1:r)),
          setNames(F_h3, paste0("L1_F", 1:r)))

Y_h1_std <- as.numeric(t(x_h1[X_ols_const_names]) %*% beta_hat_now)
Y_h4_std <- as.numeric(t(x_h4[X_ols_const_names]) %*% beta_hat_now)

# De-standardize back to transformed units
Y_h1_real <- Y_h1_std * Y_sd + Y_mean
Y_h4_real <- Y_h4_std * Y_sd + Y_mean
names(Y_h1_real) <- vars_Y
names(Y_h4_real) <- vars_Y

# Target dates (next quarter and +4 quarters)
last_date <- max(df_now_all$date, na.rm = TRUE)
h1_date   <- seq(last_date, by = "quarter", length.out = 2)[2]
h4_date   <- seq(last_date, by = "quarter", length.out = 5)[5]

now_table <- data.frame(
  variable = vars_Y,
  target_h1_date = h1_date,
  forecast_h1    = as.numeric(Y_h1_real),
  target_h4_date = h4_date,
  forecast_h4    = as.numeric(Y_h4_real)
)

# =========================
#   7.1. NOW: Route h=1..4 e back-transform to levels (recursive)
# =========================

# Factors predicted F_{t+1..t+4} by the VAR already estimated (fc_now)
fac_names <- colnames(F_all)
get_fc <- function(step) sapply(fac_names, function(nm) fc_now$fcst[[nm]][step, "fcst"])
F_h1 <- get_fc(1)
F_h2 <- get_fc(2)
F_h3 <- get_fc(3)
F_h4 <- get_fc(4)
F_t  <- as.numeric(F_all[nrow(F_all), , drop = FALSE])

# Same order as regressors of OLS: x_h = [ (Intercept), F_{t+h}, F_{t+h-1} ]
make_xh <- function(F_h, F_hm1, r) {
  c("(Intercept)" = 1,
    setNames(F_h,   paste0("F", 1:r)),
    setNames(F_hm1, paste0("L1_F", 1:r)))
}
r <- ncol(F_all)
x1 <- make_xh(F_h1, F_t,  r)
x2 <- make_xh(F_h2, F_h1, r)
x3 <- make_xh(F_h3, F_h2, r)
x4 <- make_xh(F_h4, F_h3, r)

# Predictions in TRANSFORMED UNITS (std -> real of the model)
yh1_std <- as.numeric(t(x1[X_ols_const_names]) %*% beta_hat_now)
yh2_std <- as.numeric(t(x2[X_ols_const_names]) %*% beta_hat_now)
yh3_std <- as.numeric(t(x3[X_ols_const_names]) %*% beta_hat_now)
yh4_std <- as.numeric(t(x4[X_ols_const_names]) %*% beta_hat_now)

yh1 <- yh1_std * Y_sd + Y_mean
yh2 <- yh2_std * Y_sd + Y_mean
yh3 <- yh3_std * Y_sd + Y_mean
yh4 <- yh4_std * Y_sd + Y_mean
names(yh1) <- names(yh2) <- names(yh3) <- names(yh4) <- vars_Y

# # Recursive back-transform at LEVELS
# get_transform <- function(var) {
#   i <- match(var, metadata$ts_key)
#   tr <- if (is.na(i)) NA_character_ else metadata$transform[i]
#   ifelse(is.na(tr), "none", tolower(tr))
# }

levels_df <- df %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  dplyr::select(date, all_of(vars_Y))
last_date <- max(df_now_all$date, na.rm = TRUE)

# last observed level L_t for each Y
L_t_vec <- setNames(numeric(length(vars_Y)), vars_Y)
for (v in vars_Y) {
  L_t_val <- levels_df[[v]][levels_df$date == last_date]
  if (length(L_t_val) != 1 || !is.finite(L_t_val)) {
    stop("NOW back-transform: don't find L_t for variabile ", v)
  }
  L_t_vec[v] <- L_t_val
}

# Propagation L_{t+h} starting from L_t using yh1..yh4
L_h1 <- L_h2 <- L_h3 <- L_h4 <- setNames(numeric(length(vars_Y)), vars_Y)
for (v in vars_Y) {
  tr_v <- get_transform(v)
  Lt   <- L_t_vec[v]
  if (v %in% growth_vars) {
    # GDP stays in % growth
    L1 <- yh1[v]; L2 <- yh2[v]; L3 <- yh3[v]; L4 <- yh4[v]
  } else if (tr_v == "logdiff") {
    L1 <- Lt * exp(yh1[v] / 100)
    L2 <- L1 * exp(yh2[v] / 100)
    L3 <- L2 * exp(yh3[v] / 100)
    L4 <- L3 * exp(yh4[v] / 100)
  } else if (tr_v == "diff") {
    L1 <- Lt + yh1[v]
    L2 <- L1 + yh2[v]
    L3 <- L2 + yh3[v]
    L4 <- L3 + yh4[v]
  } else {
    L1 <- yh1[v]; L2 <- yh2[v]; L3 <- yh3[v]; L4 <- yh4[v]
  }
  L_h1[v] <- L1; L_h2[v] <- L2; L_h3[v] <- L3; L_h4[v] <- L4
}

# for (v in vars_Y) {
#   tr_v <- get_transform(v)
#   Lt   <- L_t_vec[v]
#   if (tr_v == "logdiff") {
#     L1 <- Lt * exp(yh1[v] / 100)
#     L2 <- L1 * exp(yh2[v] / 100)
#     L3 <- L2 * exp(yh3[v] / 100)
#     L4 <- L3 * exp(yh4[v] / 100)
#   } else if (tr_v == "diff") {
#     L1 <- Lt + yh1[v]
#     L2 <- L1 + yh2[v]
#     L3 <- L2 + yh3[v]
#     L4 <- L3 + yh4[v]
#   } else { # none
#     L1 <- yh1[v]; L2 <- yh2[v]; L3 <- yh3[v]; L4 <- yh4[v]
#   }
#   L_h1[v] <- L1; L_h2[v] <- L2; L_h3[v] <- L3; L_h4[v] <- L4
# }

# FINAL TABLES
h1_date <- seq(last_date, by = "quarter", length.out = 2)[2]
h4_date <- seq(last_date, by = "quarter", length.out = 5)[5]

now_table_transformed <- data.frame(
  variable       = vars_Y,
  target_h1_date = h1_date,
  forecast_h1    = as.numeric(yh1[vars_Y]),
  target_h4_date = h4_date,
  forecast_h4    = as.numeric(yh4[vars_Y]),
  row.names = NULL
)

now_table_levels <- data.frame(
  variable            = vars_Y,
  target_h1_date      = h1_date,
  forecast_h1_level   = as.numeric(L_h1[vars_Y]),
  target_h4_date      = h4_date,
  forecast_h4_level   = as.numeric(L_h4[vars_Y]),
  row.names = NULL, check.names = FALSE
)

print(now_table_transformed)  # Transformed (Δlog*100 or diff)
print(now_table_levels) # Levels

# =========================
#   8. NOW: PLOT VS KOF forecasts
# =========================

last_date <- max(df_now_all$date, na.rm = TRUE)
h1_date   <- now_table_levels$target_h1_date[1]
h4_date   <- now_table_levels$target_h4_date[1]

levels_recent <- df_train %>%
  dplyr::select(date, all_of(vars_Y)) %>%
  dplyr::filter(date >= as.Date("2022-01-01")) %>%
  tidyr::pivot_longer(-date, names_to = "variable", values_to = "value")

our_pts <- bind_rows(
  now_table_levels %>% transmute(variable, date = target_h1_date, value = forecast_h1_level, horizon = "h1", source = "Our"),
  now_table_levels %>% transmute(variable, date = target_h4_date, value = forecast_h4_level, horizon = "h4", source = "Our")
)

df_growth <- df %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  arrange(date) %>%
  mutate(rvgdp_growth = c(NA, diff(log(rvgdp)) * 100))  # QoQ %

# Replace GDP level with growth in KOF comparison
df_kof_growth <- df_growth %>%
  dplyr::select(date, rvgdp_growth, cpi, wkfreuro) %>%
  rename(rvgdp = rvgdp_growth)

df_kof_levels <- df_kof_growth

get_kof_value <- function(date_target, varname) {
  if (!varname %in% names(df_kof_levels)) return(NA_real_)
  out <- df_kof_levels %>% dplyr::filter(date == date_target) %>% dplyr::pull(!!sym(varname))
  if (length(out) == 0) NA_real_ else out[1]
}

kof_pts <- bind_rows(
  tibble(variable = vars_Y, date = h1_date,
         value = sapply(vars_Y, function(v) get_kof_value(h1_date, v)),
         horizon = "h1", source = "KOF"),
  tibble(variable = vars_Y, date = h4_date,
         value = sapply(vars_Y, function(v) get_kof_value(h4_date, v)),
         horizon = "h4", source = "KOF")
) %>% dplyr::filter(is.finite(value))

pal_colors <- c("Our" = "#1f77b4", "KOF" = "#ff7f0e")
pal_shapes <- c("Our" = 16,       "KOF" = 17)

# ensure date are Date
levels_recent$date <- as.Date(levels_recent$date)
our_pts$date       <- as.Date(our_pts$date)
kof_pts$date       <- as.Date(kof_pts$date)

# Chart names
facet_labels <- c(
  "rvgdp"    = "Real GDP Growth",
  "cpi"      = "Inflation (CPI)",
  "wkfreuro" = "CHF/EUR Exchange Rate"
)

p <- ggplot() +
  geom_line(data = levels_recent,
            aes(x = date, y = value),
            linewidth = 0.6, color = "grey40") +
  geom_vline(xintercept = as.numeric(last_date),
             linetype = "solid", color = "grey50") +
  geom_point(data = bind_rows(our_pts, kof_pts),
             aes(x = date, y = value, color = source, shape = source),
             size = 3.2) +
  facet_wrap(
    ~ variable,
    scales = "free_y",
    ncol   = 1,
    labeller = as_labeller(facet_labels)   # TITLES
  ) +
  # legend
  scale_color_manual(
    values = pal_colors,
    breaks = c("Our", "KOF"),
    labels = c("Dynamic Factor Model", "KOF forecast"),
    name   = ""
  ) +
  scale_shape_manual(
    values = pal_shapes,
    breaks = c("Our", "KOF"),
    labels = c("Dynamic Factor Model", "KOF forecast"),
    name   = ""
  ) +
  labs(title = "Forecast Comparison: Dynamic Factor Model (PCA+VAR) vs KOF",
       subtitle = paste0("Historical until last observed value: ",
                         format(last_date, "%Y-%m-%d")),
       x = "Date", y = "Levels") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )

print(p)

# ================================
# 9. Forecasting with FVAR (Factor-Augmented VAR)
# ================================

# We use the same targets, same origins, and same h_long
nY    <- length(vars_Y)
nOrg  <- length(oos_origin_idx)
h_long <- 4
p_fix  <- 2  # same p as the VAR on factors

# Matrices for FVAR forecasts
pred_h1_real_fvar <- pred_h4_real_fvar <-
  matrix(NA, nrow = nOrg, ncol = nY, dimnames = list(NULL, vars_Y))

pred_h1_std_fvar_mat <- pred_h4_std_fvar_mat <-
  matrix(NA, nrow = nOrg, ncol = nY, dimnames = list(NULL, vars_Y))

cat("\n=== Expanding-window forecasting: FVAR ===\n")

k <- 0
for (tcut in oos_origin_idx) {
  k <- k + 1
  cat("FVAR Origin", k, "/", nOrg, "— up to", as.character(df_now_std$date[tcut]), "\n")
  
  # Data up to origin tcut
  X_train <- as.matrix(df_now_std[1:tcut, vars_X, drop = FALSE])
  Y_train <- as.matrix(df_now_std[1:tcut, vars_Y, drop = FALSE])
  
  # Standardization of X
  X_mean <- colMeans(X_train, na.rm = TRUE)
  X_sd   <- apply(X_train, 2, sd, na.rm = TRUE); X_sd[X_sd == 0] <- 1
  X_std  <- scale(X_train, center = X_mean, scale = X_sd)
  # Impute NA in X_std with mean (0 in standardized scale)
  for (j in seq_len(ncol(X_std))) {
    m <- mean(X_std[, j], na.rm = TRUE); if (!is.finite(m)) m <- 0
    X_std[is.na(X_std[, j]), j] <- m
  }
  
  # Standardization of Y
  Y_mean <- colMeans(Y_train, na.rm = TRUE)
  Y_sd   <- apply(Y_train, 2, sd, na.rm = TRUE); Y_sd[Y_sd == 0] <- 1
  Y_std  <- scale(Y_train, center = Y_mean, scale = Y_sd)
  
  # PCA on regressors X_std (same r_fixed chosen before with Bai–Ng)
  pca_fit <- prcomp(X_std, center = FALSE, scale. = FALSE)
  F_train <- pca_fit$x[, 1:r_fixed, drop = FALSE]
  colnames(F_train) <- paste0("F", seq_len(r_fixed))
  
  # Build dataset for the VAR: Z_t = [Y_t, F_t]
  Z_train <- cbind(Y_std, F_train)
  Z_df    <- as.data.frame(Z_train)
  
  # FVAR: VAR on targets + factors
  var_fvar <- VAR(Z_df, p = p_fix, type = "const")
  fc_fvar  <- predict(var_fvar, n.ahead = h_long)
  
  # Forecasts of Y (in standardized scale) for h=1 and h=4
  pred_h1_std <- sapply(vars_Y, function(nm) fc_fvar$fcst[[nm]][1, "fcst"])
  pred_h4_std <- sapply(vars_Y, function(nm) fc_fvar$fcst[[nm]][h_long, "fcst"])
  
  # Save standardized forecasts
  pred_h1_std_fvar_mat[k, ] <- pred_h1_std
  pred_h4_std_fvar_mat[k, ] <- pred_h4_std
  
  # De-standardization (return to the "transformed" scale as in df_now_all)
  pred_h1_real_fvar[k, ] <- pred_h1_std * Y_sd + Y_mean
  pred_h4_real_fvar[k, ] <- pred_h4_std * Y_sd + Y_mean
}

cat("\n=== FVAR forecast loop completed ===\n")

# ================================
# 9.1. FVAR – OOS metrics in transformed units
# ================================

rmse_h1_tr_fvar <- sapply(seq_len(ncol(real_h1_real)), function(j) rmse(real_h1_real[, j] - pred_h1_real_fvar[, j]))
rmse_h4_tr_fvar <- sapply(seq_len(ncol(real_h4_real)), function(j) rmse(real_h4_real[, j] - pred_h4_real_fvar[, j]))
mae_h1_tr_fvar  <- sapply(seq_len(ncol(real_h1_real)), function(j) mae(real_h1_real[, j] - pred_h1_real_fvar[, j]))
mae_h4_tr_fvar  <- sapply(seq_len(ncol(real_h4_real)), function(j) mae(real_h4_real[, j] - pred_h4_real_fvar[, j]))

metrics_transformed_fvar <- data.frame(
  variable = colnames(real_h1_real),
  RMSE_h1  = rmse_h1_tr_fvar,
  RMSE_h4  = rmse_h4_tr_fvar,
  MAE_h1   = mae_h1_tr_fvar,
  MAE_h4   = mae_h4_tr_fvar,
  row.names = NULL
)

cat("\n=== FVAR – Forecast Evaluation (Transformed Units) ===\n")
print(metrics_transformed_fvar)

# ================================
# 9.2. FVAR – Back-transform to levels
# ================================

# Pre-allocation of lists
pred_h1_level_fvar <- setNames(vector("list", length(vars_Y)), vars_Y)
pred_h4_level_fvar <- setNames(vector("list", length(vars_Y)), vars_Y)
real_h1_level_fvar <- setNames(vector("list", length(vars_Y)), vars_Y)
real_h4_level_fvar <- setNames(vector("list", length(vars_Y)), vars_Y)

for (v in vars_Y) {
  tr_v   <- get_transform(v)
  # same “base” used for the DFM
  base_h1 <- levels_df[[v]][oos_origin_idx]
  base_h4 <- levels_df[[v]][oos_origin_idx + (h_long - 1)]
  L_t1    <- levels_df[[v]][oos_origin_idx + 1]
  L_t4    <- levels_df[[v]][oos_origin_idx + h_long]
  
  # FVAR forecasts in transformed scale
  yhat_h1 <- pred_h1_real_fvar[, v]
  yhat_h4 <- pred_h4_real_fvar[, v]
  
  if (v %in% growth_vars) {
    # GDP in Δlog×100 (QoQ %), as before
    pred_h1_level_fvar[[v]] <- yhat_h1
    pred_h4_level_fvar[[v]] <- yhat_h4
    real_h1_level_fvar[[v]] <- real_h1_real[, v]
    real_h4_level_fvar[[v]] <- real_h4_real[, v]
  } else if (tr_v == "logdiff") {
    pred_h1_level_fvar[[v]] <- base_h1 * exp(yhat_h1 / 100)
    pred_h4_level_fvar[[v]] <- base_h4 * exp(yhat_h4 / 100)
    real_h1_level_fvar[[v]] <- L_t1
    real_h4_level_fvar[[v]] <- L_t4
  } else if (tr_v == "diff") {
    pred_h1_level_fvar[[v]] <- base_h1 + yhat_h1
    pred_h4_level_fvar[[v]] <- base_h4 + yhat_h4
    real_h1_level_fvar[[v]] <- L_t1
    real_h4_level_fvar[[v]] <- L_t4
  } else {
    pred_h1_level_fvar[[v]] <- yhat_h1
    pred_h4_level_fvar[[v]] <- yhat_h4
    real_h1_level_fvar[[v]] <- L_t1
    real_h4_level_fvar[[v]] <- L_t4
  }
}

pred_h1_level_fvar <- as.data.frame(pred_h1_level_fvar, check.names = FALSE)
pred_h4_level_fvar <- as.data.frame(pred_h4_level_fvar, check.names = FALSE)
real_h1_level_fvar <- as.data.frame(real_h1_level_fvar, check.names = FALSE)
real_h4_level_fvar <- as.data.frame(real_h4_level_fvar, check.names = FALSE)

# FVAR metrics in levels
rmse_h1_lvl_fvar <- sapply(vars_Y, function(v) rmse(real_h1_level_fvar[[v]] - pred_h1_level_fvar[[v]]))
rmse_h4_lvl_fvar <- sapply(vars_Y, function(v) rmse(real_h4_level_fvar[[v]] - pred_h4_level_fvar[[v]]))
mae_h1_lvl_fvar  <- sapply(vars_Y, function(v) mae(real_h1_level_fvar[[v]] - pred_h1_level_fvar[[v]]))
mae_h4_lvl_fvar  <- sapply(vars_Y, function(v) mae(real_h4_level_fvar[[v]] - pred_h4_level_fvar[[v]]))

metrics_levels_fvar <- data.frame(
  variable = vars_Y,
  RMSE_h1  = as.numeric(rmse_h1_lvl_fvar),
  RMSE_h4  = as.numeric(rmse_h4_lvl_fvar),
  MAE_h1   = as.numeric(mae_h1_lvl_fvar),
  MAE_h4   = as.numeric(mae_h4_lvl_fvar),
  row.names = NULL
)

cat("\n=== FVAR – Forecast Evaluation (Levels) ===\n")
print(metrics_levels_fvar)

# =========================
# 10. FVAR – NOW forecast (h=1..4) with Factor-Augmented VAR
# =========================

# Safety: objects that must exist from the NOW DFM section
req_now_objs <- c("df_now_all","vars_Y","vars_X","growth_vars","metadata","df")
miss_now <- req_now_objs[!vapply(req_now_objs, exists, logical(1))]
if (length(miss_now)) {
  stop("Missing objects for FVAR NOW: ", paste(miss_now, collapse = ", "))
}

# Rebuild df_now, X_all, Y_all as in NOW DFM
X_now_names <- vars_X

needed_cols <- c("date", vars_Y, X_now_names)
needed_cols <- intersect(needed_cols, names(df_now_all))
df_now <- df_now_all[, needed_cols, drop = FALSE]
df_now <- df_now[order(df_now$date), , drop = FALSE]
row.names(df_now) <- NULL

X_all <- as.matrix(df_now[, X_now_names, drop = FALSE])
Y_all <- as.matrix(df_now[, vars_Y,      drop = FALSE])

# Standardize X and Y (full history)
X_mean <- apply(X_all, 2, mean, na.rm = TRUE)
X_sd   <- apply(X_all, 2, sd,   na.rm = TRUE); X_sd[X_sd == 0] <- 1
X_std  <- scale(X_all, center = X_mean, scale = X_sd)

Y_mean <- apply(Y_all, 2, mean, na.rm = TRUE)
Y_sd   <- apply(Y_all, 2, sd,   na.rm = TRUE); Y_sd[Y_sd == 0] <- 1
Y_std  <- scale(Y_all, center = Y_mean, scale = Y_sd)

# Impute NA in standardized X (0 = mean)
X_std_imp <- X_std
for (j in seq_len(ncol(X_std_imp))) {
  X_std_imp[is.na(X_std_imp[, j]), j] <- 0
}

# Number of factors: reuse r_fixed if it exists
if (exists("r_fixed")) {
  r_now_fvar <- r_fixed
} else {
  # fallback (should not be needed in your setup)
  r_now_fvar <- min(5L, ncol(X_std_imp))
}
message("FVAR NOW: using r = ", r_now_fvar)

# PCA for NOW on the full sample
pca_now_fvar <- prcomp(X_std_imp, center = FALSE, scale. = FALSE)
r_use <- min(r_now_fvar, ncol(pca_now_fvar$x))
F_all <- pca_now_fvar$x[, 1:r_use, drop = FALSE]
colnames(F_all) <- paste0("F", 1:r_use)

# Build Z_t = [Y_std, F_all]
stopifnot(nrow(Y_std) == nrow(F_all))
Z_all <- cbind(Y_std, F_all)
Z_df  <- as.data.frame(Z_all)

# VAR on [Y, F]
p_fix <- 2
var_fvar_now <- VAR(Z_df, p = p_fix, type = "const")

# Forecast for h = 1..4
h_long <- 4
fc_fvar_now <- predict(var_fvar_now, n.ahead = h_long)

# Forecasts of Y (standardized scale)
yh1_std_fvar <- sapply(vars_Y, function(nm) fc_fvar_now$fcst[[nm]][1, "fcst"])
yh2_std_fvar <- sapply(vars_Y, function(nm) fc_fvar_now$fcst[[nm]][2, "fcst"])
yh3_std_fvar <- sapply(vars_Y, function(nm) fc_fvar_now$fcst[[nm]][3, "fcst"])
yh4_std_fvar <- sapply(vars_Y, function(nm) fc_fvar_now$fcst[[nm]][4, "fcst"])

# De-standardize to transformed scale
yh1_fvar <- yh1_std_fvar * Y_sd + Y_mean
yh2_fvar <- yh2_std_fvar * Y_sd + Y_mean
yh3_fvar <- yh3_std_fvar * Y_sd + Y_mean
yh4_fvar <- yh4_std_fvar * Y_sd + Y_mean
names(yh1_fvar) <- names(yh2_fvar) <- names(yh3_fvar) <- names(yh4_fvar) <- vars_Y

# =========================
# 10.FVAR.1 – Back-transform to levels (recursive path h=1..4)
# =========================

# # Use the same get_transform function already defined above
# get_transform <- function(var) {
#   i <- match(var, metadata$ts_key)
#   tr <- if (is.na(i)) NA_character_ else metadata$transform[i]
#   ifelse(is.na(tr), "none", tolower(tr))
# }

# Original levels of Y (untransformed) over the full sample
levels_df_fvar <- df %>%
  dplyr::mutate(date = as.Date(paste0(date, "-01"))) %>%
  dplyr::select(date, dplyr::all_of(vars_Y))

last_date_fvar <- max(df_now_all$date, na.rm = TRUE)

# Last observed level L_t for each variable
L_t_vec_fvar <- setNames(numeric(length(vars_Y)), vars_Y)
for (v in vars_Y) {
  L_t_val <- levels_df_fvar[[v]][levels_df_fvar$date == last_date_fvar]
  if (length(L_t_val) != 1 || !is.finite(L_t_val)) {
    stop("FVAR NOW back-transform: cannot find L_t for variable ", v)
  }
  L_t_vec_fvar[v] <- L_t_val
}

# Recursive propagation L_{t+h}
L_h1_fvar <- L_h2_fvar <- L_h3_fvar <- L_h4_fvar <- setNames(numeric(length(vars_Y)), vars_Y)

for (v in vars_Y) {
  tr_v <- get_transform(v)
  Lt   <- L_t_vec_fvar[v]
  
  if (v %in% growth_vars) {
    # For GDP we keep growth in Δlog*100, we do not reconstruct the level
    L1 <- yh1_fvar[v]; L2 <- yh2_fvar[v]; L3 <- yh3_fvar[v]; L4 <- yh4_fvar[v]
  } else if (tr_v == "logdiff") {
    # y = 100 * Δlog(L) -> L_{t+1} = L_t * exp(y/100)
    L1 <- Lt * exp(yh1_fvar[v] / 100)
    L2 <- L1 * exp(yh2_fvar[v] / 100)
    L3 <- L2 * exp(yh3_fvar[v] / 100)
    L4 <- L3 * exp(yh4_fvar[v] / 100)
  } else if (tr_v == "diff") {
    # y = L_t - L_{t-1} -> L_{t+1} = L_t + y
    L1 <- Lt + yh1_fvar[v]
    L2 <- L1 + yh2_fvar[v]
    L3 <- L2 + yh3_fvar[v]
    L4 <- L3 + yh4_fvar[v]
  } else {
    # No transformation: already in levels
    L1 <- yh1_fvar[v]; L2 <- yh2_fvar[v]; L3 <- yh3_fvar[v]; L4 <- yh4_fvar[v]
  }
  
  L_h1_fvar[v] <- L1
  L_h2_fvar[v] <- L2
  L_h3_fvar[v] <- L3
  L_h4_fvar[v] <- L4
}

# Target dates (same as in NOW DFM)
h1_date_fvar <- seq(last_date_fvar, by = "quarter", length.out = 2)[2]
h4_date_fvar <- seq(last_date_fvar, by = "quarter", length.out = 5)[5]

# Final NOW FVAR tables

now_fvar_transformed <- data.frame(
  variable       = vars_Y,
  target_h1_date = h1_date_fvar,
  forecast_h1    = as.numeric(yh1_fvar[vars_Y]),
  target_h4_date = h4_date_fvar,
  forecast_h4    = as.numeric(yh4_fvar[vars_Y]),
  row.names = NULL
)

now_fvar_levels <- data.frame(
  variable            = vars_Y,
  target_h1_date      = h1_date_fvar,
  forecast_h1_level   = as.numeric(L_h1_fvar[vars_Y]),
  target_h4_date      = h4_date_fvar,
  forecast_h4_level   = as.numeric(L_h4_fvar[vars_Y]),
  row.names = NULL,
  check.names = FALSE
)

cat("\n=== FVAR – NOW forecasts (transformed units) ===\n")
print(now_fvar_transformed)

cat("\n=== FVAR – NOW forecasts (levels) ===\n")
print(now_fvar_levels)

# =========================
#   11. NOW: PLOT VS KOF forecasts (DFM vs FVAR)
# =========================

last_date <- max(df_now_all$date, na.rm = TRUE)
h1_date   <- now_table_levels$target_h1_date[1]
h4_date   <- now_table_levels$target_h4_date[1]

# Recent historical series (levels)
levels_recent <- df_train %>%
  dplyr::select(date, all_of(vars_Y)) %>%
  dplyr::filter(date >= as.Date("2022-01-01")) %>%
  tidyr::pivot_longer(-date, names_to = "variable", values_to = "value")

# --- POINTS: our DFM (PCA+VAR) ---
our_pts <- bind_rows(
  now_table_levels %>%
    transmute(variable,
              date  = target_h1_date,
              value = forecast_h1_level,
              horizon = "h1",
              source  = "DFM"),
  now_table_levels %>%
    transmute(variable,
              date  = target_h4_date,
              value = forecast_h4_level,
              horizon = "h4",
              source  = "DFM")
)

# --- GDP in growth for comparison with KOF (as before) ---
df_growth <- df %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  arrange(date) %>%
  mutate(rvgdp_growth = c(NA, diff(log(rvgdp)) * 100))  # QoQ %

df_kof_growth <- df_growth %>%
  dplyr::select(date, rvgdp_growth, cpi, wkfreuro) %>%
  rename(rvgdp = rvgdp_growth)

df_kof_levels <- df_kof_growth

get_kof_value <- function(date_target, varname) {
  if (!varname %in% names(df_kof_levels)) return(NA_real_)
  out <- df_kof_levels %>%
    dplyr::filter(date == date_target) %>%
    dplyr::pull(!!rlang::sym(varname))
  if (length(out) == 0) NA_real_ else out[1]
}

# --- POINTS: KOF ---
kof_pts <- bind_rows(
  tibble(
    variable = vars_Y,
    date     = h1_date,
    value    = sapply(vars_Y, function(v) get_kof_value(h1_date, v)),
    horizon  = "h1",
    source   = "KOF"
  ),
  tibble(
    variable = vars_Y,
    date     = h4_date,
    value    = sapply(vars_Y, function(v) get_kof_value(h4_date, v)),
    horizon  = "h4",
    source   = "KOF"
  )
) %>% dplyr::filter(is.finite(value))

# --- POINTS: FVAR (NOW) ---
fvar_pts <- bind_rows(
  now_fvar_levels %>%
    transmute(variable,
              date  = target_h1_date,
              value = forecast_h1_level,
              horizon = "h1",
              source  = "FVAR"),
  now_fvar_levels %>%
    transmute(variable,
              date  = target_h4_date,
              value = forecast_h4_level,
              horizon = "h4",
              source  = "FVAR")
)

# ensure dates are Date
levels_recent$date <- as.Date(levels_recent$date)
our_pts$date       <- as.Date(our_pts$date)
kof_pts$date       <- as.Date(kof_pts$date)
fvar_pts$date      <- as.Date(fvar_pts$date)

# Palette: DFM (blue), KOF (orange), FVAR (green)
pal_colors <- c(
  "DFM"  = "#1f77b4",  # blue
  "KOF"  = "#ff7f0e",  # orange
  "FVAR" = "#2ca02c"   # green
)
pal_shapes <- c(
  "DFM"  = 16,  # filled circle
  "KOF"  = 17,  # triangle
  "FVAR" = 15   # square
)

# Panel labels
facet_labels <- c(
  "rvgdp"    = "Real GDP Growth",
  "cpi"      = "Inflation (CPI)",
  "wkfreuro" = "CHF/EUR Exchange Rate"
)

p <- ggplot() +
  geom_line(data = levels_recent,
            aes(x = date, y = value),
            linewidth = 0.6, color = "grey40") +
  geom_vline(xintercept = as.numeric(last_date),
             linetype = "solid", color = "grey50") +
  geom_point(
    data = bind_rows(our_pts, kof_pts, fvar_pts),
    aes(x = date, y = value, color = source, shape = source),
    size = 3.2
  ) +
  facet_wrap(
    ~ variable,
    scales = "free_y",
    ncol   = 1,
    labeller = as_labeller(facet_labels)
  ) +
  scale_color_manual(
    values = pal_colors,
    breaks = c("DFM", "FVAR", "KOF"),
    labels = c("Dynamic Factor Model", "FVAR", "KOF forecast"),
    name   = ""
  ) +
  scale_shape_manual(
    values = pal_shapes,
    breaks = c("DFM", "FVAR", "KOF"),
    labels = c("Dynamic Factor Model", "FVAR", "KOF forecast"),
    name   = ""
  ) +
  labs(
    title = "Forecast Comparison: Dynamic Factor Model (PCA+VAR), FVAR and KOF",
    subtitle = paste0("Historical until last observed value: ",
                      format(last_date, "%Y-%m-%d")),
    x = "Date",
    y = "Levels"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )

print(p)


# =============================
# Economic Indicator – Step 1:
# Extraction of the first factor F1 (NOW, over the full sample)
# =============================

# Safety: objects that must exist
req_ei_objs <- c("df_now_all", "vars_X")
miss_ei <- req_ei_objs[!vapply(req_ei_objs, exists, logical(1))]
if (length(miss_ei)) {
  stop("Missing objects to build the Economic Indicator: ",
       paste(miss_ei, collapse = ", "))
}

# Use the same regressors X used for DFM/FVAR
X_now_names <- vars_X

# Build a “NOW” data frame ordered by time
df_now_ei <- df_now_all %>%
  dplyr::select(date, dplyr::all_of(X_now_names)) %>%
  dplyr::arrange(date)

# Matrix X (entire available sample, already transformed)
X_all_ei <- as.matrix(df_now_ei[, X_now_names, drop = FALSE])

# Standardization as in NOW (full history)
X_mean_ei <- apply(X_all_ei, 2, mean, na.rm = TRUE)
X_sd_ei   <- apply(X_all_ei, 2, sd,   na.rm = TRUE)
X_sd_ei[X_sd_ei == 0] <- 1

X_std_ei <- scale(X_all_ei, center = X_mean_ei, scale = X_sd_ei)

# Imputation of NA: 0 = mean on standardized scale
X_std_imp_ei <- X_std_ei
for (j in seq_len(ncol(X_std_imp_ei))) {
  X_std_imp_ei[is.na(X_std_imp_ei[, j]), j] <- 0
}

# PCA on X_std_imp_ei (entire sample, NOW)
pca_ei <- prcomp(X_std_imp_ei, center = FALSE, scale. = FALSE)

# First factor F1 (raw Economic Indicator)
F1 <- pca_ei$x[, 1]

# Base data frame for the indicator
ei_raw <- data.frame(
  date = df_now_ei$date,
  F1   = as.numeric(F1)
)
# =============================
# Economic Indicator – Step 2:
# Normalization (SD=1 and SD=10 with mean=100)
# =============================

# Safety check
if (!exists("ei_raw")) stop("You must first run step 1 (ei_raw)")

# Compute mean and sd of the factor
F_mean <- mean(ei_raw$F1, na.rm = TRUE)
F_sd   <- sd(ei_raw$F1, na.rm = TRUE)

# z-score version (SD=1, mean=0)
ei_raw$EI_z <- (ei_raw$F1 - F_mean) / F_sd

# KOF-style version (mean=100, SD=10)
ei_raw$EI_kof <- 100 + 10 * (ei_raw$F1 - F_mean) / F_sd

# Final output: a single clean object
economic_indicator <- ei_raw %>%
  dplyr::select(date, EI_z, EI_kof)

ggplot(economic_indicator, aes(x = date)) +
  geom_line(aes(y = EI_z), color = "#1f77b4", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    title = "Economic Indicator (z-score version)",
    subtitle = "First principal component from PCA (mean=0, sd=1)",
    x = "Date", y = "Standardized index"
  ) +
  theme_minimal(base_size = 13)

# =============================
# 12. Economic Indicator vs Real GDP growth (QoQ, z-score)
# =============================

# Real GDP QoQ growth (rvgdp)
gdp_growth <- df %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  arrange(date) %>%
  mutate(rvgdp_growth = c(NA, diff(log(rvgdp)) * 100)) %>%
  dplyr::select(date, rvgdp_growth)

# Merge with EI_z
ei_gdp <- economic_indicator %>%
  left_join(gdp_growth, by = "date")

# Standardize GDP growth as well (for visual comparison)
mean_gdp <- mean(ei_gdp$rvgdp_growth, na.rm = TRUE)
sd_gdp   <- sd(ei_gdp$rvgdp_growth,   na.rm = TRUE)

ei_gdp <- ei_gdp %>%
  mutate(
    rvgdp_z = (rvgdp_growth - mean_gdp) / sd_gdp
  )

# Put both series in long format for ggplot
plot_data <- ei_gdp %>%
  dplyr::select(date, EI_z, rvgdp_z) %>%
  tidyr::pivot_longer(cols = c(EI_z, rvgdp_z),
                      names_to = "series",
                      values_to = "value")

series_labels <- c(
  "EI_z"    = "Economic Indicator",
  "rvgdp_z" = "Real GDP growth"
)

# Plot
p_ei_gdp <- ggplot(plot_data, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_color_manual(
    values = c("EI_z" = "#1f77b4", "rvgdp_z" = "#ff7f0e"),
    labels = series_labels,
    name   = ""
  ) +
  labs(
    title    = "Economic Indicator vs Real GDP Growth",
    subtitle = "Both series standardized (z-score)",
    x        = "Date",
    y        = "Standardized value"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top"
  )

print(p_ei_gdp)


# =============================
# Import KOF barometer (monthly) and convert to quarterly
# =============================




# Adjust these names if they differ in the file
names(kof_raw)[1:2] <- c("month_str", "kof_barometer")

kof_q <- kof_raw %>%
  # "2009-09" -> "2009-09-01" -> Date
  mutate(date = as.Date(paste0(month_str, "-01"))) %>%
  arrange(date) %>%
  # quarter date (beginning of the quarter)
  mutate(q_date = floor_date(date, "quarter")) %>%
  group_by(q_date) %>%
  # take ONLY the last month available in the quarter
  slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    date = q_date,                
    kof_barometer = kof_barometer # barometer value at end of quarter
  ) %>%
  arrange(date)

# =============================
# GDP growth QoQ (%) – quarterly
# =============================

gdp_q <- df %>%
  mutate(date = as.Date(paste0(date, "-01"))) %>%
  arrange(date) %>%
  mutate(
    gdp_growth = c(NA, diff(log(rvgdp)) * 100)   # QoQ growth in %
  ) %>%
  dplyr::select(date, gdp_growth)

# =============================
# Merge: EI (quarterly), GDP growth and KOF (quarterly)
# =============================

merged <- economic_indicator %>%
  # we have EI_z (factor z-score)
  left_join(gdp_q,   by = "date") %>%
  left_join(kof_q,   by = "date")

# =============================
# Put everything on the same scale: mean=100, sd=100
# =============================

# EI_z is already a z-score: just rescale it
merged <- merged %>%
  mutate(
    EI_100_raw = 100 + 100 * EI_z
  )

# Standardize GDP growth and KOF to z-score, then rescale to mean=100, sd=100
gdp_mean <- mean(merged$gdp_growth, na.rm = TRUE)
gdp_sd   <- sd(merged$gdp_growth,   na.rm = TRUE)

kof_mean <- mean(merged$kof_barometer, na.rm = TRUE)
kof_sd   <- sd(merged$kof_barometer,   na.rm = TRUE)

merged <- merged %>%
  mutate(
    GDP_100 = 100 + 100 * ((gdp_growth    - gdp_mean) / gdp_sd),
    KOF_100 = 100 + 100 * ((kof_barometer - kof_mean) / kof_sd),
    EI_100  = EI_100_raw
  )

# =============================
# Long format for the plot
# =============================

plot_data <- merged %>%
  dplyr::select(date, EI_100, GDP_100, KOF_100) %>%
  pivot_longer(
    cols      = c(EI_100, GDP_100, KOF_100),
    names_to  = "series",
    values_to = "value"
  )

series_labels <- c(
  "EI_100"  = "Economic Indicator",
  "GDP_100" = "Real GDP growth",
  "KOF_100" = "KOF Barometer"
)

# =============================
# Plot
# =============================

ggplot(plot_data, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey40") +
  scale_color_manual(
    values = c(
      "EI_100"  = "#1f77b4",  # blue
      "GDP_100" = "#ff7f0e",  # orange
      "KOF_100" = "#2ca02c"   # green
    ),
    labels = series_labels,
    name   = ""
  ) +
  labs(
    title    = "Economic Indicator, GDP Growth and KOF Barometer",
    subtitle = "All series standardized to mean = 100, sd = 100 (quarterly)",
    x        = "Date",
    y        = "Index (mean=100, sd=100)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top"
  )





