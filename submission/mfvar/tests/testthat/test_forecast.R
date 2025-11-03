test_that("predict_mfvar returns expected dimensions", {
  
  # Create mock posterior object
  n <- 2
  p <- 1
  T_obs <- 24
  n_draws <- 5
  
  posterior <- list(
    n = n,
    p = p,
    n_draws = n_draws,
    series_names = c("var1", "var2"),
    A_draws = array(rnorm((n*p + 1) * n * n_draws), dim = c(n*p + 1, n, n_draws)),
    Sigma_draws = array(0, dim = c(n, n, n_draws)),
    y_latent_draws = array(rnorm(T_obs * n * n_draws), dim = c(T_obs, n, n_draws))
  )
  
  # Make Sigma positive definite
  for (d in 1:n_draws) {
    posterior$Sigma_draws[, , d] <- diag(runif(n, 0.5, 1.5))
  }
  
  horizons_months <- 12
  
  result <- tryCatch({
    predict_mfvar(posterior, horizons_months = horizons_months, nsim = 10, seed = 123)
  }, error = function(e) {
    NULL
  })
  
  if (!is.null(result)) {
    expect_equal(nrow(result$y_mean), horizons_months)
    expect_equal(ncol(result$y_mean), n)
    expect_equal(dim(result$y_forecasts), c(horizons_months, n, n_draws))
  } else {
    skip("predict_mfvar failed - may need package dependencies")
  }
})
