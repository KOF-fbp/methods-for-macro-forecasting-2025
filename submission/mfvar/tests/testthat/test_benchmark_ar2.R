test_that("AR(2) fit and forecast work correctly", {
  
  set.seed(123)
  
  # Generate AR(2) process
  n <- 100
  phi1 <- 0.5
  phi2 <- 0.3
  const <- 1.0
  
  series <- numeric(n)
  series[1] <- const
  series[2] <- const + phi1 * series[1] + rnorm(1, 0, 0.1)
  
  for (i in 3:n) {
    series[i] <- const + phi1 * series[i-1] + phi2 * series[i-2] + rnorm(1, 0, 0.1)
  }
  
  # Fit AR(2)
  fit <- fit_ar2(series, origin_index = n)
  
  expect_length(fit$coef, 3)
  expect_true(is.numeric(fit$sigma2))
  expect_true(fit$sigma2 > 0)
  
  # Forecast
  h <- 3
  y_last <- series[(n-1):n]
  forecast_val <- forecast_ar2(fit, h, y_last)
  
  expect_true(is.numeric(forecast_val))
  expect_length(forecast_val, 1)
})

test_that("evaluate_forecasts computes RMSE and MAE", {
  
  forecasts_mfvar <- data.table::data.table(
    target = rep("GDP_growth", 2),
    horizon = c(1, 12),
    value = c(2.0, 2.5)
  )
  
  forecasts_ar2 <- data.table::data.table(
    target = rep("GDP_growth", 2),
    horizon = c(1, 12),
    value = c(1.8, 2.3)
  )
  
  actuals <- data.table::data.table(
    target = rep("GDP_growth", 2),
    horizon = c(1, 12),
    actual = c(2.1, 2.4)
  )
  
  eval_table <- evaluate_forecasts(forecasts_mfvar, forecasts_ar2, actuals)
  
  expect_true("RMSE" %in% names(eval_table))
  expect_true("MAE" %in% names(eval_table))
  expect_equal(nrow(eval_table), 4)  # 2 models x 2 horizons
})
