test_that("prepare_monthly_grid handles toy quarterly and monthly series", {
  
  # Create toy data
  dates <- zoo::as.yearmon("2020-01") + (0:11)/12
  
  df <- data.table::data.table(
    date = dates,
    monthly_var = 1:12,
    quarterly_var = c(10, NA, NA, 20, NA, NA, 30, NA, NA, 40, NA, NA)
  )
  
  meta <- list(
    vars = c("monthly_var", "quarterly_var"),
    freq = list(monthly_var = "monthly", quarterly_var = "quarterly")
  )
  
  result <- prepare_monthly_grid(df, meta, log_vars = NULL, standardize = FALSE)
  
  expect_equal(nrow(result$data), 12)
  expect_true(all(!is.na(result$data$monthly_var)))
  
  # Quarterly should have values only at quarter ends (months 3, 6, 9, 12)
  qtr_ends <- c(3, 6, 9, 12)
  expect_true(all(!is.na(result$data$quarterly_var[qtr_ends])))
  expect_true(all(is.na(result$data$quarterly_var[-qtr_ends])))
})

test_that("prepare_monthly_grid applies log transformation", {
  
  dates <- zoo::as.yearmon("2020-01") + (0:5)/12
  
  df <- data.table::data.table(
    date = dates,
    positive_var = c(100, 110, 120, 130, 140, 150)
  )
  
  meta <- list(
    vars = "positive_var",
    freq = list(positive_var = "monthly")
  )
  
  result <- prepare_monthly_grid(df, meta, log_vars = "positive_var", standardize = FALSE)
  
  expect_true("positive_var" %in% result$transforms$logs)
  expect_equal(result$data$positive_var[1], log(100))
})
