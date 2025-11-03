test_that("companion_blocks creates correct companion form", {
  
  set.seed(123)
  
  n <- 2
  p <- 2
  
  A1 <- matrix(runif(4), 2, 2)
  A2 <- matrix(runif(4), 2, 2)
  Sigma <- diag(c(1, 1.5))
  
  comp <- companion_blocks(list(A1, A2), Sigma)
  
  # Check dimensions
  expect_equal(dim(comp$Tmat), c(4, 4))
  expect_equal(dim(comp$Rmat), c(4, 2))
  expect_equal(dim(comp$Qmat), c(2, 2))
  
  # Check top block of T
  expect_equal(comp$Tmat[1:2, 1:2], A1)
  expect_equal(comp$Tmat[1:2, 3:4], A2)
  
  # Check identity block
  expect_equal(comp$Tmat[3:4, 1:2], diag(2))
  
  # Check R selection matrix
  expect_equal(comp$Rmat[1:2, ], diag(2))
  expect_equal(comp$Rmat[3:4, ], matrix(0, 2, 2))
})
