test_that("non overlapping estimator, weights matrix ", {
  n <- 25
  p <- 10
  Sigma <- generate_sigma(p)
  data <- matrix(rt(n*p, df=5), ncol=p, nrow=n) %*% t(chol(Sigma))

  target_portfolio <- OnesVec(p)/p
  reallocation_points <- c(12)
  relative_loss <- 0

  weights <- wGMVOverlapping(data, reallocation_points, target_portfolio, relative_loss)
  expect_equal(nrow(weights), length(reallocation_points))
})

test_that("non overlapping estimator, weights matrix rows are portfolios", {
  n <- 25
  p <- 10
  Sigma <- generate_sigma(p)
  data <- matrix(rt(n*p, df=5), ncol=p, nrow=n) %*% t(chol(Sigma))
  target_portfolio <- OnesVec(p)/p
  weights <- wGMVOverlapping(data,
                             reallocation_points=c(12, 16, 20),
                             target_portfolio,
                             relative_loss=1)
  expect_equal(rowSums(weights), rep(1, length(reallocation_points)))
})

test_that("non overlapping estimator, weights matrix rows are portfolios", {
  n <- 25
  p <- 10
  Sigma <- generate_sigma(p)
  data <- matrix(rt(n*p, df=5), ncol=p, nrow=n) %*% t(chol(Sigma))

  target_portfolio <- OnesVec(p)/p
  reallocation_points <-
    relative_loss <- 0

  weights <- wGMVOverlapping(data,
                             reallocation_points=c(12),
                             target_portfolio,
                             relative_loss=0)
  expect_equal(as.vector(weights), target_portfolio)
})
