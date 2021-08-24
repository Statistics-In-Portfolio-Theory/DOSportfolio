test_that("non overlapping estimator, weights matrix ", {
  n <- 25
  p <- 10
  data <- 5/3 * matrix(rt(n*p, df=5), ncol=p, nrow=n)

  target_portfolio <- rep(1,p)/p
  reallocation_points <- c(12)
  relative_loss <- 0

  weights <- wGMVNonOverlapping(data,reallocation_points, target_portfolio, relative_loss)
  expect_equal(nrow(weights), length(reallocation_points))
})

test_that("non overlapping estimator, weights matrix rows are portfolios", {
  n <- 250
  p <- 10
  data <- 5/3 * matrix(rt(n*p, df=5), ncol=p, nrow=n)

  target_portfolio <- rep(1,p)/p
  reallocation_points <- c(12, 100, 150)
  relative_loss <- 1.22

  weights <- wGMVNonOverlapping(data,
                                reallocation_points,
                                target_portfolio,
                                relative_loss)
  expect_equal(rowSums(weights), rep(1, length(reallocation_points)))
})


test_that("non overlapping estimator, perfect guess of target portfolio", {
  n <- 25
  p <- 10
  data <- 5/3 * matrix(rt(n*p, df=5), ncol=p, nrow=n)

  target_portfolio <- rep(1,p)/p
  weights <- wGMVNonOverlapping(data,
                             reallocation_points=c(12),
                             target_portfolio=target_portfolio,
                             relative_loss=0)
  expect_equal(as.vector(weights), target_portfolio)
})
