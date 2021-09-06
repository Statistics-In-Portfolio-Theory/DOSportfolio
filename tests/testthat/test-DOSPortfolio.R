test_that("interface of DOSPortfolio", {
  # test the class name
  p <- 15
  n <- 50
  df <- 5
  data <- ((df-2)/df)*matrix(stats::rt(n*p, df=df), ncol=p, nrow=n)
  reallocation_points <- c(25, 42)
  ret <- DOSPortfolio(data, reallocation_points)
  expect_equal(class(ret), "DOSPortfolio")
})

test_that("Constructor - non-correct argument", {
  # Check that errors are thrown on wrong argument for shrinkage.
  p <- 20
  df <- 5
  n <- 50
  Sigma <- HDShOP::RandCovMtrx(p)
  data <- (df/(df-2))*matrix(stats::rt(n*p, df=df), ncol=p, nrow=n) %*%
    t(chol(Sigma))
  reallocation_points <- c(25, 42, 50)
  expect_error(new_DOSPortfolio(data,
                                reallocation_points,
                                target_portfolio = rep(1,p),
                                relative_loss=0,
                                shrinkage_type = "test"))
})

test_that("Non-overlapping est. (validate_input) - data error", {
  p <- 35
  df <- 5
  n <- 20
  Sigma <- HDShOP::RandCovMtrx(p)
  data <- (df/(df-2))*matrix(stats::rt(n*p, df=df), ncol=p, nrow=n) %*%
    t(chol(Sigma))
  reallocation_points <- c(10)
  expect_error(validate_input(data,
                              reallocation_points,
                              target_portfolio = rep(1,p)/p,
                              relative_loss=1,
                              shrinkage_type = "non-overlapping"))
})

test_that("Non-overlapping est. (validate_input) - reallocation error", {
  p <- 10
  n <- 20
  df <- 5
  Sigma <- HDShOP::RandCovMtrx(p)
  data <- (df/(df-2))*matrix(stats::rt(n*p, df=df), ncol=p, nrow=n) %*%
    t(chol(Sigma))
  reallocation_points <- c(22)
  expect_error(validate_input(data,
                              reallocation_points,
                              target_portfolio = rep(1,p)/p,
                              relative_loss=1,
                              shrinkage_type = "non-overlapping"))
})

test_that("Non-overlapping est. (validate_input) - reallocation test, first reallocation", {
  p <- 10
  n <- 17
  df <- 5
  Sigma <- HDShOP::RandCovMtrx(p)
  data <- (df/(df-2))*matrix(stats::rt(n*p, df=df), ncol=p, nrow=n) %*%
    t(chol(Sigma))
  reallocation_points <- c(1, 18)
  expect_null(validate_input(data,
                              reallocation_points,
                              target_portfolio = rep(1,p)/p,
                              relative_loss=1,
                              shrinkage_type = "non-overlapping"))
})

test_that("Non-overlapping est. (validate_input) - c_2 > 1", {
  p <- 20
  n <- 50
  df <- 5
  Sigma <- HDShOP::RandCovMtrx(p)
  data <- (df/(df-2))*matrix(stats::rt(n*p, df=df), ncol=p, nrow=n) %*%
    t(chol(Sigma))
  reallocation_points <- c(25, 42, 50)
  expect_error(validate_input(data,
                              reallocation_points,
                              target_portfolio = rep(1,p)/p,
                              relative_loss=1,
                              shrinkage_type = "non-overlapping"))
})

test_that("Overlapping est. (validate_input) - c_3 > 1", {
  p <- 10
  n <- 50
  df <- 5
  Sigma <- HDShOP::RandCovMtrx(p)
  data <- (df/(df-2))*matrix(stats::rt(n*p, df=df), ncol=p, nrow=n) %*%
    t(chol(Sigma))
  reallocation_points <- c(25, 42, 50)
  expect_null(validate_input(data,
                             reallocation_points,
                             target_portfolio = rep(1,p)/p,
                             relative_loss=1,
                             shrinkage_type = "overlapping"))
})

test_that("Overlapping est. (validate_input) - target portfolio", {
  p <- 20
  n <- 50
  df <- 5
  Sigma <- HDShOP::RandCovMtrx(p)
  data <- (df/(df-2))*matrix(stats::rt(n*p, df=df), ncol=p, nrow=n) %*%
    t(chol(Sigma))
  reallocation_points <- c(25, 42, 50)
  expect_error(validate_input(data,
                              reallocation_points,
                              target_portfolio = rep(1,p),
                              relative_loss=1,
                              shrinkage_type = "overlapping"))
})

test_that("Overlapping est. (validate_input) - relative loss", {
  p <- 20
  n <- 50
  df <- 5
  Sigma <- HDShOP::RandCovMtrx(p)
  data <- (df/(df-2))*matrix(stats::rt(n*p, df=df), ncol=p, nrow=n) %*%
    t(chol(Sigma))
  reallocation_points <- c(25, 42, 50)
  expect_error(validate_input(data,
                              reallocation_points,
                              target_portfolio = rep(1,p),
                              relative_loss=-20,
                              shrinkage_type = "overlapping"))
})

