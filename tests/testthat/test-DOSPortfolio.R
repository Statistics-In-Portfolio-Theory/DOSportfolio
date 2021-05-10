generate_data <- function(n,p) {
  Sigma <- generate_sigma(p)
  matrix(rt(n*p, df=5), ncol=p, nrow=n) %*% t(chol(Sigma))
}

test_that("interface of DOSPortfolio", {
  # test the class name
  data <- generate_data(n=50, p=15)
  reallocation_points <- c(25, 42)
  ret <- DOSPortfolio(data, reallocation_points)
  expect_equal(class(ret), "DOSPortfolio")
})

test_that("Constructor - non-correct argument", {
  # Check that errors are thrown on wrong argument for shrinkage.
  p <- 20
  data <- generate_data(n=50, p=p)
  reallocation_points <- c(25, 42, 50)
  expect_error(new_DOSPortfolio(data,
                                reallocation_points,
                                target_portfolio = OnesVec(p),
                                relative_loss=0,
                                shrinkage_type = "test"))
})

test_that("Non-overlapping est. (validate_input) - data error", {
  p <- 35
  data <- generate_data(n=10, p=p)
  reallocation_points <- c(10)
  expect_error(validate_input(data,
                              reallocation_points,
                              target_portfolio = OnesVec(p)/p,
                              relative_loss=1,
                              shrinkage_type = "non-overlapping"))
})

test_that("Non-overlapping est. (validate_input) - reallocation error", {
  p <- 10
  data <- generate_data(n=20, p=p)
  reallocation_points <- c(22)
  expect_error(validate_input(data,
                              reallocation_points,
                              target_portfolio = OnesVec(p)/p,
                              relative_loss=1,
                              shrinkage_type = "non-overlapping"))
})

test_that("Non-overlapping est. (validate_input) - reallocation test, first reallocation", {
  p <- 10
  data <- generate_data(n=20, p=p)
  reallocation_points <- c(1, 18)
  expect_null(validate_input(data,
                              reallocation_points,
                              target_portfolio = OnesVec(p)/p,
                              relative_loss=1,
                              shrinkage_type = "non-overlapping"))
})

test_that("Non-overlapping est. (validate_input) - c_2 > 1", {
  p <- 20
  data <- generate_data(n=50, p=p)
  reallocation_points <- c(25, 42, 50)
  expect_error(validate_input(data,
                              reallocation_points,
                              target_portfolio = OnesVec(p)/p,
                              relative_loss=1,
                              shrinkage_type = "non-overlapping"))
})

test_that("Overlapping est. (validate_input) - c_3 > 1", {
  p <- 20
  data <- generate_data(n=50, p=p)
  reallocation_points <- c(25, 42, 50)
  expect_null(validate_input(data,
                             reallocation_points,
                             target_portfolio = OnesVec(p)/p,
                             relative_loss=1,
                             shrinkage_type = "overlapping"))
})

test_that("Overlapping est. (validate_input) - target portfolio", {
  p <- 20
  data <- generate_data(n=50, p=p)
  reallocation_points <- c(25, 42, 50)
  expect_error(validate_input(data,
                              reallocation_points,
                              target_portfolio = OnesVec(p),
                              relative_loss=1,
                              shrinkage_type = "overlapping"))
})

test_that("Overlapping est. (validate_input) - relative loss", {
  p <- 20
  data <- generate_data(n=50, p=p)
  reallocation_points <- c(25, 42, 50)
  expect_error(validate_input(data,
                              reallocation_points,
                              target_portfolio = OnesVec(p),
                              relative_loss=-20,
                              shrinkage_type = "overlapping"))
})
