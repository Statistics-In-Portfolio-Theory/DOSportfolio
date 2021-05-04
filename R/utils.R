#' Creates a vector of length p with ones
#'
#' @param p integer
#'
#' @return vector
#' @export
ones_vec <- function(p) as.vector(rep(1, p))

#' Computes the Global Minimum Variance (GMV) portfolio weights.
#'
#' @param S_inv the inverse of a sample covariance matrix
#'
#' @return vector
w_gmv <- function(S_inv) {S_inv%*%ones_vec(nrow(S_inv)) / as.numeric(sum(S_inv))}

#' Computes a convex combination between two
#'
#' @param lambda a numeric value between 0 and 1.
#' @param x1 a vector.
#' @param x2 a vector.
#'
#' @return vector
#' @export
convex_combination <- function(x1, x2, lambda) {
  if (length(lambda) > 1) {
    stop("Parameter lambda is a numeric, not a vector of length greater than 2.", call. = FALSE)
  }
  lambda*x1 + (1-lambda)*x2
}

#' Computes a simple plugin estimator for the computation of relative loss to the variance of the GMV portfolio
#'
#' @param S_inv the inverse of a sample covariance matrix.
#' @param S a sample covariance matrix.
#' @param target_w a vector representing the target portfolio.
#' @param c a numeric which is the concentration ratio.
#'
#' @return vector
#' @export
r0_strategy <- function(S_inv, S, target_w, c){
  (1-c)*as.numeric(t(ones_vec(nrow(S_inv))) %*% S_inv %*% ones_vec(nrow(S_inv)))*as.numeric(t(target_w) %*% S %*% target_w)-1
}
