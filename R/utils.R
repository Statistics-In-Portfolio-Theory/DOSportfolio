#' Creates a vector of length p which has ones as elements.
#'
#' @param p integer
#'
#' @return vector
#' @export
OnesVec <- function(p) as.vector(rep(1, p))

#' The Global Minimum Variance (GMV) portfolio weights. This function
#' computes the global minimum portfolio weights which is part of Modern
#' Portfolio Theory, Markowitz (1952). For a positive semi-definite matrix
#' S, the weights are obtained as the solution to a quadratic optimization
#' problem given that the solution sums to one.
#'
#' @param S_inv the inverse of a sample covariance matrix
#'
#' @return a vector, which is the Global Minimum Variance Portfolio.
wGMV <- function(S_inv) {S_inv%*%OnesVec(nrow(S_inv)) / as.numeric(sum(S_inv))}

#' Computes a convex combination between two vectors.
#'
#' @param lambda a numeric value between 0 and 1.
#' @param x1 a vector.
#' @param x2 a vector.
#'
#' @return a vector
ConvexCombination <- function(x1, x2, lambda) {
  if (length(lambda) > 1) {
    stop("Parameter lambda is a numeric, not a vector of length greater than 2.", call. = FALSE)
  }
  lambda*x1 + (1-lambda)*x2
}

#' Simple plugin estimator for the relative loss. This function computes a
#' simple plugin estimator for the relative loss to the variance of the GMV portfolio
#' and the target portfolio.
#'
#' @param S_inv the inverse of a sample covariance matrix.
#' @param S a sample covariance matrix.
#' @param target_w a vector representing a target portfolio which is shrunk to.
#' @param c a numeric which is the concentration ratio.
#'
#' @return vector
#' @export
r0Strategy <- function(S_inv, S, target_w, c){
  (1-c)*as.numeric(t(OnesVec(nrow(S_inv))) %*% S_inv %*% OnesVec(nrow(S_inv)))*as.numeric(t(target_w) %*% S %*% target_w)-1
}
