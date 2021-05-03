#' Creates a vector of length p with ones
#'
#' @param p integer
#'
#' @return vector
ones_vec <- function(p) as.vector(rep(1, p))

#' Computes the Global Minimum Variance (GMV) portfolio weights.
#'
#' @param r_prev numeric
#' @param c numeric
#'
#' @return vector
w_gmv <- function(S_inv) S_inv%*%ones_vec(nrow(S_inv)) / as.numeric(sum(S_inv))

#' Computes a convex combination between two
#'
#' @param lambda numeric
#' @param x1 vector
#' @param x2 vector
#'
#' @return vector
convex_combination <- function(x1, x2, lambda) {
  if (length(lambda) > 1) {
    stop("Parameter lambda is a numeric, not a vector of length greater than 2.", call. = FALSE)
  }
  lambda*x1 + (1-lambda)*x2
}

#' Computes a simple plugin init for the computation of
#'
#' @param lambda numeric
#' @param x1 vector
#' @param x2 vector
#'
#' @return vector
r0_strategy <- function(S_inv, S, b, c){
  (1-c)*as.numeric(t(ones_vec(nrow(S_inv))) %*% S_inv %*% ones_vec(nrow(S_inv)))*as.numeric(t(b) %*% S %*% b)-1
}
