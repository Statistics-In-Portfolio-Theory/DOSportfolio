#' Sample estimator of the weights of the global minimum variance portfolio
#'
#' The functions computes the sample estimate of the weights of the global
#' minimum variance portfolio (see, e.g., Eq. (1.4) of
#' \insertCite{BODNAR21dynshrink;textual}{DOSPortfolio})).
#'
#' @param S_inv the inverse of a sample covariance matrix
#'
#' @return a vector, which is the Global Minimum Variance Portfolio.
wGMV <- function(S_inv) {S_inv%*%rep(1, nrow(S_inv)) / as.numeric(sum(S_inv))}

#' Computes a convex combination between two vectors.
#'
#' @param lambda a numeric value between 0 and 1.
#' @param x1 a vector.
#' @param x2 a vector.
#'
#' @keywords internal
#'
#' @return a vector
ConvexCombination <- function(x1, x2, lambda) {
  if (length(lambda) > 1) {
    stop("Parameter lambda is a numeric, not a vector of length greater than 2.",
         call. = FALSE)
  }
  lambda*x1 + (1-lambda)*x2
}

#' Computes the relative loss of the target portfolio used
#'
#' The function computes the initial value of the relative loss in the variance
#' of the target portfolio as given in Eq. (2.10) of
#' \insertCite{BODNAR21dynshrink;textual}{DOSPortfolio}.
#'
#' @references
#'  \insertAllCited{}
#'
#' @param S_inv the inverse of a sample covariance matrix.
#' @param S a sample covariance matrix.
#' @param target_portfolio a vector which determines the weights of the target
#' portfolio used when the shrinkage estimator of the global minimum variance
#' portfolio is constructed for the first time.
#' @param c a numeric which is the concentration ratio.
#'
#' @return vector
#' @export
r0Strategy <- function(S_inv, S, target_portfolio, c){
  (1-c)*(
    as.numeric(t(rep(1, nrow(S_inv))) %*% S_inv %*% rep(1, nrow(S_inv))) *
      as.numeric(t(target_portfolio) %*% S %*% target_portfolio)
    ) -1
}
