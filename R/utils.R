#' Sample estimator of the weights of the global minimum variance portfolio
#'
#' The functions computes the sample estimate of the weights of the global
#' minimum variance portfolio (see, e.g., Eq. (1.4) of
#' \insertCite{BODNAR21dynshrink;textual}{DOSPortfolio})).
#'
#' @param data an n by p matrix of asset returns. Columns represent different
#' assets rows are observations, where n>p, containing, for instance, log-returns.
#'
#' @return a vector, which is the Global Minimum Variance Portfolio.
#'
#' @examples
#'
#' n <- 200
#' p <- 80
#' data <- 3/5 * matrix(rt(n*p, df=5), ncol=p, nrow=n)
#' weights <- wGMV(data)
#' # since the covariance matrix is the identity-matrix the estimated weights
#' # should be close to the equally weighted portfolio.
#' mean(abs(wGMV(data) - 1/p))
#'
#' @export
wGMV <- function(data) {
  S <- var(data)
  S_inv <- solve(S)
  S_inv%*%rep(1, nrow(S_inv)) / as.numeric(sum(S_inv))
}



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
#' @param data an n by p matrix of asset returns. Columns represent different
#' assets rows are observations, where n>p, containing, for instance, log-returns.
#' @param target_portfolio a vector which determines the weights of the target
#' portfolio used when the shrinkage estimator of the global minimum variance
#' portfolio is constructed for the first time.
#' @param c a numeric which is the concentration ratio.
#'
#' @return vector
#'
#' @examples
#'
#' n <- 200*2
#' p <- 80
#' data <- 5/3 * matrix(rt(n*p, df=5), ncol=p, nrow=n)
#' # set a target portfolio, such as equally weighted
#' b <- rep(1,p)/p
#' r0Strategy(data, b, p/n)
#'
#' @export
r0Strategy <- function(data, target_portfolio, c){
  S <- var(data)
  S_inv <- solve(S)
  (1-c)*(
    as.numeric(t(rep(1, nrow(S_inv))) %*% S_inv %*% rep(1, nrow(S_inv))) *
      as.numeric(t(target_portfolio) %*% S %*% target_portfolio)
    ) -1
}
