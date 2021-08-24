#' Dynamic optimal shrinkage estimator of the weights of the global minimum
#' variance portfolio when non-overlapping samples are used.
#'
#' The function implements the dynamic shrinkage estimator of the weights of the
#' global minimum-variance portfolio when the overlapping samples are used as
#' given in Eq. (2.11) of \insertCite{BODNAR21dynshrink;textual}{DOSPortfolio} .
#'
#' @param data an n by p matrix of asset returns. Columns represent different
#' assets rows are observations, where n>p, containing, for instance, log-returns.
#' @param reallocation_points a vector of reallocation points. The reallocation
#' points determine when the holding portfolio should be reconstructed and its
#' weights should be recomputed.
#' @param target_portfolio a vector which determines the weights of the target
#' portfolio used when the shrinkage estimator of the global minimum variance
#' portfolio is constructed for the first time.
#' @param relative_loss possibly a numeric or NULL. The initial value of the
#' relative loss in the variance of the target portfolio. If its NULL, then it
#' will be initialized with the first subsample and the function
#' \code{\link{r0Strategy}}.
#'
#' @return a matrix of the constructed weights at each reallocation point of the
#' dynamic shrinkage estimator of the global minimum variance portfolio when
#' non-overlapping samples are used.
#' @seealso section 2.1 \insertCite{BODNAR21dynshrink}{DOSPortfolio}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' n <- 200*2
#' p <- 80
#' reallocation_point <- c(199)
#' data <- matrix(rt(n*p, df=5), ncol=p, nrow=n)
#' target_portfolio <- as.vector(rep(1,p))/p
#' wGMVNonOverlapping(data, reallocation_point, target_portfolio, 1)
#' @export
wGMVNonOverlapping <- function(data, reallocation_points, target_portfolio, relative_loss) {
  p <- ncol(data)
  weights_matrix <- matrix(ncol=p, nrow=length(reallocation_points))
  for (idx in 1:length(reallocation_points)) {
    if (idx == 1) {
      data_subsample <- data[1:reallocation_points[idx],]
    }else{
      data_subsample <- data[(reallocation_points[idx-1]):reallocation_points[idx],]
    }
    c <- p/nrow(data_subsample)
    S <- stats::var(data_subsample)
    S_chol_inv <- t(solve(chol(S)))
    if (idx == 1) {
      old_weights <- target_portfolio
    }else{
      # use the new info to estimate r recursively
      relative_loss <- xi^2 * c / (1 - c) + (1 - xi)^2 * relative_loss
      old_weights <- w_gmv_new
    }
    # Compute the shrinkage coefficient
    xi <- (1 - c) * relative_loss / ((1 - c) * relative_loss + c)
    w_gmv_new <- ConvexCombination(
      wGMV(S_chol_inv %*% t(S_chol_inv)),
      old_weights,
      xi)
    weights_matrix[idx,] <- w_gmv_new
  }
  weights_matrix
}
