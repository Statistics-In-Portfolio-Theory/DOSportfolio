#' The (non overlapping) Global Minimum Variance (GMV) portfolio using a dynamic optimal shrinkage scheme.
#'
#' This function implements the recursive estimation of the GMV portfolio in the dynamic optimal shrinkage setting.
#' The method demands that the data is on long format, observations are on rows and covariates are columns and that
#' the number of rows are greater than the number of columns. This function estimates each GMV portfolio
#' using the most recent data, e.g. from the last break point to the current.
#'
#' @param data a matrix of size (n x p), where n>p, containing a sample of p-dimensional vectors of asset returns.
#' @param reallocation_points a vector of reallocationpoints. The reallocationpoints are what determines
#' when we recompute weights.
#' @param target_portfolio a vector which is the target weights that one wants to shrink to in the first period.
#' @param relative_loss a numeric of the initial value of the relative loss for the variance of the GMV portfolio.
#'
#' @return a matrix of shrunk GMV portfolio weights where each row corresponds to each reallocation point.
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
