#' The (non overlapping) Global Minimum Variance (GMV) portfolio using a dynamic optimal shrinkage scheme.
#'
#' This function implements the recursive estimation of the GMV portfolio in the dynamic optimal shrinkage setting.
#' The method demands that the data is on long format, observations are on rows and covariates are columns and that
#' the number of rows are greater than the number of columns. This function estimates each GMV portfolio
#' using the most recent data, e.g. from the last break point to the current. To see a more detailed description of the method, see
#' vignette \code{vignette("introduction", package = "DOSPortfolio")}.
#'
#' @param data a matrix of size (n x p), where n>p, containing, for instance, log-returns.
#' @param change_points a vector of break points. The breakpoints are what determines
#' when we recompute weights.
#' @param target_w a vector which is the target weights that one wants to shrink to in the first period.
#' @param relative_loss a numeric of the initial value of the relative loss for the variance of the GMV portfolio.
#'
#' @return vector of shrunk GMV portfolio weights
#' @export
#'
#' @examples
#' n <- 200*2
#' p <- 80
#' change_point <- c(199)
#' data <- matrix(rt(n*p, df=5), ncol=p, nrow=n)
#' target_w <- as.vector(rep(1,p))/p
#' wGMVNonOverlapping(data, change_point, target_w, 1)
#'
wGMVNonOverlapping <- function(data, change_points, target_w, relative_loss) {
  p <- ncol(data)
  if (!((change_points[1] == 1) && (length(change_points) > 2))) {
    change_points <- c(1,change_points)
  }
  for (idx in 2:length(change_points)) {
    data_subsample <- data[(change_points[idx-1]):change_points[idx],]
    c <- p/nrow(data_subsample)
    S <- stats::var(data_subsample)
    S_chol_inv <- t(solve(chol(S)))
    if (idx - 1 == 1) {
      old_weights <- target_w
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
  }
  w_gmv_new
}
