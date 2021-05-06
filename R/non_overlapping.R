#' The (non overlapping) Global Minimum Variance (GMV) portfolio using a dynamic optimal shrinkage scheme.
#'
#' This function implements the recursive estimation of the GMV portfolio in the dynamic optimal shrinkage setting.
#' The method demands that the data is on long format, observations are on rows and covariates are columns and that
#' the number of rows are greater than the number of columns. This function estimates each GMV portfolio
#' using the most recent data, e.g. from the last break point to the current. To see a more detailed description of the method, see
#' vigenette \code{vignette("DOSPortfolio", package = "DOSPortfolio")}.
#'
#' @param data a matrix of size (n x p), where n>p, containing, for instance, log-returns.
#' @param break_points a vector of break points. The breakpoints are what determines
#' when we recompute weights.
#' @param target_w a vector which is the target weights that one wants to shrink to in the first period.
#' @param r a numeric of the initial value of the relative loss for the variance of the GMV portfolio.
#'
#' @export
#'
#' @return vector of shrunk GMV portfolio weights
#'
#' @examples
#' n <- 200*2
#' p <- 80
#' c <- p/n
#' break_points <- c(199)
#' data <- matrix(rt(n*p, df=5), ncol=p, nrow=n)
#' target_w <- as.vector(rep(1,p))/p
#' wGMVNonOverlapping(data, break_points, target_w, 1)
#'
wGMVNonOverlapping <- function(data, break_points, target_w, r) {
  p <- ncol(data)
  if (!((break_points[1] == 1) && (length(break_points) > 2))) {
    break_points <- c(1,break_points)
  }
  for (idx in 2:length(break_points)) {
    data_subsample <- data[(break_points[idx-1]):break_points[idx],]
    c <- p/nrow(data_subsample)
    S <- stats::var(data_subsample)
    S_chol_inv <- t(solve(chol(S)))
    if (idx - 1 == 1) {
      old_weights <- target_w
    }else{
      r <- Rupdate(xi, c, r_prev = r)
      old_weights <- w_gmv_new
    }
    xi <- ShrinkageCoefNonOverlapping(c, r)
    # use the new info to estimate r recursively
    w_gmv_new <- ConvexCombination(
      wGMV(S_chol_inv %*% t(S_chol_inv)),
      old_weights,
      xi)
  }
  structure(w_gmv_new, class="DOSPortfolio")
}


#' Computes the shrinkage coefficient for non-overlapping samples.
#'
#' @param r_prev numeric
#' @param c numeric
#'
#' @return numeric
#'
ShrinkageCoefNonOverlapping <- function(r_prev, c) {
  (1 - c) * r_prev / ((1 - c) * r_prev + c)
}

#' Function which updates values of r0.
#'
#' @param xi numeric in range of (0,1)
#' @param c numeric in range of (0,1)
#' @param r_prev numeric in range of (0,1)
#'
#' @return numeric
#'
Rupdate <- function(xi, c, r_prev) {
  xi^2 * c / (1 - c) + (1 - xi)^2 * r_prev
}
