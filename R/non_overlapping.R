#' Computes the GMV portfolio using the optimal weighting scheme where each weight is based of non-overlapping samples.
#'
#' @param break_points the number and time points (row number) of break points
#' @param target_w vector with the target portfolio
#' @param data data.frame on long format. Contains log returns.
#'
#' @export
#'
#' @return vector
w_non_overlapping <- function(data, break_points, target_w, r) {
  # TODO: make sure that data has correct format/types in cols.
  stopifnot(sum(target_w) == 1)
  p <- ncol(data)
  break_points <- c(1,break_points)
  for (idx in 2:length(break_points)) {
    data_subsample <- data[(break_points[idx-1]):break_points[idx],]
    c <- p/nrow(data_subsample)
    S <- stats::var(data_subsample)
    S_chol_inv <- t(solve(chol(S)))
    if (idx - 1 == 1) {
      old_weights <- target_w
    }else{
      r <- r_update(xi, c, r_prev = r)
      old_weights <- w_gmv_new
    }
    xi <- shrinkage_coef_non_overlapping(c, r)
    # use the new info to estimate r recursively
    w_gmv_new <- convex_combination(
      w_gmv(S_chol_inv %*% t(S_chol_inv)),
      old_weights,
      xi)
  }
  w_gmv_new
}


#' Computes the shrinkage coefficient for non-overlapping samples.
#'
#' @param r_prev numeric
#' @param c numeric
#'
#' @return numeric
#'
shrinkage_coef_non_overlapping <- function(r_prev, c) {
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
r_update <- function(xi, c, r_prev) {
  xi^2 * c / (1 - c) + (1 - xi)^2 * r_prev
}
