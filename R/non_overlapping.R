#' Computes the shrunk GMV portfolio weights
#'
#' @param data data.frame on long format. Contains log returns.
#' @param r_previous numeric
#' @param w_target vector
#' @export
#'
#' @return vector
w_non_overlapping <- function(data, w_random_target, r_previous) {
  # TODO: make sure that data has correct format/types in cols.

  stopifnot(sum(w_random_target) == 1)
  n <- nrow(data)
  p <- ncol(data)
  c <- p/n
  # Theory assumes that c is less than one though the analytical formulas do not.
  stopifnot(c < 1)
  S_chol <- chol(stats::var(data))
  S_chol_inv <- t(solve(S_chol))
  w_gmv_new <- w_gmv(S_chol_inv %*% t(S_chol_inv))
  xi <- shrinkage_coef_non_overlapping(c, r_previous)
  convex_combination(w_gmv_new, w_random_target, xi)
}


#' Computes the shrinkage coefficient for non-overlapping samples.
#'
#' @param r_prev numeric
#' @param c numeric
#'
#' @return numeric
#' @export
#'
#' @examples shrinkage_coef_non_overlapping(1, 0.5)
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
#' @export
#'
#' @examples shrinkage_coef(1, 0.5)
r_update <- function(xi, c, r_prev) {
  xi^2 * c / (1 - c) + (1 - xi)^2 * r_prev
}
