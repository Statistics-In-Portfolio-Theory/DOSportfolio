#' Computes the shrinkage for non-overlapping samples.
#'
#' @param r_prev numeric
#' @param c numeric
#'
#' @return numeric
#' @export
#'
#' @examples shrinkage_coef(1, 0.5)
shrinkage_coef <- function(r_prev, c) {
  (1-c)*r_prev / ((1-c)*r_prev + c)
}

r_update <- function(xi, c, r_prev) {
  xi^2 * c/(1-c) + (1-xi)^2 * r_prev
}
