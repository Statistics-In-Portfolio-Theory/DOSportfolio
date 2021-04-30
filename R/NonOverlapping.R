shrinkage_coef <- function(r_prev, c) {
  (1-c)*r_prev / ((1-c)*r_prev + c)
}

r_update <- function(xi, c, r_prev) {
  xi^2 * c/(1-c) + (1-xi)^2 * r_prev
}
