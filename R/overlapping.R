#' Computes the GMV weights when shrinking dynamically with overlapping samples.
#'
#' @param data matrix in long format containing, for instance, log returns.
#' @param break_points vector of break points. The breakpoints are what determines
#' when we recompute weights.
#' @param target_w the target weights that one wants to shrink to in the first period.
#'
#' @return
#' @export
#'
#' @examples
#'
w_overlapping <- function(data, break_points, target_w, R) {
  # TODO: make sure that data has correct format/types in cols.
  stopifnot(sum(target_w) == 1)
  p <- ncol(data)
  # Theory assumes that c is less than one though the analytical formulas do not
  K <- 1
  c_vec <- c()
  Psi_vec <- c()
  for (idx in 1:length(break_points)) {
    c_vec <- c(c_vec, p/break_points[idx])
    data_subsample <- data[1:break_points[idx],]
    S <- stats::var(data_subsample)
    S_chol_inv <- t(solve(chol(S)))
    if (idx == 1) {
      old_weights <- target_w
    }else{
      old_weights <- w_gmv_new
      tmp <- c()
      for (j in 1:(idx-1)){
        tmp <- c(tmp, compute_beta(idx-1, j, Psi_vec)*compute_D(c_vec[j], c_vec[idx]))
      }
      K <-  compute_beta(0, idx-1, Psi_vec) + sum(tmp)
    }
    psi <- ((R +1) - K) / (
      (R +1) + 1/(1-c_vec[idx]) - 2*K
    )
    Psi_vec <- c(Psi_vec, psi)
    R <- R_update_overlapping(psi, c_vec[idx], R, K)
    # Compute the GMV portfolio according to the current subsample.
    # Use chol decomp since its supposed to invert faster.
    w_gmv_new <- convex_combination(
      w_gmv(S_chol_inv %*% t(S_chol_inv)),
      old_weights,
      psi)
  }
  w_gmv_new
}

#' Function which computes the coefficients (denoted beta) for the recursive formulas
#' in determining K from Bodnar et. al. 2021
#'
#' @param i integer
#' @param j integer
#' @param Psi vector of
#'
#' @return
compute_beta <- function(i,j,Psi){
  if (i==0){
    return(prod((1-Psi[1:i])))
  }
  if (i == j) {
    return(Psi[i])
  }
  Psi[j]*prod((1-Psi[i:j]))
}

#' The recursive scheme for updating the relative loss in the overlapping scheme.
#'
#' @param Psi
#' @param c
#' @param prev_R
#' @param K
#'
#' @return
R_update_overlapping <- function(Psi, c, prev_R, K){
  Psi^2 * c/(1-c) + (1-Psi)^2*prev_R + 2*Psi*(1-Psi)*(K-1)
}

#' A small helper function which computes the terms D_{ij} from Bodnar et al. 2021
#'
#' @param Ci
#' @param Cj
#'
#' @return

compute_D <- function(Ci,Cj) {
  1- 2*(1-Cj)/(
    (1-Cj) + (1-Ci)*Cj/Ci + sqrt((1-Cj/Ci)^2 + 4*(1-Ci)*Cj/Ci)
  )
}
