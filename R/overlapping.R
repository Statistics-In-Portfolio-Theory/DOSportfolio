#' The (overlapping) Global Minimum Variance (GMV) portfolio using a dynamic optimal shrinkage scheme.
#'
#' This function implements the second version of the recursive estimation of the GMV portfolio in the dynamic optimal shrinkage setting.
#' The difference between this function and the function \code{wGMVNonOverlapping(...)} is that this function estimates each GMV portfolio using
#' all available data. To see a more detailed description of the method, see vigenette
#' \code{vignette("DOSPortfolio", package = "DOSPortfolio")}.
#'
#' @param data a matrix of size (n x p), where n>p, containing, for instance, log-returns.
#' @param break_points a vector of break points. The breakpoints are what determines
#' when we recompute weights.
#' @param target_w a vector which is the target weights that one wants to shrink to in the first period.
#' @param R a numeric of the initial value of the relative loss for the variance of the GMV portfolio.
#'
#' @return vector of portfolio weights
#' @export
#'
#' @examples
#' n <- 200*2
#' p <- 80
#' c <- p/n
#' break_points <- c(199)
#' data <- matrix(rt(n*p, df=5), ncol=p, nrow=n)
#' target_w <- as.vector(rep(1,p))/p
#' wGMVOverlapping(data, break_points, target_w, 1)
#'
wGMVOverlapping <- function(data, break_points, target_w, R) {
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
        tmp <- c(tmp, ComputeBeta(idx-1, j, Psi_vec)*ComputeD(c_vec[j], c_vec[idx]))
      }
      K <-  ComputeBeta(0, idx-1, Psi_vec) + sum(tmp)
    }
    psi <- ((R +1) - K) / (
      (R +1) + 1/(1-c_vec[idx]) - 2*K
    )
    Psi_vec <- c(Psi_vec, psi)
    R <- rUpdateOverlapping(psi, c_vec[idx], R, K)
    # Compute the GMV portfolio according to the current subsample.
    # Use chol decomp since its supposed to invert faster.
    w_gmv_new <- ConvexCombination(
      wGMV(S_chol_inv %*% t(S_chol_inv)),
      old_weights,
      psi)
  }
  structure(w_gmv_new, class="DOSPortfolio")
}

#' A helper function for computation of coefficients.
#'
#' Function which computes the coefficients (denoted beta) for the recursive formulas in determining K from Bodnar et. al. 2021
#'
#' @param i integer a integer greater than one.
#' @param j integer a integer greater than one.
#' @param Psi vector of shrinkage coefficients.
#'
#' @return a number
ComputeBeta <- function(i,j,Psi){
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
#' @param Psi vector of shrinkage coefficients.
#' @param c numeric between 0 and 1. It is the concentration ration.
#' @param prev_R numeric greater than 0, The previous value of the relative loss
#' @param K numeric parameter.
#'
#' @return a number
rUpdateOverlapping <- function(Psi, c, prev_R, K){
  Psi^2 * c/(1-c) + (1-Psi)^2*prev_R + 2*Psi*(1-Psi)*(K-1)
}

#' A small helper function which computes the terms D_{ij} from Bodnar et al. 2021
#'
#' @param Ci numeric concentration ratio of period i
#' @param Cj numeric concentration ratio of period j
#'
#' @return a number
ComputeD <- function(Ci,Cj) {
  1- 2*(1-Cj)/(
    (1-Cj) + (1-Ci)*Cj/Ci + sqrt((1-Cj/Ci)^2 + 4*(1-Ci)*Cj/Ci)
  )
}
