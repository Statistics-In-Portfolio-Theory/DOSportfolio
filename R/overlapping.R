#' Dynamic optimal shrinkage estimator of the weights of the global minimum
#' variance portfolio when overlapping samples are used.
#'
#' The function implements the dynamic shrinkage estimator of the weights of the
#' global minimum-variance portfolio when the overlapping samples are used as
#' given in Eq. (2.23) of  \insertCite{BODNAR21dynshrink;textual}{DOSPortfolio}.
#'
#' @inheritParams wGMVNonOverlapping
#'
#' @seealso \code{\link{wGMVNonOverlapping}}
#'
#' @return a matrix of the constructed weights at each reallocation point of the
#' dynamic shrinkage estimator of the global minimum variance portfolio when
#' overlapping samples are used.
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' n <- 200*2
#' p <- 80
#' reallocation_points <- c(199)
#' data <- matrix(rt(n*p, df=5), ncol=p, nrow=n)
#' target_portfolio <- as.vector(rep(1,p))/p
#' wGMVOverlapping(data, reallocation_points, target_portfolio, 1)
#'
#' @export
wGMVOverlapping <- function(data, reallocation_points, target_portfolio, relative_loss) {
  p <- ncol(data)
  # Theory assumes that c is less than one though the analytical formulas do not
  K <- 1
  c_vec <- c()
  Psi_vec <- c()
  weights_matrix <- matrix(ncol=p, nrow=length(reallocation_points))

  for (idx in 1:length(reallocation_points)) {
    c_vec <- c(c_vec, p/reallocation_points[idx])
    data_subsample <- data[1:reallocation_points[idx],]
    if (idx == 1) {
      old_weights <- target_portfolio
    }else{
      old_weights <- w_gmv_new
      tmp <- c()
      for (j in 1:(idx-1)){
        tmp <- c(tmp, ComputeBeta(idx-1, j, Psi_vec)*ComputeD(c_vec[j], c_vec[idx]))
      }
      K <-  ComputeBeta(0, idx-1, Psi_vec) + sum(tmp)
    }
    psi <- ((relative_loss +1) - K) / (
      (relative_loss +1) + 1/(1-c_vec[idx]) - 2*K
    )
    Psi_vec <- c(Psi_vec, psi)
    # Update the relative loss according to formula
    relative_loss <- psi^2 * c_vec[idx]/(1-c_vec[idx]) + (1-psi)^2*relative_loss + 2*psi*(1-psi)*(K-1)
    w_gmv_new <- ConvexCombination(
      wGMV(data),
      old_weights,
      psi)
    weights_matrix[idx,] <- w_gmv_new
  }
  weights_matrix
}

#' A helper function for computing beta coefficients used in the case of the overlapping sample \insertCite{BODNAR21dynshrink}{DOSPortfolio}
#'
#' The function computes the beta coefficients from Eq. (2.20) in \insertCite{BODNAR21dynshrink;textual}{DOSPortfolio},
#' which are used in the recursive computation of the dynamic shrinkage estimator of the GMV portfolio weights in the
#' case of overlapping samples.
#'
#' @param i an integer greater than one.
#' @param j an integer greater than one.
#' @param Psi vector, the vector of the optimal shrinkage intensities computed in the previous step of the recursion.
#'
#' @return a number
#'
#' @keywords internal
#'
#' @references
#' \insertAllCited{}
#'
ComputeBeta <- function(i,j,Psi){
  if (i==0){
    return(prod((1-Psi[1:i])))
  }
  if (i == j) {
    return(Psi[i])
  }
  Psi[j]*prod((1-Psi[i:j]))
}

#' The recursive estimation for updating the relative loss in the variance of the
#' holding portfolio.
#'
#' This function implements equation (2.19), the recursive estimation for
#' updating the relative loss in the variance of the holding portfolio in the
#' overlapping scheme.
#'
#' @param Psi a vector of shrinkage coefficients.
#' @param c a numeric between 0 and 1. It is the concentration ration.
#' @param prev_R a numeric greater than 0, the previous value of the relative loss
#' @param K a numeric parameter.
#'
#' @return a number
#'
#' @keywords internal
rUpdateOverlapping <- function(Psi, c, prev_R, K){
  Psi^2 * c/(1-c) + (1-Psi)^2*prev_R + 2*Psi*(1-Psi)*(K-1)
}

#' A helper function for computing D-coefficients used in the case of the overlapping sample \insertCite{BODNAR21dynshrink}{DOSPortfolio}
#'
#' The function computes the \eqn{D_{j,i}} coefficients from Eq. (2.21) in \insertCite{BODNAR21dynshrink;textual}{DOSPortfolio},
#' which are used in the recursive computation of the dynamic shrinkage estimator of the GMV portfolio weights in the case
#' of overlapping samples.
#'
#' @param Ci a number equal to the concentration ratio of period i
#' @param Cj a number equal to the concentration ratio of period j
#'
#' @return a number
#'
#' @keywords internal
ComputeD <- function(Ci,Cj) {
  1- 2*(1-Cj)/(
    (1-Cj) + (1-Ci)*Cj/Ci + sqrt((1-Cj/Ci)^2 + 4*(1-Ci)*Cj/Ci)
  )
}
