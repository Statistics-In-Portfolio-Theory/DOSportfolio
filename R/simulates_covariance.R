#' Simulates a covariance matrix
#'
#' The simulated covariance matrix has eigenvalues at 0.2, 1 and 5 with a multiplicity of p/5, 2p/5 and 2p/5 respectively.
#'
#' @param p an integer the dimension on the covariance matrix
#'
#' @return a positive definite covariance matrix which has a specific eigenvalue distribution.
#' @export
generate_sigma <- function(p) {
  # the eigenvalues
  a<- matrix(0.2, p/5,1)
  b<- matrix(1, 2*p/5,1)
  b2<- matrix(5, 2*p/5,1)
  Z <- array(stats::rnorm(p*p,0,1),dim=c(p,p))
  #1. Generating large covariance matrix Sigma
  QR<- qr(Z)
  Q<-  qr.Q(QR)
  R<-  qr.R(QR)
  D<-  diag(as.vector(diag(R)))
  D<-  D%*%solve(chol(D%*%D))
  #this matrix is Haar distributed, i.e., H%*%t(H)=I
  H<-Q%*%D
  #1.2 Choice of eigenvalues (20%=3, 40%=1 and 40%=0.5)
  E<-diag(as.vector(cbind(t(a),t(b),t(b2))))
  sigma_true<-H%*%E%*%t(H)
  return(sigma_true)
}
