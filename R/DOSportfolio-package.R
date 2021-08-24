#' A set of tools for constructing Dynamic Optimal Shrinkage estimator of the
#' global minimum variance portfolio.
#'
#' The DOSPortfolio package consists of two shrinkage estimators for the Global
#' Minimum Variance (GMV) portfolios. These are implemented in the packages main
#' interface: \code{\link{DOSPortfolio}}. The shrinkage is performed at fixed
#' reallocation points in a dynamic manner where the current (random) portfolio
#' is shrunk towards a new (random) GMV portfolio. The reallocation points are
#' specified by a deterministic sequence given a priori. The estimation is
#' performed such that the shrinkage coefficients are optimal for large
#' portfolios, e.g. there are many assets in comparison to observations. The
#' main interface validates the main assumptions of the theory used to derive
#' these methods.
#'
#' @section Methods:
#'
#' DOSPortfolio: \insertCite{BODNAR21dynshrink;textual}{DOSPortfolio}
#'
#'
#' @name DOSPortfolio-package
#' @docType package
#' @references
#' \insertAllCited{}
NULL

# Importing from Rdpack (for working with references)
# to avoid getting a warning from "R CMD check"
#' @importFrom Rdpack reprompt
NULL
