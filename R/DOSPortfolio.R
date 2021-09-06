#' The Dynamic Optimal Shrinkage Portfolio interface.
#'
#' This is the main function to compute the weights of the global minimum
#' variance portfolio by using the dynamic optimal shrinkage estimators presented
#' in Eq. (2.11) and Eq. (2.23) of
#' \insertCite{BODNAR21dynshrink;textual}{DOSPortfolio}. It implements two
#' different estimators for the shrinkage coefficients, one using overlapping
#' samples (see, Eq. (2.23) of
#' \insertCite{BODNAR21dynshrink;textual}{DOSPortfolio}) and one using
#' non-overlapping samples (see, Eq. (2.11) of
#' \insertCite{BODNAR21dynshrink;textual}{DOSPortfolio}).
#'
#' @param data an n by p matrix of asset returns. Columns represent different
#' assets rows are observations, where n>p, containing, for instance, log-returns.
#' @param reallocation_points a vector of reallocation points. The reallocation
#' points determine when the holding portfolio should be reconstructed and its
#' weights should be recomputed.
#' @param target_portfolio a vector which determines the weights of the target
#' portfolio used when the shrinkage estimator of the global minimum variance
#' portfolio is constructed for the first time.
#' @param relative_loss possibly a numeric or NULL. The initial value of the
#' relative loss in the variance of the target portfolio. If its NULL, then it
#' will be initialized with the first subsample and the function
#' \code{\link{r0Strategy}}.
#' @param shrinkage_type the type of shrinkage estimator to use. The two
#' implemented approaches are "non-overlapping" and "overlapping".
#'
#' @return An S3 class which contains the matrix of the constructed weights of
#' the dynamic shrinkage estimator of the global minimum variance portfolio and
#' the type of the shrinkage estimator (i.e., "overlapping" or
#' "non-overlapping") that was used in its construction. Each row of the weight
#' matrix corresponds to the reallocation point and the column corresponds to the
#' asset.
#'
#' @seealso Section 2.1 and 2.2 of \insertCite{BODNAR21dynshrink}{DOSPortfolio}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' n <- 250*2
#' p <- 80
#' c <- p/n
#' reallocation_points <- c(120, 240)
#' data <- sqrt(5/3) * matrix(rt(n*p, df=5) , ncol=p, nrow=n)
#' weights <- DOSPortfolio(data, reallocation_points, 1)
#' @export
DOSPortfolio <- function(data,
                         reallocation_points,
                         relative_loss=NULL,
                         target_portfolio=rep(1,ncol(data))/ncol(data),
                         shrinkage_type="non-overlapping") {
  data <- as.matrix(data)
  new_DOSPortfolio(data, reallocation_points, target_portfolio, relative_loss, shrinkage_type)
}

#' Constructor for the DOSPortfolio class
#'
#' @inheritParams DOSPortfolio
#'
#' @return a DOSPortfolio class.
new_DOSPortfolio <- function(data,
                             reallocation_points,
                             target_portfolio,
                             relative_loss,
                             shrinkage_type) {
  # Check dtypes of inputs
  stopifnot(is.matrix(data))
  stopifnot(is.vector(reallocation_points))
  stopifnot(is.vector(target_portfolio))
  match.arg(shrinkage_type, c("overlapping", "non-overlapping"))

  # Check the validity of the input other than it being the right type.
  validate_input(data, reallocation_points, target_portfolio, relative_loss, shrinkage_type)

  # TODO: better way to solve this?
  if (is.null(relative_loss)) {
    relative_loss <- r0Strategy(data[1:reallocation_points[1],],
                                target_portfolio = target_portfolio,
                                c = ncol(data)/reallocation_points[1])
  }
  if (shrinkage_type == "overlapping") {
    x <- wGMVOverlapping(data, reallocation_points, target_portfolio, relative_loss)
  }else{
    x <- wGMVNonOverlapping(data, reallocation_points, target_portfolio, relative_loss)
  }
  # add mean variance options here.
  structure(list("weights"=x,
                 "shrinkage_type"=shrinkage_type),
            class="DOSPortfolio")
}

#' Validates input to the DOSPortfolio function.
#'
#' This function validates the assumptions made to derive the analytic formulas
#' implemented in the different functions of the package. It is called for its
#' side-effects.
#'
#' @inheritParams DOSPortfolio
#'
#' @return NULL, only called for its side effects
validate_input <- function(data, reallocation_points, target_portfolio, relative_loss, shrinkage_type) {
  if (!(ncol(data) > 2)){
    stop("Need more than two assets (columns) in data.", call. = FALSE)
  }
  if (ncol(data) > nrow(data)) {
    stop("More assets (columns) than observations (rows).",
         call. = FALSE)
  }
  if (reallocation_points[1] > nrow(data)) {
    stop("First break point is past the number of observations.",
         call. = FALSE)
  }
  if (any(!(diff(reallocation_points) > 0))) {
    stop("The break points need to be in increasing order",
         call. = FALSE)
  }
  if (shrinkage_type == "non-overlapping") {
    # assert that all subsamples are of correct size
    if (any(ncol(data)/diff(reallocation_points) > 1)) {
      stop("Non-overlapping estimator can not handle concentration ratios above one.
            Consider excluding one (or more) break point(s) or provide more data.", call.=FALSE)
    }
  }
  if (length(target_portfolio) != ncol(data)){
    stop("Number of assets (columns) and length of target portfolio differ.")
  }
  if (!(abs(sum(target_portfolio)- 1) < 1e-6)) {
    stop("The target portfolio is not normalised, e.g. does not sum to one.")
  }
}

