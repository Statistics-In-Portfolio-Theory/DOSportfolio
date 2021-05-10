#' The Dynamic Optimal Shrinkage Portfolio interface.
#'
#' This is the main interface to compute portfolios using the Dynamic Optimal Shrinkage estimation from
#' \insertCite{BODNAR21dynshrink}{DOSPortfolio}. It implements two different estimators for the shrinkage
#' coefficients, one using overlapping samples and one using non-overlapping samples.
#'
#' @param data a matrix of size (n x p), where n>p, containing, for instance, log-returns.
#' @param reallocation_points a vector of change points. The change points are what determines when we recompute weights.
#' @param target_portfolio a vector which is the target weights that one wants to shrink to in the first period.
#' @param relative_loss possibly a numeric or NULL. The initial value of the relative loss for the variance of the GMV portfolio.
#' If its NULL, then it will be initialized with the first subsample and the function \code{\link{r0Strategy}}.
#' @param shrinkage_type the type of shrinkage estimator to use. The two implemented are "non-overlapping" and "overlapping".
#'
#' @return An S3 class which contains the a matrix of the shrunk GMV portfolio weights and what type of shrinkage
#' estimator that was used to construct the portfolios. Each row of the weight matrix corresponds to the change point
#' and the column corresponds to the asset.
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
#' data <- matrix(rt(n*p, df=5), ncol=p, nrow=n)
#' weights <- DOSPortfolio(data, reallocation_points, 1)
#' @export
DOSPortfolio <- function(data, reallocation_points,
                        relative_loss=NULL,
                        target_portfolio=OnesVec(ncol(data))/ncol(data),
                        shrinkage_type="non-overlapping") {
  data <- as.matrix(data)
  new_DOSPortfolio(data, reallocation_points, target_portfolio, relative_loss, shrinkage_type)
}

#' Constructor for the DOSPortfolio class.
#'
#' @inheritParams DOSPortfolio
#'
#' @return a DOSPortfolio class.
new_DOSPortfolio <- function(data, reallocation_points, target_portfolio, relative_loss, shrinkage_type) {
  # Check dtypes of inputs
  stopifnot(is.matrix(data))
  stopifnot(is.vector(reallocation_points))
  stopifnot(is.vector(target_portfolio))
  match.arg(shrinkage_type, c("overlapping", "non-overlapping"))

  # Check the validity of the input other than it being the right type.
  validate_input(data, reallocation_points, target_portfolio, relative_loss, shrinkage_type)

  # TODO: better way to solve this?
  if (is.null(relative_loss)) {
    S <- stats::var(data[1:reallocation_points[1],])
    relative_loss <- r0Strategy(S_inv = solve(S), S=S, target_portfolio = target_portfolio,
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
#' This function is exists to validate the assumptions made to derive the analytic formulas implemented in the different
#' functions. Is called for its side-effects.
#'
#' @param data the data to validated, should be on long format.
#' @param reallocation_points a vector of break points.
#' @param target_portfolio the target vector at time point 0.
#' @param relative_loss the relative loss towards the GMV portfolios variance.
#' @param shrinkage_type what type of shrinkage method to be used.
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

