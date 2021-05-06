

#' The Dynamic Optimal Shrinkage Portfolio interface.
#'
#' @inheritParams wGMVNonOverlapping
#' @param shrinkage_type the type of shrinkage estimator to use. The two implemented are "non-overlapping" and "overlapping"
#'
#' @return a DOSPortfolio class with the GMV weights and its corresponding type.
#' @export
#'
#' @examples
#' n <- 250*2
#' p <- 80
#' c <- p/n
#' change_points <- c(120, 240)
#' data <- matrix(rt(n*p, df=5), ncol=p, nrow=n)
#' weights <- DOSPortfolio(data, change_points, 1)
#'
DOSPortfolio <- function(data, change_points,
                        relative_loss=NULL,
                        target_w=OnesVec(ncol(data))/ncol(data),
                        shrinkage_type="non-overlapping") {
  data <- as.matrix(data)
  new_DOSPortfolio(data, change_points, target_w, relative_loss, shrinkage_type)
}

#' Constructor for the DOSPortfolio class.
#'
#' @inheritParams DOSPortfolio
#'
#' @return a DOSPortfolio class.
new_DOSPortfolio <- function(data, change_points, target_w, relative_loss, shrinkage_type) {
  # Check dtypes of inputs
  stopifnot(is.matrix(data))
  stopifnot(is.vector(change_points))
  stopifnot(is.vector(target_w))
  match.arg(shrinkage_type, c("overlapping", "non-overlapping"))

  # Check the validity of the input other than it being the right type.
  validate_input(data, change_points, target_w, relative_loss, shrinkage_type)

  # TODO: better way to solve this?
  if (is.null(relative_loss)) {
    S <- stats::var(data[1:change_points[1],])
    relative_loss <- r0Strategy(S_inv = solve(S), S=S, target_w = target_w,
                                c = ncol(data)/change_points[1])
  }

  if (shrinkage_type == "overlapping") {
    x <- wGMVOverlapping(data, change_points, target_w, relative_loss)
  }else{
    x <- wGMVNonOverlapping(data, change_points, target_w, relative_loss)
  }
  # add mean variance options here.
  structure(list("weights"=x,
                 "type"=shrinkage_type),
            class="DOSPortfolio")
}

#' Validates input to the DOSPortfolio function.
#'
#' This function is exists to validate the assumptions made to derive the analytic formulas implemented in the different
#' functions. Is called for its side-effects.
#'
#' @param data the data to validated, should be on long format.
#' @param change_points a vector of break points.
#' @param target_w the target vector at time point 0.
#' @param relative_loss the relative loss towards the GMV portfolios variance.
#' @param shrinkage_type what type of shrinkage method to be used.
#'
#' @return NULL, only called for its side effects
validate_input <- function(data, change_points, target_w, relative_loss, shrinkage_type) {
  if (!(ncol(data) > 2)){
    stop("Need more than two assets (columns) in data.", call. = FALSE)
  }
  if (ncol(data) > nrow(data)) {
    stop("More assets (columns) than observations (rows).",
         call. = FALSE)
  }
  if (change_points[1] > nrow(data)) {
    stop("First break point is past the number of observations.",
         call. = FALSE)
  }
  if (any(!(diff(change_points) > 0))) {
    stop("The break points need to be in increasing order",
         call. = FALSE)
  }
  if (shrinkage_type == "non-overlapping") {
    # assert that all subsamples are of correct size
    if (any(ncol(data)/diff(change_points) > 1)) {
      stop("Non-overlapping estimator can not handle concentration ratios above one.
            Consider excluding one break point or provide more data.", call.=FALSE)
    }
  }
  if (length(target_w) != ncol(data)){
    stop("Number of assets (columns) and length of target portfolio differ.")
  }
  if (sum(target_w) != 1){
    stop("The target portfolio is not normalised, e.g. does not sum to one.")
  }
}

