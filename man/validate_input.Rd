% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DOSPortfolio.R
\name{validate_input}
\alias{validate_input}
\title{Validates input to the DOSPortfolio function.}
\usage{
validate_input(
  data,
  reallocation_points,
  target_portfolio,
  relative_loss,
  shrinkage_type
)
}
\arguments{
\item{data}{an n by p matrix of asset returns. Columns represent different
assets rows are observations, where n>p, containing, for instance, log-returns.}

\item{reallocation_points}{a vector of reallocation points. The reallocation
points determine when the holding portfolio should be reconstructed and its
weights should be recomputed.}

\item{target_portfolio}{a vector which determines the weights of the target
portfolio used when the shrinkage estimator of the global minimum variance
portfolio is constructed for the first time.}

\item{relative_loss}{possibly a numeric or NULL. The initial value of the
relative loss in the variance of the target portfolio. If its NULL, then it
will be initialized with the first subsample and the function
\code{\link{r0Strategy}}.}

\item{shrinkage_type}{the type of shrinkage estimator to use. The two
implemented approaches are "non-overlapping" and "overlapping".}
}
\value{
NULL, only called for its side effects
}
\description{
This function validates the assumptions made to derive the analytic formulas
implemented in the different functions of the package. It is called for its
side-effects.
}
