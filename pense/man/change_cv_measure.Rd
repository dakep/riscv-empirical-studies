% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/change_cv_measure.R
\name{change_cv_measure}
\alias{change_cv_measure}
\title{Change the Cross-Validation Measure}
\usage{
change_cv_measure(
  x,
  measure = c("wrmspe", "wmape", "tau_size", "wrmspe_cv", "wmape_cv"),
  max_solutions = Inf
)
}
\arguments{
\item{x}{fitted (adaptive) PENSE or M-estimator}

\item{measure}{the measure to use for prediction accuracy}

\item{max_solutions}{consider only this many of the best solutions.
If missing, all solutions are considered.}
}
\value{
a \code{pense.cvfit} object using the updated measure of prediction accuracy
}
\description{
For cross-validated fits using the RIS-CV strategy, the measure of prediction
accuracy can be adjusted post-hoc.
}
