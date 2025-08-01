% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cv_utilities.R
\name{.run_replicated_cv_ris}
\alias{.run_replicated_cv_ris}
\title{Run replicated K-fold CV with random splits, matching the global estimates
to the CV estimates by Kendall's tau-b computed on the robustness weights.}
\usage{
.run_replicated_cv_ris(
  std_data,
  cv_k,
  cv_repl,
  cv_est_fun,
  global_ests,
  min_similarity = 0,
  par_cluster = NULL,
  rho_cc,
  handler_args = list()
)
}
\arguments{
\item{std_data}{standardized full data set
(standardized by \code{.standardize_data})}

\item{cv_k}{number of folds per CV split}

\item{cv_repl}{number of CV replications.}

\item{cv_est_fun}{function taking the standardized training set and
the indices of the left-out observations and returns a list of estimates.
The function always needs to return the same number of estimates!}

\item{global_ests}{estimates computed on all observations.}

\item{min_similarity}{minimum (average) similarity for CV solutions to be considered
(between 0 and 1).
If no CV solution satisfies this lower bound, the best CV solution will be used regardless
of similarity.}

\item{par_cluster}{parallel cluster to parallelize computations.}

\item{rho_cc}{consistency constant for Tukey's bisquare rho function.}

\item{handler_args}{additional arguments to the handler function.}
}
\description{
Run replicated K-fold CV with random splits, matching the global estimates
to the CV estimates by Kendall's tau-b computed on the robustness weights.
}
\keyword{internal}
