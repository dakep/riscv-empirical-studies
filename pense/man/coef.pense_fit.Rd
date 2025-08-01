% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coef-methods.R
\name{coef.pense_fit}
\alias{coef.pense_fit}
\title{Extract Coefficient Estimates}
\usage{
\method{coef}{pense_fit}(object, lambda, alpha = NULL, sparse = NULL, standardized = FALSE, ...)
}
\arguments{
\item{object}{PENSE regularization path to extract coefficients from.}

\item{lambda}{a single number for the penalty level.}

\item{alpha}{Either a single number or \code{NULL} (default).
If given, only fits with the given \code{alpha} value are considered.
If \code{object} was fit with multiple \code{alpha} values, and no value is provided, the
first value in \code{object$alpha} is used with a warning.}

\item{sparse}{should coefficients be returned as sparse or dense vectors? Defaults to the
sparsity setting in \code{object}.
Can also be set to \code{sparse = 'matrix'}, in which case a sparse matrix
is returned instead of a sparse vector.}

\item{standardized}{return the standardized coefficients.}

\item{...}{currently not used.}
}
\value{
either a numeric vector or a sparse vector of type
\link[Matrix:sparseVector-class]{dsparseVector}
of size \eqn{p + 1}, depending on the \code{sparse} argument.
Note: prior to version 2.0.0 sparse coefficients were returned as sparse matrix
of type \emph{dgCMatrix}.
To get a sparse matrix as in previous versions, use \code{sparse = 'matrix'}.
}
\description{
Extract coefficients from an adaptive PENSE (or LS-EN) regularization path fitted by \code{\link[=pense]{pense()}}
or \code{\link[=elnet]{elnet()}}.
}
\examples{
# Compute the PENSE regularization path for Freeny's revenue data
# (see ?freeny)
data(freeny)
x <- as.matrix(freeny[ , 2:5])

regpath <- pense(x, freeny$y, alpha = 0.5)
plot(regpath)

# Extract the coefficients at a certain penalization level
coef(regpath, lambda = regpath$lambda[[1]][[40]])

# What penalization level leads to good prediction performance?
set.seed(123)
cv_results <- pense_cv(x, freeny$y, alpha = 0.5,
                       cv_repl = 2, cv_k = 4)
plot(cv_results, se_mult = 1)

# Print a summary of the fit and the cross-validation results.
summary(cv_results)

# Extract the coefficients at the penalization level with
# smallest prediction error ...
coef(cv_results)
# ... or at the penalization level with prediction error
# statistically indistinguishable from the minimum.
coef(cv_results, lambda = '1-se')
}
\seealso{
\code{\link[=coef.pense_cvfit]{coef.pense_cvfit()}} for extracting coefficients from a PENSE fit with
hyper-parameters chosen by cross-validation

Other functions for extracting components: 
\code{\link{coef.pense_cvfit}()},
\code{\link{predict.pense_cvfit}()},
\code{\link{predict.pense_fit}()},
\code{\link{residuals.pense_cvfit}()},
\code{\link{residuals.pense_fit}()}
}
\concept{functions for extracting components}
