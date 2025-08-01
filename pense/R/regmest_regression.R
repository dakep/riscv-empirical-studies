#' Compute (Adaptive) Elastic Net M-Estimates of Regression
#'
#' Compute elastic net M-estimates along a grid of penalization levels with optional
#' penalty loadings for adaptive elastic net.
#'
#' @param x `n` by `p` matrix of numeric predictors.
#' @param y vector of response values of length `n`.
#'          For binary classification, `y` should be a factor with 2 levels.
#' @param alpha elastic net penalty mixing parameter with \eqn{0 \le \alpha \le 1}.
#'    `alpha = 1` is the LASSO penalty, and `alpha = 0` the Ridge penalty.
#' @param scale fixed scale of the residuals.
#' @param nlambda number of penalization levels.
#' @param lambda_min_ratio Smallest value of the penalization level as a fraction of the
#'    largest level (i.e., the smallest value for which all coefficients are zero).
#'    The default depends on the sample size relative to the number of variables and `alpha`.
#'    If more observations than variables are available, the default is `1e-3 * alpha`,
#'    otherwise `1e-2 * alpha`.
#' @param penalty_loadings a vector of positive penalty loadings (a.k.a. weights)
#'    for different penalization of each coefficient. Only allowed for `alpha` > 0.
#' @param standardize logical flag to standardize the `x` variables prior to fitting the
#'    M-estimates. Coefficients are always returned on the original scale.
#'    This can fail for variables with a large proportion of a single value
#'    (e.g., zero-inflated data). In this case, either compute with
#'    `standardize = FALSE` or standardize the data manually.
#' @param lambda optional user-supplied sequence of penalization levels.
#'    If given and not `NULL`, `nlambda` and `lambda_min_ratio` are ignored.
#' @param starting_points a list of staring points, created by [starting_point()].
#'    The starting points are shared among all penalization levels.
#' @param intercept include an intercept in the model.
#' @param add_zero_based also consider the 0-based regularization path in addition to the given
#'     starting points.
#' @param cc cutoff constant for Tukey's bisquare \eqn{\rho} function.
#' @param eps numerical tolerance.
#' @param explore_solutions number of solutions to compute up to the desired precision `eps`.
#' @param explore_tol numerical tolerance for exploring possible solutions.
#'    Should be (much) looser than `eps` to be useful.
#' @param max_solutions only retain up to `max_solutions` unique solutions per penalization level.
#' @param comparison_tol numeric tolerance to determine if two solutions are equal.
#'    The comparison is first done on the absolute difference in the value of the objective
#'    function at the solution.
#'    If this is less than `comparison_tol`, two solutions are deemed equal if the
#'    squared difference of the intercepts is less than `comparison_tol` and the squared
#'    \eqn{L_2} norm of the difference vector is less than `comparison_tol`.
#' @param sparse use sparse coefficient vectors.
#' @param ncores number of CPU cores to use in parallel. By default, only one CPU core is used.
#'    Not supported on all platforms, in which case a warning is given.
#' @param algorithm_opts options for the MM algorithm to compute estimates.
#'    See [mm_algorithm_options()] for details.
#' @param mscale_bdp,mscale_opts options for the M-scale estimate used to standardize
#'    the predictors (if `standardize = TRUE`).
#'
#' @return a list-like object with the following items
#'    \describe{
#'      \item{`alpha`}{the sequence of `alpha` parameters.}
#'      \item{`lambda`}{a list of sequences of penalization levels, one per `alpha` parameter.}
#'      \item{`scale`}{the used scale of the residuals.}
#'      \item{`estimates`}{a list of estimates. Each estimate contains the following information:
#'        \describe{
#'          \item{`intercept`}{intercept estimate.}
#'          \item{`beta`}{beta (slope) estimate.}
#'          \item{`lambda`}{penalization level at which the estimate is computed.}
#'          \item{`alpha`}{*alpha* hyper-parameter at which the estimate is computed.}
#'          \item{`objf_value`}{value of the objective function at the solution.}
#'          \item{`statuscode`}{if `> 0` the algorithm experienced issues when
#'                              computing the estimate.}
#'          \item{`status`}{optional status message from the algorithm.}
#'        }
#'      }
#'      \item{`call`}{the original call.}
#'    }
#'
#' @example examples/regmest_fit.R
#'
#' @family functions to compute robust estimates
#' @seealso [regmest_cv()] for selecting hyper-parameters via cross-validation.
#' @seealso [coef.pense_fit()] for extracting coefficient estimates.
#' @seealso [plot.pense_fit()] for plotting the regularization path.
#'
#' @export
regmest <- function(x, y, alpha, nlambda = 50, lambda, lambda_min_ratio, scale, starting_points,
                    penalty_loadings, intercept = TRUE, cc = 4.7,
                    eps = 1e-6, explore_solutions = 10, explore_tol = 0.1,
                    max_solutions = 1, comparison_tol = sqrt(eps), sparse = FALSE,
                    ncores = 1, standardize = TRUE,
                    algorithm_opts = mm_algorithm_options(), add_zero_based = TRUE,
                    mscale_bdp = 0.25, mscale_opts = mscale_algorithm_options()) {
  call <- match.call(expand.dots = TRUE)
  args <- as.list(call[-1L])
  args$standardize <- isTRUE(standardize)  # Ignore standardize = 'cv_only'!
  args <- do.call(.regmest_args, args, envir = parent.frame())

  compute_fit <- function (alpha, lambda) {
    fit <- .regmest_internal(args$std_data$x, args$std_data$y,
                             alpha = alpha,
                             lambda = lambda,
                             scale = args$scale,
                             penalty_loadings = args$penalty_loadings,
                             mest_opts = args$mest_opts,
                             optional_args = args$optional_args)

    fit$estimates <- lapply(fit$estimates, function (lambda_ests) {
      lapply(lambda_ests, function (est) {
        args$restore_coef_length(args$std_data$unstandardize_coefs(est))
      })
    })
    # Handle metrics
    fit$estimates <- .metrics_attrib(fit$estimates, fit$metrics)
    fit$alpha <- alpha
    fit
  }

  fits <- mapply(
    args$alpha, args$lambda,
    SIMPLIFY = FALSE, USE.NAMES = FALSE,
    FUN = compute_fit)

  structure(list(
    call = call,
    scale = args$scale,
    lambda = args$lambda,
    estimates = unlist(lapply(fits, `[[`, 'estimates'), recursive = FALSE, use.names = FALSE),
    alpha = vapply(fits, FUN.VALUE = numeric(1L), FUN = `[[`, 'alpha', USE.NAMES = FALSE)),
    class = c('mest', 'pense_fit'))
}

#' Cross-validation for (Adaptive) Elastic Net M-Estimates
#'
#' Perform (repeated) K-fold cross-validation for [regmest()].
#'
#' @inheritParams regmest
#' @param standardize whether to standardize the `x` variables prior to fitting the PENSE estimates.
#'    Can also be set to `"cv_only"`, in which case the input data is not standardized, but the
#'    training data in the CV folds is scaled to match the scaling of the input data.
#'    Coefficients are always returned on the original scale.
#'    This can fail for variables with a large proportion of a single value
#'    (e.g., zero-inflated data).
#'    In this case, either compute with `standardize = FALSE` or standardize the data manually.
#' @template cv_params
#' @inheritDotParams regmest -standardize
#'
#' @seealso [regmest()] for computing regularized S-estimates without cross-validation.
#'
#' @return a list-like object as returned by [regmest()], plus the following components:
#'    \describe{
#'      \item{`cvres`}{data frame of average cross-validated performance.}
#'    }
#'
#' @example examples/regmest_fit.R
#'
#' @family functions to compute robust estimates with CV
#' @seealso [coef.pense_cvfit()] for extracting coefficient estimates.
#' @seealso [plot.pense_cvfit()] for plotting the CV performance or the regularization path.
#' @export
#' @importFrom stats sd
#' @importFrom rlang abort
regmest_cv <- function(x, y, standardize = TRUE, lambda, cv_k, cv_repl = 1,
                       cv_type = 'naive',
                       cv_metric = c('tau_size', 'mape', 'rmspe', 'auroc'),
                       fit_all = TRUE, cl = NULL, ...) {
  call <- match.call(expand.dots = TRUE)
  args <- do.call(.regmest_args, as.list(call[-1L]), envir = parent.frame())

  if (!identical(cv_type, 'naive')) {
    stop("`regmest_cv()` supports only `cv_type='naive'`")
  }

  fit_ses <- if (is.character(fit_all)) {
    unique(vapply(fit_all, FUN = .parse_se_string, FUN.VALUE = numeric(1L), only_fact = TRUE,
                  USE.NAMES = FALSE))
  } else if (isFALSE(fit_all)) {
    .parse_se_string('min', only_fact = TRUE)
  } else {
    TRUE
  }

  if (length(fit_ses) < 1L) {
    fit_ses <- TRUE
  }

  cv_k <- .as(cv_k[[1L]], 'integer')
  cv_repl <- .as(cv_repl[[1L]], 'integer')

  if (cv_k < 2L) {
    abort("`cv_k` must be greater than 1.")
  }

  if (cv_repl < 1L) {
    abort("`cv_repl` must be greater than 0.")
  }

  if (identical(cv_repl, 1L) && any(fit_ses > 0)) {
    warn("To use `fit_all = \"se\"`, `cv_repl` must be 2 or greater.")
    fit_ses <- 0
  }

  if (!is.null(cl)) {
    if (args$mest_opts$num_threads > 1L) {
      abort("`cl` can only be used if `ncores = 1`.")
    }
    if (!is(cl, 'cluster')) {
      abort("`cl` must be a valid `parallel` cluster.")
    }
  }

  cv_metric <- if (is.null(call$cv_metric) && args$binary_response) {
    cv_measure_str <- 'auroc'
    .cv_auroc
  } else if (is.character(cv_metric)) {
    cv_measure_str <- match.arg(cv_metric)
    switch(cv_measure_str, mape = .cv_mape, rmspe = .cv_rmspe, tau_size = tau_size,
           auroc = if (args$binary_response) {
             .cv_auroc
           } else {
             abort("cv_metric=\"auroc\" is only valid for binary responses.")
           })
  } else {
    cv_measure_str <- 'user_fun'
    match.fun(cv_metric)
  }

  if (is.null(formals(cv_metric))) {
    abort("Function `cv_metric` must accept at least 1 argument.")
  }

  # Get a common seed to be used for every alpha value
  fit_seed <- sample.int(.Machine$integer.max, 1L)

  cv_est_fun <- function (train_data, test_ind, handler_args) {
    cv_fit <- .regmest_internal(
      train_data$x, train_data$y,
      alpha = handler_args$alpha,
      lambda = handler_args$lambda,
      scale = handler_args$args$scale,
      penalty_loadings = handler_args$args$penalty_loadings,
      mest_opts = handler_args$args$mest_opts,
      optional_args = handler_args$args$optional_args)

    # For CV consider only best local optima
    lapply(cv_fit$estimates, `[[`, 1L)
  }

  cv_est_dispatch <- function (alpha, lambda) {
    set.seed(fit_seed)
    cv_perf <- .run_replicated_cv(args$std_data,
                                  cv_k = cv_k,
                                  cv_repl = cv_repl,
                                  metric = cv_metric,
                                  cv_est_fun = cv_est_fun,
                                  par_cluster = cl,
                                  handler_args = list(args = args,
                                                      alpha = alpha,
                                                      lambda = lambda))
    data.frame(lambda = lambda, alpha = alpha,
               cvavg = rowMeans(cv_perf),
               cvse = if (cv_repl > 1L) { apply(cv_perf, 1, sd) } else { 0 })
  }

  cv_curves <- mapply(
    args$alpha, args$lambda,
    SIMPLIFY = FALSE, USE.NAMES = FALSE,
    FUN = cv_est_dispatch)

  cv_curves <- do.call(rbind, cv_curves)

  if (isTRUE(fit_ses)) {
    fit_lambda <- args$lambda
  } else {
    fit_lambda <- lapply(args$alpha, function (alpha) {
      rows <- which((cv_curves$alpha - alpha)^2 < .Machine$double.eps)

      lambda_inds <- vapply(fit_ses, FUN.VALUE = numeric(1L),
                            FUN = function (se_fact) {
                              sel <- .cv_se_selection(cv_curves$cvavg[rows],
                                                      cv_curves$cvse[rows],
                                                      se_fact)
                              which(sel == 'se_fact')
                            })
      unique(cv_curves$lambda[rows[lambda_inds]])
    })
  }

  compute_global_fits <- function (alpha, lambda) {
    fit <- .regmest_internal(args$std_data$x, args$std_data$y,
                             alpha = alpha,
                             lambda = lambda,
                             scale = args$scale,
                             penalty_loadings = args$penalty_loadings,
                             mest_opts = args$mest_opts,
                             optional_args = args$optional_args)

    fit$estimates <- lapply(fit$estimates, function (lambda_ests) {
      lapply(lambda_ests, function (est) {
        args$restore_coef_length(args$std_data$unstandardize_coefs(est))
      })
    })
    # Handle metrics
    fit$estimates <- .metrics_attrib(fit$estimates, fit$metrics)
    fit$alpha <- alpha
    fit
  }

  fits <- mapply(
    args$alpha, fit_lambda,
    SIMPLIFY = FALSE, USE.NAMES = FALSE,
    FUN = compute_global_fits)

  structure(list(
    call = call,
    scale = args$scale,
    cvres = cv_curves,
    cv_measure = cv_measure_str,
    lambda = args$lambda,
    estimates = unlist(lapply(fits, `[[`, 'estimates'),
                       recursive = FALSE, use.names = FALSE),
    alpha = vapply(fits, FUN.VALUE = numeric(1L),
                   FUN = `[[`, 'alpha',
                   USE.NAMES = FALSE)),
    class = c('mest', 'pense_cvfit'))
}

#' @description `adamest_cv()` is a convenience wrapper to compute adaptive elastic-net M-estimates.
#'
#' @details
#' `adamest_cv()` is a convenience wrapper which performs 3 steps:
#'
#' 1. compute preliminary estimates via `regmest_cv(..., alpha = alpha_preliminary)`,
#' 2. computes the penalty loadings from the estimate `beta` with best prediction performance by
#'    `adamest_loadings = 1 / abs(beta)^exponent`, and
#' 3. compute the adaptive PENSE estimates via
#'    `regmest_cv(..., penalty_loadings = adamest_loadings)`.
#'
#' @param alpha_preliminary `alpha` parameter for the preliminary estimate.
#' @param exponent the exponent for computing the penalty loadings based on the
#'                 preliminary estimate.
#'
#' @return a list-like object as returned by [adamest_cv()] plus the following components:
#'    \describe{
#'      \item{`exponent`}{value of the exponent.}
#'      \item{`preliminary`}{CV results for the preliminary estimate.}
#'      \item{`penalty_loadings`}{penalty loadings used for the adaptive
#'                                elastic net M-estimate.}
#'    }
#'
#' @rdname regmest_cv
#' @family functions to compute robust estimates with CV
#' @export
#' @importFrom stats coef
#' @importFrom rlang abort
adamest_cv <- function (x, y, alpha, alpha_preliminary = 0, exponent = 1, ...) {
  call <- match.call(expand.dots = TRUE)
  if (!is.null(call$penalty_loadings)) {
    abort(paste("Argument `penalty_loadings` not valid for `adamest_cv`",
                "Penalty loadings are determined internally."))
  }
  exponent <- .as(exponent[[1L]], 'numeric')

  # Compute preliminary estimate
  prelim_call <- call
  prelim_call[[1L]] <- quote(pense::regmest_cv)
  prelim_call$alpha <- .as(alpha_preliminary[[1L]], 'numeric')
  prelim_call$alpha_preliminary <- NULL
  prelim_call$exponent <- NULL
  prelim <- eval.parent(prelim_call)

  prelim_coef <- coef(prelim, sparse = FALSE, concat = FALSE)
  pen_loadings <- abs(prelim_coef$beta)^(-exponent)

  adamstep <- regmest_cv(x, y, alpha = alpha,
                         penalty_loadings = pen_loadings, ...)
  adamstep$call <- call
  adamstep$exponent <- exponent
  adamstep$preliminary <- prelim
  adamstep$penalty_loadings <- pen_loadings
  return(adamstep)
}

## Get the smallest lambda such that the PENSE estimate gives the empty model.
.regmest_max_lambda <- function (x, y, alpha, scale, mest_options,
                                 penalty_loadings = NULL) {
  optional_args <- list()
  if (!is.null(penalty_loadings)) {
    optional_args$pen_loadings <- penalty_loadings
  }
  .Call(C_mesten_max_lambda, x, y, scale, mest_options, optional_args) /
    max(0.01, alpha)
}

## Generate a log-spaced grid of decreasing lambda values
.regmest_lambda_grid <- function (x, y, alpha, scale, nlambda,
                                  lambda_min_ratio, mest_options,
                                  penalty_loadings) {
  alpha <- max(0.01, alpha)
  x_dim <- dim(x)
  if (is.null(lambda_min_ratio)) {
    lambda_min_ratio <- alpha * if (x_dim[[1L]] > x_dim[[2L]]) { 1e-3 } else { 1e-2 }
  }
  max_lambda <- .regmest_max_lambda(x, y, alpha, scale, mest_options, penalty_loadings)
  rev(exp(seq(log(lambda_min_ratio * max_lambda), log(max_lambda), length.out = nlambda)))
}

## Perform some final input adjustments and call the internal C++ code.
.regmest_internal <- function(x, y, alpha, scale, lambda, penalty_loadings = NULL, mest_opts,
                              optional_args) {
  # Create penalties-list, without sorting the lambda sequence
  penalties <- lapply(lambda, function (l) { list(lambda = l, alpha = alpha) })

  if (!is.null(penalty_loadings)) {
    optional_args$pen_loadings <- penalty_loadings
  }

  .Call(C_mesten_regression, x, y, scale, penalties, mest_opts, optional_args)
}

#' @importFrom rlang warn abort
#' @importFrom methods is
#' @importFrom Matrix sparseVector
#' @importFrom stats runif
.regmest_args <- function (x, y, alpha, nlambda = 50, lambda, lambda_min_ratio, scale,
                           starting_points, penalty_loadings, intercept = TRUE, cc = 4.7,
                           eps = 1e-6, explore_solutions = 10, explore_tol = 0.1,
                           max_solutions = 10, comparison_tol = sqrt(eps), sparse = FALSE,
                           ncores = 1, standardize = TRUE, algorithm_opts = mm_algorithm_options(),
                           add_zero_based = TRUE, mscale_bdp = 0.25,
                           mscale_opts = mscale_algorithm_options(), ...) {
  args_call <- match.call(expand.dots = FALSE)
  optional_args <- list()

  ## Process input arguments
  response <- .validate_response(y)
  y <- response$values
  x_dim <- dim(x)

  if (length(y) != x_dim[[1L]]) {
    abort("Number of observations in `x` and `y` does not match.")
  } else if (x_dim[[2L]] <= 1L) {
    abort("`x` must be a matrix with at least 2 columns.")
  }

  alpha <- .as(alpha, 'numeric')
  if (any(alpha < 0 | alpha > 1)) {
    abort("`alpha` is outside 0 and 1.")
  } else if (any(alpha < sqrt(.Machine$double.eps))) {
    alpha[which(alpha < sqrt(.Machine$double.eps))] <- 0
    if (any(alpha > 0)) {
      abort("`alpha=0` cannot be mixed with other `alpha` values.")
    }
  }

  scale <- .as(scale[[1L]], 'numeric')
  if (!isTRUE(scale > .Machine$double.eps)) {
    abort("`scale` must be positive.")
  }

  mscale_opts <- .full_mscale_algo_options(bdp = mscale_bdp, mscale_opts = mscale_opts)

  mest_opts <- list(algo_opts = algorithm_opts,
                    cc = .as(cc[[1L]], 'numeric'),
                    strategy_0 = isTRUE(add_zero_based),
                    algorithm = .regmest_algorithm_id(algorithm_opts),
                    intercept = !isFALSE(intercept),
                    sparse = isTRUE(sparse),
                    eps = .as(eps[[1L]], 'numeric'),
                    max_it = mscale_opts$max_it,  # max. iterations for finding the largest lambda
                    comparison_tol = .as(comparison_tol[[1L]], 'numeric'),
                    explore_tol = .as(explore_tol[[1L]], 'numeric'),
                    nr_tracks = .as(explore_solutions[[1L]], 'integer'),
                    max_optima = .as(max_solutions[[1L]], 'integer'),
                    num_threads = max(1L, .as(ncores[[1L]], 'integer')))

  if (mest_opts$cc < .Machine$double.eps) {
    abort("`cc` must be positive.")
  }

  if (mest_opts$explore_tol < mest_opts$eps) {
    abort("`explore_tol` must not be less than `eps`")
  }
  if (mest_opts$comparison_tol < mest_opts$eps) {
    abort("`comparison_tol` must not be less than `eps`")
  }

  # If using the MM algorithm, ensure that the EN options are set.
  if (mest_opts$algorithm == 1L) {
    mest_opts$algo_opts$en_options <- .select_en_algorithm(mest_opts$algo_opts$en_options,
                                                           alpha,
                                                           mest_opts$sparse, eps)
    mest_opts$sparse <- mest_opts$algo_opts$en_options$sparse
  }

  # Set the number of cores for the ENPY options
  if (mest_opts$num_threads > 1L && !isTRUE(.k_multithreading_support)) {
    warn("Multithreading not supported. Using only 1 core.")
    mest_opts$num_threads <- 1L
  }

  # Standardizing the data
  standardize <- if (is.character(standardize)) {
    if (pmatch(standardize[[1L]], 'cv_only', nomatch = 0L) == 1L) {
      standardize <- 'cv_only'
    } else {
      abort("`standardize` must be either TRUE/FALSE or \"cv_only\".")
    }
  } else {
    isTRUE(standardize)
  }

  # Check penalty loadings
  if (!missing(penalty_loadings) && !is.null(penalty_loadings)) {
    checked_pls <- .prepare_penalty_loadings(penalty_loadings, x, alpha, sparse = mest_opts$sparse)
    penalty_loadings <- checked_pls$loadings
    restore_coef_length <- checked_pls$restore_fun
    x <- checked_pls$trimmed_x
  } else {
    restore_coef_length <- function (coef) coef
    penalty_loadings <- NULL
  }

  if (ncol(x) == 0L) {
    warn("All values in `penalty_loadings` are infinite. Only computing the intercept.")
    mest_opts$intercept <- TRUE
    std_data <- .standardize_data(matrix(runif(x_dim[[1L]]), ncol = 1L), y, intercept = TRUE,
                                  sparse = mest_opts$sparse,
                                  standardize = standardize, robust = TRUE,
                                  mscale_opts = mscale_opts,
                                  bdp = mscale_opts$delta, cc = mscale_opts$cc)
    # Compute only the 0-based solution.
    mest_opts$strategy_0 <- TRUE
    lambda <- lapply(args$alpha, FUN = .regmest_lambda_grid,
                     x = std_data$x, y = std_data$y, scale = scale,
                     nlambda = 1, lambda_min_ratio = 1,
                     mest_options = mest_opts, penalty_loadings1 = NULL)

    return(list(std_data = std_data,
                alpha = args$alpha,
                lambda = lambda,
                scale = scale,
                penalty_loadings = NULL,
                mest_opts = mest_opts,
                optional_args = optional_args,
                restore_coef_length = restore_coef_length))
  }

  std_data <- .standardize_data(x, y, intercept = mest_opts$intercept,
                                standardize = standardize,
                                robust = TRUE,
                                sparse = mest_opts$sparse,
                                mscale_opts = mscale_opts,
                                bdp = mscale_opts$delta,
                                cc = mscale_opts$cc)

  # Scale penalty loadings appropriately
  penalty_loadings <- penalty_loadings / std_data$scale_x
  if (length(penalty_loadings) == 0L) {
    penalty_loadings <- NULL
  }

  # Determine lambda grid
  lambda <- if (missing(lambda) || is.null(lambda)) {
    if (missing(lambda_min_ratio)) {
      lambda_min_ratio <- NULL
    }
    lapply(alpha, FUN = .regmest_lambda_grid,
           x = std_data$x, y = std_data$y, scale = scale,
           nlambda = nlambda,
           lambda_min_ratio = lambda_min_ratio,
           mest_options = mest_opts,
           penalty_loadings = penalty_loadings)
  } else if (!is.list(lambda)) {
    rep.int(list(sort(.as(lambda, 'numeric'), decreasing = TRUE)), length(alpha))
  } else if (identical(length(lambda), length(alpha))) {
    lapply(lambda, function (l) {
      if (any(l < .Machine$double.eps)) {
        abort("All values in `lambda` must be positive.")
      }
      sort(.as(l, 'numeric'), decreasing = TRUE)
    })
  } else {
    abort("`lambda` must either be a numeric vector or a list the same length as `alpha`.")
  }

  if (!missing(starting_points)) {
    if (is(starting_points, 'pense_fit') || is(starting_points, 'pense_cvfit')) {
      starting_points <- as_starting_point(starting_points)
    } else if (is(starting_points, 'starting_point')) {
      starting_points <- structure(list(starting_points), class = 'starting_points')
    } else if (!is(starting_points, 'starting_points')) {
      abort(paste("`starting_points` must be a list of starting points created by",
                  "`starting_point()`, `enpy_initial_estimates()`, or a combination thereof."))
    }

    # Ensure the `beta` coefficients in `other_starts` agree with the desired vector class
    # (sparse vs. dense) and standardize them.
    optional_args$shared_starts <- lapply(.sparsify_other_starts(starting_points, mest_opts$sparse),
                                          std_data$standardize_coefs)
  }

  return(list(std_data = std_data,
              binary_response = response$binary,
              alpha = alpha,
              scale = scale,
              lambda = lambda,
              penalty_loadings = penalty_loadings,
              mest_opts = mest_opts,
              optional_args = optional_args,
              restore_coef_length = restore_coef_length))
}
