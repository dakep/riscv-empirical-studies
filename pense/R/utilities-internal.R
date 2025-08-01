#' Get the Constant for Consistency for the M-Scale Using the Bisquare Rho Function
#' @param delta desired breakdown point (between 0 and 0.5)
#'
#' @return consistency constant
#' @keywords internal
#' @importFrom stats pnorm uniroot
#' @importFrom rlang abort
.bisquare_consistency_const <- function (delta) {
  ##
  ## Pre-computed values for some delta values
  ##
  eps <- sqrt(.Machine$double.eps)
  if (!isTRUE(delta < 0.5 + eps && delta > -eps)) {
    abort("`delta` is outside valid bounds")
  }

  if (abs(delta - 0.5) < eps) {
    return(1.5476450)
  } else if (abs(delta - 0.25) < eps) {
    return(2.937015)
  } else if (abs(delta - 0.1) < eps) {
    return(5.182361)
  } else if (delta < 0.005) {
    return(50) # ~.1% bdp for bisquare
  }
  integral_interval <- if (delta > 0.1) {
    c(1.5, 5.5)
  } else {
    c(5, 25)
  }

  # For bisquare we have the closed form solution to the expectation
  expectation <- function(cc, delta) {
    pnorm.mcc <- 2 * pnorm(-cc)
    1/cc^6 * exp(-(cc^2/2)) * (
      -cc * (15 - 4 * cc^2 + cc^4) * sqrt(2 / pi) +
        3 * (5 - 3 * cc^2 + cc^4) * exp(cc^2/2) * (1 - pnorm.mcc) +
        cc^6 * exp(cc^2/2) * pnorm.mcc
    ) - delta
  }
  uniroot(expectation, interval = integral_interval, delta)$root
}

#' Determine a breakdown point with stable numerical properties of the M-scale
#' with Tukey's bisquare rho function.
#'
#' The M-scale objective (and hence the S-loss) can have unbounded or very high
#' 1st derivative. This can lead to numerical instability of the algorithms and
#' in turn excessive computation time.
#' This function chooses the breakdown point with lowest upper bound of the 1st
#' derivative from a range of bdp's in the vicinity of the desired bdp.
#'
#' @param n number of observations in the sample
#' @param desired_bdp the desired breakdown point (between 0.05 and 0.5)
#' @param tolerance how far can the chosen bdp be away from the desired bdp.
#'                  The chosen bdp is guaranteed to be in the range given by `interval`.
#' @param interval restrict the chosen bdp to this interval.
#' @param precision granularity of the grid of considered bdp's.
#' @importFrom rlang warn
#' @keywords internal
.find_stable_bdb_bisquare <- function (n, desired_bdp, tolerance = 0.01, precision = 1e-4,
                                       interval = c(0.05, 0.5)) {
  if (isTRUE(attr(desired_bdp, 'fixed', TRUE))) {
    return(desired_bdp)
  }

  numeric_tol <- sqrt(.Machine$double.eps)

  from <- min(max(desired_bdp - tolerance, interval[[1L]]),
              interval[[2L]])
  to <- max(min(desired_bdp + tolerance, interval[[2L]]),
            interval[[1L]])
  bdp_range <- seq(from, to, by = precision)

  # Filter bdp's where the 1st derivative is unbounded
  bdp_range <- bdp_range[abs(bdp_range * n - floor(bdp_range * n)) > numeric_tol]

  # Determine an upper bound for the 1st derivative of the M-scale objective function
  first_deriv_bound <- vapply(bdp_range, FUN.VALUE = numeric(1L), FUN = function (bdp) {
    thresh <- tryCatch(uniroot(f = function (t) {
      up <- n * (1 - bdp) / (1 - t)
      up - floor(up) - n * t / (1 - t)
    }, interval = c(0, 0.5),  extendInt = 'downX', tol = numeric_tol)$root,
    error = function (e) {
      return(NA_real_)
    })

    1 / sqrt(1 - (1 - thresh)^(1/3))
  })
  good_bounds <- which(is.finite(first_deriv_bound))

  if (length(good_bounds) == 0L) {
    warn(paste("The chosen breakdown point may lead to numerical instability and",
               "excessive computation time.",
               "Consider changing the breakdown point via argument `bdp`."))
    return(desired_bdp)
  }
  bdp_range[[which.min(first_deriv_bound)]]
}

#' Approximate Value Matching
#'
#' @param x,table see [base::match] for details.
#' @param eps numerical tolerance for matching.
#' @return a vector the same length as `x` with integers giving the position in
#'         `table` of the first match if there is a match, or `NA_integer_`
#'         otherwise.
#' @keywords internal
.approx_match <- function(x, table, eps) {
  if (missing(eps)) {
    eps <- max(.Machine$double.eps, min(sqrt(.Machine$double.eps), 0.5 * min(x, table)))
  }
  .Call(C_approx_match, as.numeric(x), as.numeric(table), as.numeric(eps[[1L]]))
}

## Extract the given metric from all matching nodes (by name).
extract_metric <- function (metrics, attr, node) {
  matches <- c()
  if (!is.null(metrics[[attr]]) && isTRUE(metrics$name == node)) {
    matches <- c(matches, metrics[[attr]])
  }
  if (!is.null(metrics$sub_metrics)) {
    matches <- c(matches, unlist(lapply(metrics$sub_metrics, extract_metric,
                                        attr, node),
                                 use.names = FALSE, recursive = FALSE))
  }
  return (matches)
}

.recurisve_metrics_class <- function (metrics) {
  class(metrics) <- 'nsoptim_metrics'
  if (!is.null(metrics$sub_metrics)) {
    metrics$sub_metrics <- lapply(metrics$sub_metrics, .recurisve_metrics_class)
  }
  return(metrics)
}

.metrics_attrib <- function (estimates, metrics) {
  if (!is.null(metrics) && isTRUE(metrics$name != '')) {
    attr(estimates, 'metrics') <- .recurisve_metrics_class(metrics)
  }
  return(estimates)
}

#' Standardize data
#'
#' @param x predictor matrix. Can also be a list with components `x` and `y`,
#'    in which case `y` is ignored.
#' @param y response vector.
#' @param intercept is an intercept included (i.e., should `y` be centered?)
#' @param standardize standardize or not.
#' @param robust use robust standardization.
#' @param location_rho rho function for location estimate
#' @param cc cutoff value for the rho functions used in scale and location
#'  estimates.
#' @param ... passed on to `mlocscale()`.
#' @return a list with the following entries:
#' @importFrom Matrix drop
#' @importFrom methods is
#' @importFrom rlang abort
#' @importFrom stats sd
#' @keywords internal
.standardize_data <- function (x, y, intercept, standardize, robust, sparse,
                               mscale_opts, location_rho = 'bisquare', cc,
                               target_scale_x = NULL, ...) {
  if (is.list(x) && !is.null(x$x) && !is.null(x$y)) {
    y <- x$y
    x <- x$x
  }

  ret_list <- list(scale_x = rep.int(1, ncol(x)), mux = numeric(ncol(x)),
                   muy = 0, x = x, y = y)

  ## Center data for numerical convenience
  if (isTRUE(intercept)) {
    if (!isTRUE(robust)) {
      ret_list$mux <- colMeans(x)
      ret_list$muy <- mean(y)
    } else {
      ret_list$mux <- apply(x, 2, function (xj) {
        mlocscale(xj, location_rho = location_rho, location_cc = cc,
                  scale_cc = cc, opts = mscale_opts, ...)[['location']]
      })
      # Center the response using the S-estimate of regression for the
      # 0-slope.
      y_locscale <- mlocscale(y, location_rho = location_rho, location_cc = cc,
                              scale_cc = cc, opts = mscale_opts, ...)
      if (!isTRUE(y_locscale[['scale']] > .Machine$double.eps)) {
        abort("M-scale of response is 0.")
      }
      ret_list$muy <- y_locscale[['location']]
      if (!is.finite(ret_list$muy)) {
        # In case the response has more than 50% equal values.
        ret_list$muy <- 0
      }
    }

    ret_list$x <- sweep(x, 2L, ret_list$mux, FUN = `-`, check.margin = FALSE)
    ret_list$y <- y - ret_list$muy
  }

  ## Scale predictors
  if (isTRUE(standardize) || isTRUE(standardize == 'cv_only')) {
    ret_list$scale_x <- if (!isTRUE(robust)) {
      apply(ret_list$x, 2, sd)
    } else {
      locscale <- apply(ret_list$x, 2, function (xj) {
        mlocscale(xj, location_rho = location_rho, location_cc = cc,
                  scale_cc = cc, opts = mscale_opts, ...)
      })
      if (isTRUE(intercept)) {
        # Re-center the predictors with the updated centers
        ret_list$mux <- ret_list$mux + locscale[1L, ]
        ret_list$x <- sweep(x, 2L, ret_list$mux, FUN = `-`,
                            check.margin = FALSE)
      }
      locscale[2L, ]
    }

    if (!isTRUE(all(ret_list$scale_x > 0))) {
      abort(paste("Standardization failed. One or more variables in `x`",
                  "have a scale of 0."))
    }

    if (isTRUE(standardize)) {
      ret_list$x <- if (!is.null(target_scale_x)) {
        sweep(ret_list$x, 2L, target_scale_x / ret_list$scale_x, FUN = `*`,
              check.margin = FALSE)
      } else {
        sweep(ret_list$x, 2L, ret_list$scale_x, FUN = `/`, check.margin = FALSE)
      }
    }
  }

  # Set the target scale to 1, so that standardizing and unstandardizing works.
  if (is.null(target_scale_x)) {
    target_scale_x <- 1
  }

  ret_list$cv_standardize <- function(x, y) {
    if (is.list(x) && !is.null(x$x) && !is.null(x$y)) {
      y <- x$y
      x <- x$x
    }

    if (isTRUE(standardize == 'cv_only')) {
      # In case of "CV only" standardization, match the original scaling
      .standardize_data(x, y,
                        intercept = intercept,
                        standardize = TRUE,
                        robust = robust,
                        sparse = sparse,
                        location_rho = location_rho,
                        cc = cc,
                        mscale_opts = mscale_opts,
                        target_scale_x = ret_list$scale_x,
                        ... = ...)
    } else {
      .standardize_data(x, y,
                        intercept = intercept,
                        standardize = standardize,
                        robust = robust,
                        sparse = sparse,
                        location_rho = location_rho,
                        cc = cc,
                        mscale_opts = mscale_opts,
                        ... = ...)
    }
  }

  ret_list$standardize_coefs <- function(coef_obj) {
    if (is.null(coef_obj)) {
      return(NULL)
    }
    if (is.null(coef_obj$intercept)) {
      coef_obj$intercept <- 0
    }
    if (isTRUE(intercept)) {
      # Adjust intercept
      coef_obj$intercept <- coef_obj$intercept - ret_list$muy +
        sum(ret_list$mux * coef_obj$beta)
    }
    if (isTRUE(standardize)) {
      coef_obj$beta <- coef_obj$beta * (ret_list$scale_x / target_scale_x)
    }
    return(coef_obj)
  }

  ret_list$unstandardize_coefs <- if (isTRUE(sparse)) {
    function(coef_obj) {
      if (is.null(coef_obj)) {
        return(coef_obj)
      }
      if (is.null(coef_obj$intercept)) {
        coef_obj$intercept <- 0
      }

      coef_obj$std_beta <- coef_obj$beta
      coef_obj$std_intercept <- coef_obj$intercept

      if (isTRUE(standardize)) {
        coef_obj$beta@x <- coef_obj$beta@x * target_scale_x /
          ret_list$scale_x[coef_obj$beta@i]
      }
      if (isTRUE(intercept)) {
        # Recreate intercept
        coef_obj$intercept <- coef_obj$intercept + ret_list$muy -
          sum(ret_list$mux[coef_obj$beta@i] * coef_obj$beta@x)
      }
      return(coef_obj)
    }
  } else {
    function(coef_obj) {
      if (is.null(coef_obj)) {
        return(coef_obj)
      }
      if (is.null(coef_obj$intercept)) {
        coef_obj$intercept <- 0
      }

      coef_obj$std_beta <- coef_obj$beta
      coef_obj$std_intercept <- coef_obj$intercept

      if (isTRUE(standardize)) {
        coef_obj$beta <- coef_obj$beta * target_scale_x / ret_list$scale_x
      }
      if (isTRUE(intercept)) {
        # Recreate intercept
        coef_obj$intercept <- coef_obj$intercept + ret_list$muy -
          sum(ret_list$mux * coef_obj$beta)
      }
      return(coef_obj)
    }
  }

  return(ret_list)
}

## Validate the response type and return a list with elements `values` and `binary`.
#' @importFrom rlang warn abort
.validate_response <- function (y) {
  nlevels <- length(unique(y))
  if (is.factor(y)) {
    if (nlevels == 2L) {
      return(list(values = as.numeric(y) - 1, binary = TRUE))
    } else if (nlevels > 2L) {
      warn("`y` with more than 2 factor levels is implicitly treated as numeric.")
      return(list(values = as.numeric(y), binary = FALSE))
    }
  } else {
    y <- .as(y, 'numeric')
    if (nlevels == 2L) {
      warn(paste("`y` is interpreted as continuous response but has only 2 distinct values.",
                 "If binary classification is desired, coerce `y` to factor with `as.factor()`."))
    }
    if (nlevels > 1L) {
      return(list(values = y, binary = FALSE))
    }
  }
  abort("`y` must have at least 2 distinct values.")
}

## A wrapper around `methods::as` which raises an error if the conversion results in NA.
## @param ... passed on to [methods::as].
##
#' @importFrom methods as
#' @importFrom rlang abort
.as <- function (object, class, ...) {
  object_var <- deparse(substitute(object))
  tryCatch(methods::as(object, class, ...), warning = function (w) {
    abort(sprintf('`%s` is not of type `%s`', object_var, class))
  })
}

## Create a function which restores the original length of the coefficient vector
.restore_coef_length_fun <- function (positions, length) {
  if (length(positions) > 0L) {
    function (coef) {
      if (is(coef$beta, 'dsparseVector')) {
        coef$beta <- sparseVector(coef$beta@x, positions[coef$beta@i], length)
      } else {
        beta <- numeric(length)
        beta[positions] <- coef$beta
        coef$beta <- beta
      }
      return(coef)
    }
  } else {
    function (coef) {
      if (is(coef$beta, 'dsparseVector')) {
        coef$beta <- sparseVector(numeric(0L), integer(0L), length)
      } else {
        coef$beta <- numeric(length)
      }
      return(coef)
    }
  }
}

.prepare_penalty_loadings <- function (penalty_loadings, x, alpha, sparse,
                                       stop_all_infinite = FALSE) {
  orig_p <- ncol(x)
  restore_coef_length <- function (coef) coef

  if(any(alpha < .Machine$double.eps)) {
    abort("Non-empty `penalty_loadings` only supported for `alpha` > 0.")
  } else if (length(penalty_loadings) != orig_p) {
    abort("`penalty_loadings` has different number of elements than `x` columns.")
  }
  penalty_loadings <- .as(penalty_loadings, 'numeric')

  if (any(penalty_loadings < 0)) {
    abort("`penalty_loadings` must be positive.")
  }

  # Determine finite penalty loadings
  good_pl <- which(is.finite(penalty_loadings))
  if (length(good_pl) < orig_p) {
    # Some penalty loadings are infinite! Remove the corresponding predictors from `x`.
    x <- x[ , good_pl, drop = FALSE]
    penalty_loadings <- penalty_loadings[good_pl]
    restore_coef_length <- if (length(good_pl) > 0L) {
      if (isTRUE(sparse)) {
        function (coef) {
          if (!is.null(coef$std_beta)) {
            coef$std_beta <- sparseVector(coef$std_beta@x, good_pl[coef$std_beta@i], orig_p)
          }
          if (!is.null(coef$beta)) {
            coef$beta <- sparseVector(coef$beta@x, good_pl[coef$beta@i], orig_p)
          }
          return(coef)
        }
      } else {
        function (coef) {
          coef_vector <- numeric(orig_p)
          if (!is.null(coef$std_beta)) {
            coef_vector[good_pl] <- coef$std_beta
            coef$std_beta <- coef_vector
          }
          if (!is.null(coef$beta)) {
            coef_vector[good_pl] <- coef$beta
            coef$beta <- coef_vector
          }
          return(coef)
        }
      }
    } else {
      if (stop_all_infinite) {
        abort("At least one value in `penalty_loadings` must be finite.")
      }
      if (isTRUE(sparse)) {
        function (coef) {
          coef$std_beta <- sparseVector(numeric(0L), integer(0L), orig_p)
          coef$beta <- sparseVector(numeric(0L), integer(0L), orig_p)
          return(coef)
        }
      } else {
        function (coef) {
          coef$std_beta <- numeric(orig_p)
          coef$beta <- numeric(orig_p)
          return(coef)
        }
      }
    }
  }

  return(list(loadings = penalty_loadings, trimmed_x = x, restore_fun = restore_coef_length))
}


#' @importFrom methods is
.sparsify_other_starts <- function (other_starts, sparse) {
  lapply(other_starts, function (est) {
    if (isTRUE(sparse) && !is(est$beta, 'dsparseVector')) {
      est$beta <- sparseVector(as.numeric(est$beta), seq_along(est$beta), length(est$beta))
    } else if (!isTRUE(sparse) && !is.numeric(est$beta)) {
      est$beta <- .as(est$beta, 'numeric')
    }
    class(est) <- NULL
    return(est)
  })
}


#' @importFrom parallel clusterEvalQ clusterCall clusterApplyLB
#' @importFrom rlang abort
.make_cluster_handler <- function (par_cluster) {
  if (is.null(par_cluster)) {
    return(function (X, FUN, ..., x, fun) {
      lapply(X, FUN, ...)
    })
  } else {
    tryCatch({
      clusterEvalQ(par_cluster, {
        library(pense)
      })
    }, error = function (e) {
      abort(paste("`parallel` cluster cannot be used:", e))
    })

    return(function (X, FUN, ..., x, fun) {
      clusterApplyLB(par_cluster, x = X, fun = function (X, FUN, ...) {
        FUN(X, ...)
      }, FUN = FUN, ... = ...)
    })
  }
}

## Parse strings of the form *min*, *se*, or *{m}-se*.
#' @importFrom rlang abort
.parse_se_string <- function (x, only_fact = FALSE) {
  x <- .as(x[[1L]], 'character')
  xlen <- nchar(x)
  se_fact <- 1
  se_str <- if (identical('-se', substr(x, xlen - 2L, xlen))) {
    se_fact <- as.numeric(substr(x, 0L, xlen - 3L))
    if (anyNA(se_fact)) {
      abort(sprintf("Cannot parse standard error string '%s'.", x))
    }
    'se'
  } else {
    match.arg(x, c('min', 'se'))
  }
  if (identical(se_str, 'min')) {
    se_fact <- 0
  }

  if (isTRUE(only_fact)) {
    se_fact
  } else {
    list(se_type = se_str, se_fact = se_fact)
  }
}

## Filter a list to only include items with matching values.
.filter_list <- function (x, what, value, eps = sqrt(.Machine$double.eps),
                          comp_fun, ...) {
  comp_fun <- if (missing(comp_fun)) {
    function (v) { abs(v - value) < eps }
  } else {
    match.fun(comp_fun)
  }
  matches <- vapply(x, FUN.VALUE = logical(1L), FUN = function (el) {
    comp_fun(el[[what]], ...)
  })
  x[matches]
}
