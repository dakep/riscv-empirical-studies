eval_cache <- function (expr, file, msg, envir = parent.frame()) {
  requireNamespace('cli')
  res <- tryCatch({
    if (file.exists(file)) {
      cli::cli_inform(c(i = "{msg} -- loading from cache"))
      readRDS(file)
    } else {
      NULL
    }
  }, error = \(e) {
    cli::cli_alert_warning(c(x = "Error loading cache file.",
                             " " = as.character(e)))
    NULL
  })

  if (is.null(res)) {
    cli::cli_inform(c(i = "{msg} -- computing"))
    res <- eval(expr, envir = envir)
    saveRDS(res, file = file)
  }
  res
}

replicated_cv_glmnet <- function (y, x, replications, ...) {
  requireNamespace('glmnet')
  if (!is.numeric(replications) ||
      length(replications) != 1L ||
      any(replications < 3)) {
    stop("Argument `replications` must be an integer greater than 2.")
  }

  first_fit <- glmnet::cv.glmnet(y = y, x = x,...)

  repl_cvm <- vapply(seq_len(replications - 1), FUN.VALUE = first_fit$cvm,
                     FUN = function (.) {
                       fit <- glmnet::cv.glmnet(y = y, x = x, ...)
                       fit$cvm
                     })

  first_fit$cvm <- rowMeans(cbind(repl_cvm, first_fit$cvm))
  first_fit$cvsd <- apply(repl_cvm, 1, sd)
  first_fit$cvup <- first_fit$cvm + first_fit$cvsd
  first_fit$cvlo <- first_fit$cvm - first_fit$cvsd

  sign <- if (identical(first_fit$name[[1]], "AUC")) {
    -1
  } else {
    +1
  }

  which_min <- which.min(sign * first_fit$cvm)
  first_fit$lambda.min <- first_fit$lambda[[which_min]]
  one_se_lambda <- with(first_fit,
                        which(lambda >= lambda.min &
                                sign * cvm <= max(sign * cvup[[which_min]],
                                                  sign * cvlo[[which_min]])))
  if (length(one_se_lambda) > 0) {
    first_fit$lambda.1se <- max(first_fit$lambda[one_se_lambda])
  } else {
    first_fit$lambda.1se <- max(first_fit$lambda)
  }
  first_fit
}
