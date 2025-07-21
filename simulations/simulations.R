suppressPackageStartupMessages({
  library(argparser)
  library(pense)
})

COMPATIBILITY_VERSION <- Sys.getenv("CODE_COMPATIBILITY_VERSION", unset = '')

source("simulations/simulation-settings.R")

git_hash <- tryCatch(system2('git', args = c('rev-parse', 'HEAD'),
                             stdout = TRUE, stderr = FALSE) |>
                       substr(start = 1, stop = 6),
                     error = \(cnd) 'unknown')

args <- arg_parser(sprintf('Run simulation (commit %s).', git_hash)) |>
  add_argument('task', type = 'integer',
               help = paste('Task number XXYYY to determine simulation setting (XX) and',
                            'replication (YYY).')) |>
  add_argument('--ncores', type = 'integer', default = 1L,
               help = 'Number of cores to use.') |>
  add_argument('--out', type = 'character', default = NA_character_,
               help = 'Output path for the results') |>
  add_argument('--force', flag = TRUE,
               help = paste('Force computing of this simulation task even if the',
                            'result file already exists.')) |>
  add_argument('--include-naive', flag = TRUE,
               help = paste('Include N-CV')) |>
  add_argument('--include-warmcold', flag = TRUE,
               help = paste('Include RIS-CV with warm & cold starts.')) |>
  add_argument('--include-twostep', flag = TRUE,
               help = paste('Include N-CV with a two-step approximation to the CV fits')) |>
  add_argument('--include-sharedstart', flag = TRUE,
               help = paste('Include N-CV with a shared starting points')) |>
  parse_args()

source("simulations/generate_data.R")

print_log <- function (fmt, ..., file = stdout()) {
  cat(strftime(Sys.time(), '[%Y/%m/%d %H:%M:%S] '),
      sprintf(fmt, ...),
      '\n',
      sep = '',
      file = file)
}

if (any(is.na(args$out))) {
  stop("Argument `--out` needs to be specified")
}

args$out <- file.path(args$out, COMPATIBILITY_VERSION)

if (!dir.exists(args$out)) {
  dir.create(args$out, recursive = TRUE, mode = '0700')
}

## Determine seed and task settings  ###############################################################
task_setting_id <- args$task %/% MAX_NUM_REPLICATIONS
seed <- args$task %% MAX_NUM_REPLICATIONS
if (task_setting_id > nrow(TASK_SETTINGS)) {
  print_log("Setting %02d outside valid range.", task_setting_id, file = stderr())
  stop("Invalid task number")
}


print_log("Running simulation %s with setting %02d, using seed %03d. Saving results to path %s.",
          COMPATIBILITY_VERSION, task_setting_id, seed, args$out)

TASK_SETTING <- TASK_SETTINGS[task_setting_id, ]

## Generate data ###################################################################################

set.seed(seed)

comp_data <- generate_mixed_data(n = TASK_SETTING$n,
                                 p = TASK_SETTING$p,
                                 pve = TASK_SETTING$pve,
                                 resid_dist = TASK_SETTING$resid_dist,
                                 contamination_percentage = CONT_PERCENT,
                                 test_n = 10000)

## RIS-CV ##########################################################################################
output_file_ris <- file.path(args$out, 'ris',
                             sprintf('_ris_%03d-%03d.rds', task_setting_id, seed))

if (!file.exists(output_file_ris)) {
  if (!dir.exists(dirname(output_file_ris))) {
    dir.create(dirname(output_file_ris), mode = "0700", recursive = TRUE)
  }
  set.seed(seed)

  timing <- system.time(
    fit <- pense_cv(comp_data$x,
                    comp_data$y,
                    alpha = ALPHA,
                    nlambda = N_LAMBDA,
                    standardize = TRUE,
                    cv_k = N_CV_FOLDS,
                    cv_repl = CV_REPLICATIONS,
                    max_solutions = N_MAX_SOLUTIONS,
                    explore_it = PENSE_EXPLORE_IT,
                    lambda_min_ratio = 1e-2,
                    bdp = DESIRED_BDP,
                    ncores = args$ncores))

  fit$timinig <- timing

  # The saved PENSE object should contain the actual values for the non-data
  # arguments for easier replication.
  fit$call[-(1:3)] <- lapply(fit$call[-(1:3)], \(s) {
    eval(s, envir = parent.frame(n = 2))
  })

  fit$estimates <- lapply(fit$estimates, \(lambda_ests) {
    lapply(lambda_ests, \(est) {
      prediction_residuals <- comp_data$test_data$y - est$intercept -
        drop(comp_data$test_data$x %*% est$beta)

      est$pred_tau_size <- tau_size(prediction_residuals)
      est$pred_rmse <- sqrt(mean(prediction_residuals^2))
      est$pred_mape <- mean(abs(prediction_residuals))
      est$pred_medape <- median(abs(prediction_residuals))

      est$fit_residuals <- comp_data$y - est$intercept -
        drop(comp_data$x %*% est$beta)

      est$resid_tau_size <- tau_size(est$fit_residuals)

      est
    })
  })

  print_log("Finished RIS-CV for setting %02d, using seed %03d. Results saved to file %s.",
            task_setting_id, seed, output_file_ris)

  fit$code_compatibility_version <- COMPATIBILITY_VERSION
  fit$code_git_hash <- git_hash

  saveRDS(fit, output_file_ris)
} else {
  print_log("Skipping RIS-CV for setting %02d, using seed %03d. Results available in cache file %s.",
            task_setting_id, seed, output_file_ris)
}

## Naive CV ########################################################################################
if (isTRUE(args$include_naive)) {
  output_file_naive <- file.path(args$out, 'naive',
                                 sprintf('_naive_%03d-%03d.rds', task_setting_id, seed))

  if (!file.exists(output_file_naive)) {
    if (!dir.exists(dirname(output_file_naive))) {
      dir.create(dirname(output_file_naive), mode = "0700", recursive = TRUE)
    }
    set.seed(seed)

    timing <- system.time(
      fit <- pense_cv(comp_data$x,
                      comp_data$y,
                      alpha = ALPHA,
                      nlambda = N_LAMBDA,
                      standardize = TRUE,
                      cv_k = N_CV_FOLDS,
                      cv_repl = CV_REPLICATIONS,
                      max_solutions = 1,
                      cv_type = 'naive',
                      fold_starts = 'enpy',
                      lambda_min_ratio = 1e-2,
                      explore_it = PENSE_EXPLORE_IT,
                      explore_solutions = 10,
                      bdp = DESIRED_BDP,
                      ncores = args$ncores))

    fit$timing <- timing

    fit$call[-(1:3)] <- lapply(fit$call[-(1:3)], \(s) {
      eval(s, envir = parent.frame(n = 2))
    })

    fit$estimates <- lapply(fit$estimates, \(lambda_ests) {
      lapply(lambda_ests, \(est) {
        prediction_residuals <- comp_data$test_data$y - est$intercept -
          drop(comp_data$test_data$x %*% est$beta)

        est$pred_tau_size <- tau_size(prediction_residuals)
        est$pred_rmse <- sqrt(mean(prediction_residuals^2))
        est$pred_mape <- mean(abs(prediction_residuals))
        est$pred_medape <- median(abs(prediction_residuals))

        est$fit_residuals <- comp_data$y - est$intercept -
          drop(comp_data$x %*% est$beta)

        est$resid_tau_size <- tau_size(est$fit_residuals)

        est
      })
    })

    print_log("Finished naive CV for setting %02d, using seed %03d. Results saved to file %s.",
              task_setting_id, seed, output_file_naive)

    fit$code_compatibility_version <- COMPATIBILITY_VERSION
    fit$code_git_hash <- git_hash

    saveRDS(fit, output_file_naive)
  } else {
    print_log("Skipping naive CV for setting %02d, using seed %03d. Results available in cache file %s.",
              task_setting_id, seed, output_file_naive)
  }
}

## Shared start Naive CV ###########################################################################
if (isTRUE(args$include_sharedstart)) {
  output_file_sharedstart <- file.path(args$out, 'sharedstart',
                                       sprintf('_sharedstart_%03d-%03d.rds', task_setting_id, seed))

  if (!file.exists(output_file_sharedstart)) {
    if (!dir.exists(dirname(output_file_sharedstart))) {
      dir.create(dirname(output_file_sharedstart), mode = "0700", recursive = TRUE)
    }
    set.seed(seed)

    timing <- system.time(
      fit <- pense_cv(comp_data$x,
                      comp_data$y,
                      alpha = ALPHA,
                      nlambda = N_LAMBDA,
                      standardize = TRUE,
                      cv_k = N_CV_FOLDS,
                      cv_repl = CV_REPLICATIONS,
                      max_solutions = 1,
                      cv_type = 'naive',
                      fold_starts = 'full',
                      lambda_min_ratio = 1e-2,
                      explore_it = PENSE_EXPLORE_IT,
                      explore_solutions = 10,
                      bdp = DESIRED_BDP,
                      ncores = args$ncores))

    fit$timing <- timing

    fit$call[-(1:3)] <- lapply(fit$call[-(1:3)], \(s) {
      eval(s, envir = parent.frame(n = 2))
    })

    fit$estimates <- lapply(fit$estimates, \(lambda_ests) {
      lapply(lambda_ests, \(est) {
        prediction_residuals <- comp_data$test_data$y - est$intercept -
          drop(comp_data$test_data$x %*% est$beta)

        est$pred_tau_size <- tau_size(prediction_residuals)
        est$pred_rmse <- sqrt(mean(prediction_residuals^2))
        est$pred_mape <- mean(abs(prediction_residuals))
        est$pred_medape <- median(abs(prediction_residuals))

        est$fit_residuals <- comp_data$y - est$intercept -
          drop(comp_data$x %*% est$beta)

        est$resid_tau_size <- tau_size(est$fit_residuals)

        est
      })
    })

    print_log("Finished naive CV with shared starts for setting %02d, using seed %03d. Results saved to file %s.",
              task_setting_id, seed, output_file_sharedstart)

    fit$code_compatibility_version <- COMPATIBILITY_VERSION
    fit$code_git_hash <- git_hash

    saveRDS(fit, output_file_sharedstart)
  } else {
    print_log("Skipping naive-CV with shared starts for setting %02d, using seed %03d. Results available in cache file %s.",
              task_setting_id, seed, output_file_sharedstart)
  }
}

## Two-step CV fits ################################################################################
if (isTRUE(args$include_twostep)) {
  output_file_onecvit <- file.path(args$out, 'twostepcv',
                                   sprintf('_twostepcv_%03d-%03d.rds', task_setting_id, seed))

  if (!file.exists(output_file_onecvit)) {
    if (!dir.exists(dirname(output_file_onecvit))) {
      dir.create(dirname(output_file_onecvit), mode = "0700", recursive = TRUE)
    }
    set.seed(seed)

    timing <- system.time(
      fit <- pense_cv(comp_data$x,
                      comp_data$y,
                      alpha = ALPHA,
                      nlambda = N_LAMBDA,
                      standardize = TRUE,
                      cv_k = N_CV_FOLDS,
                      cv_repl = CV_REPLICATIONS,
                      max_solutions = 1,
                      cv_type = 'naive',
                      fold_starts = 'full',
                      lambda_min_ratio = 1e-2,
                      cv_algorithm_opts = mm_algorithm_options(max_it = 2),
                      explore_it = 0L,
                      explore_solutions = 10,
                      bdp = DESIRED_BDP,
                      ncores = args$ncores))

    fit$timing <- timing

    fit$call[-(1:3)] <- lapply(fit$call[-(1:3)], \(s) {
      eval(s, envir = parent.frame(n = 2))
    })

    fit$estimates <- lapply(fit$estimates, \(lambda_ests) {
      lapply(lambda_ests, \(est) {
        prediction_residuals <- comp_data$test_data$y - est$intercept -
          drop(comp_data$test_data$x %*% est$beta)

        est$pred_tau_size <- tau_size(prediction_residuals)
        est$pred_rmse <- sqrt(mean(prediction_residuals^2))
        est$pred_mape <- mean(abs(prediction_residuals))
        est$pred_medape <- median(abs(prediction_residuals))

        est$fit_residuals <- comp_data$y - est$intercept -
          drop(comp_data$x %*% est$beta)

        est$resid_tau_size <- tau_size(est$fit_residuals)

        est
      })
    })

    print_log("Finished naive CV with two-step CV for setting %02d, using seed %03d. Results saved to file %s.",
              task_setting_id, seed, output_file_onecvit)

    fit$code_compatibility_version <- COMPATIBILITY_VERSION
    fit$code_git_hash <- git_hash

    saveRDS(fit, output_file_onecvit)
  } else {
    print_log("Skipping naive-CV with two-step CV for setting %02d, using seed %03d. Results available in cache file %s.",
              task_setting_id, seed, output_file_onecvit)
  }
}

## RIS-CV with warm & cold starts ##################################################################
if (isTRUE(args$include_warmcold)) {
  output_file_ris_wc <- file.path(args$out, 'ris_warmcold',
                                  sprintf('_ris_warmcold_%03d-%03d.rds', task_setting_id, seed))

  if (!file.exists(output_file_ris_wc)) {
    if (!dir.exists(dirname(output_file_ris_wc))) {
      dir.create(dirname(output_file_ris_wc), mode = "0700", recursive = TRUE)
    }
    set.seed(seed)

    timing <- system.time(
      fit <- pense_cv(comp_data$x,
                      comp_data$y,
                      fold_starts = 'both',
                      alpha = ALPHA,
                      nlambda = N_LAMBDA,
                      standardize = TRUE,
                      cv_k = N_CV_FOLDS,
                      cv_repl = CV_REPLICATIONS,
                      max_solutions = N_MAX_SOLUTIONS,
                      explore_it = PENSE_EXPLORE_IT,
                      lambda_min_ratio = 1e-2,
                      bdp = DESIRED_BDP,
                      ncores = args$ncores))

    fit$timinig <- timing

    # The saved PENSE object should contain the actual values for the non-data
    # arguments for easier replication.
    fit$call[-(1:3)] <- lapply(fit$call[-(1:3)], \(s) {
      eval(s, envir = parent.frame(n = 2))
    })

    fit$estimates <- lapply(fit$estimates, \(lambda_ests) {
      lapply(lambda_ests, \(est) {
        prediction_residuals <- comp_data$test_data$y - est$intercept -
          drop(comp_data$test_data$x %*% est$beta)

        est$pred_tau_size <- tau_size(prediction_residuals)
        est$pred_rmse <- sqrt(mean(prediction_residuals^2))
        est$pred_mape <- mean(abs(prediction_residuals))
        est$pred_medape <- median(abs(prediction_residuals))

        est$fit_residuals <- comp_data$y - est$intercept -
          drop(comp_data$x %*% est$beta)

        est$resid_tau_size <- tau_size(est$fit_residuals)

        est
      })
    })

    print_log("Finished RIS-CV with warm & cold starts for setting %02d, using seed %03d. Results saved to file %s.",
              task_setting_id, seed, output_file_ris)

    fit$code_compatibility_version <- COMPATIBILITY_VERSION
    fit$code_git_hash <- git_hash

    saveRDS(fit, output_file_ris_wc)
  } else {
    print_log("Skipping RIS-CV with warm & cold starts for setting %02d, using seed %03d. Results available in cache file %s.",
              task_setting_id, seed, output_file_ris_wc)
  }
}
