MAX_NUM_REPLICATIONS <- 1000

## Task grid
TASK_SETTINGS <- expand.grid(
  pve = 0.5,
  resid_dist = c('norm', 'laplace', 'stable(alpha=1.5, beta=0)'),
  n = c(100, 200),
  p = c(50, 100, 200),
  stringsAsFactors = FALSE
)

## Fixed settings
CV_REPLICATIONS <- 5
N_LAMBDA <- 50
N_CV_FOLDS <- 5
N_MAX_SOLUTIONS <- 40
PENSE_EXPLORE_IT <- 10
CONT_PERCENT <- 0.25
DESIRED_BDP <- 1/3
ALPHA <- 0.5
