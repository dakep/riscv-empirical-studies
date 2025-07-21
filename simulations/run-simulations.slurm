#!/bin/bash

#SBATCH --job-name RISCV
#SBATCH --partition=contrib
#SBATCH --cpus-per-task=4
#SBATCH --mem-per-cpu=2GB
#SBATCH --time=0-08:00
#SBATCH --output=simulations/logs/ris_cv_%a.log
#SBATCH --error=simulations/logs/ris_cv_%a.log

module load gnu12 r/4.4.0-ba

## Ensure OpenBLAS does not use multithreading
export OPENBLAS_NUM_THREADS=1

## Ensure OpenMP threads are "close"
export OMP_PROC_BIND="CLOSE"

echo "Running SLURM job ${SLURM_ARRAY_JOB_ID} (task ${SLURM_ARRAY_TASK_ID})"

export RENV_CONFIG_SYNCHRONIZED_CHECK="FALSE"

Rscript --no-restore --quiet --no-save simulations/simulations.R \
  --ncores ${SLURM_CPUS_PER_TASK} \
  --out simulations/results \
  --include-naive \
  --include-warmcold \
  --include-twostep \
  --include-sharedstart \
  ${SLURM_ARRAY_TASK_ID}
