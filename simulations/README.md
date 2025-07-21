# Simulations

The R scripts and Quarto documents (R Markdown) in this folder reproduce the simulation results and the figures/tables in the manuscript.


## File Structure

* `simulations.R` runs a single replication of a given simulation task.
  This file must be run for each of the 18 simulation settings and 100 replications (i.e., tasks 01001—01100,02001—02100,…,18001—18100).
  The total time to finish all 1,800 tasks is about 7,200 hours when running on 4 CPUs.
  The R code applies N-CV and RIS-CV as well as all the variants considered in the manuscript and saves the results to disk.
* `run-simulations.slurm` runs the simulations on a high-performance compute cluster equipped with the SLURM scheduler.
* `results.qmd` combines all the 1800 result files created with `simulations.R` and produces the figures and tables reported in the manuscript and supplementary materials.


## Running the Simulation

To run a single replication of one of the 18 settings, the task ID must be specified as *{SETTING}{REPLICATION}*, where *{SETTING}* is a two-digit integer from 1--17, and *{REPLICATION}* is a three-digit integer between 1 and 100.
For instance, to re-create the results for the **25-th replication of the 1-st simulation setting** using 4 CPUs and saving the results under *results/*, the *simulations.R* script needs to be run with task ID *01025*:

```sh
# Command line (shell) code
Rscript simulations/simulations.R \
  --ncores 4 \
  --out simulations/results \
  --include-naive \
  --include-warmcold \
  --include-twostep \
  --include-sharedstart \
  01025
```

**Note: the R code must be run from the parent folder!**
If you run the code from within the *simulations/* folder, you will get an error like:

```
In file(filename, "r", encoding = encoding) :
  cannot open file 'simulations/simulation-settings.R': No such file or directory
```
