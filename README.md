# RIS-CV

These scripts and markdown files reproduce the figures/tables for the applications in the manuscript.

## File Structure

* `application_I-cav.qmd` re-creates all figures and results for the biomarker discovery study (Section 4).
  This takes approximately 30 minutes when using 3 CPU cores.
  The file contains a setup code block where the number of CPU cores can be changed.
* `application_II-gene_pathway.qmd` re-creates all figures and results for the gene pathway analysis (Section S2.1 of the supplementary materials).
  This takes approximately 3 hours when using 3 CPU cores.
* `application_III-plasma_retinol.qmd` re-creates all figures and results for the Plasma Retinol application (Section S2.2 of the supplementary materials).
  This takes approximately 20 minutes when using 3 CPU cores.
* `90-compute-utils.R` contains helper functions to compute the classical LS-EN estimator and cache results.
* `91-plotting-utils.R` contains common helper functions for plotting.
* `data/` contains all the data sets for the applications.
* `pense/` contains the R package implementing penalized S- and M-estimators for linear regression with RIS-CV.
* `simulations/` contains the code to re-create the results from the simulation study.
  See [simulations/README.md](simulations/README.md) for instructions on how to reproduce those results.


## Setup

### Manual Setup

To run the R code it is necessary to install the following R packages from CRAN:

```r
# R code
install.packages(c("tidyverse",
                   "argparser",
                   "glmnet",
                   "rmarkdown",
                   "knitr",
                   "yaml",
                   "cli",
                   "kableExtra",
                   "gridExtra",
                   "here",
                   "mvnfast",
                   "RcppArmadillo",
                   "robustbase",
                   "stabledist",
                   "remotes"))
```

Afterwards, the **pense** package from the `pense/` folder must be installed.
A working C++17 toolchain is required to install this R package:

```r
# R code
remotes::install_local("./pense")
```

### Alternative: `renv`

Alternatively, the project comes with an [renv](https://rstudio.github.io/renv/) lock file to re-create the exact environment used to compute the results presented in the paper.
To activate this environment, run the following R code in an R session with the working directory set to this project:

```r
# R code
source("renv/activate.R")

renv::activate()
```

After restarting the R session, run

```r
renv::restore()
```

to install the required packages.

## Compiling Documents

To compile the *.qmd* documents for the three applications from the command line, the [Quarto utility](https://quarto.org/) must be installed.
Please see their [Get Started](https://quarto.org/docs/get-started/) guide on installing Quarto.
When Quarto is installed and on the search path, the *.qmd* document can be compiled as follows:

```sh
# Command line (shell) code
quarto render application_I-cav.qmd --to html
```
