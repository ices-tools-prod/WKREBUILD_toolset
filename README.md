
# FLR toolset for evaluating recovery plans for ICES WKREBUILD2

- Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl

## Installation

The latest version of the required [FLR](https:://flr-project.org) packages, and all their dependencies, can be installed from the [FLR R-universe page](https://flr.r-universe.dev) by calling:

```r
install.packages("icesTAF")

install.packages(icesTAF::deps(), repos=c(
  FLR="https://flr.r-universe.dev",
  CRAN="https://cloud.r-project.org/"))
```

They can also be installed from the source code in the [FLR github repositories](https://github.com/flr) by calling

```r
remotes::install_github(paste0("flr/", c("FLCore", "ggplotFL", "FLFishery", "FLasher", "FLSRTMB", "mse", "mseviz")))
```

## Setup

### Progress reporting

The [mse](https://flr-project.org/mse) package allows setting up notifications of process on long computations through [progressr](https://progressr.futureverse.org/). You can request a progress bar to be shown for the calls to the `mp()`, `mps()` nd `bootstrapSR` functions in this way

```r
library(progressr)
handlers(global=TRUE)
handlers("progress")
```

### Parallelization

Calls to various functions in the [mse](https://flr-project.org/mse) package (`mp` and `mps`) can be speeded-up through parallelization. For a single `mp()` call, iterations are split in blocks across the number of cores available. In the case of `mps()`, each procedure is executed in a separate process across the available cores.

If the `cores` variable is defined in `data.R` or 'model.R', the call to [plan()](https://future.futureverse.org/reference/plan.html) in `utilities.R` will set up a 'multisession' plan. Please refer to the `plan` help page for further information.

The number of cores to use should be chosen by considering the available memory. Performance will be poor if too many cores are trying to use too little memory.

### Running the code

Once the repository has been cloned, only two steps are necessary to test run the example code, once `icesTAF` has been loaded:

```r
library(icesTAF)

# RUN the repository bootstrap steps (data)
taf.bootstrap()

# SOURCE all R scripts in order
make.all()
```

## TAF repository

### Repository description

A new repository should contain the following folder and tree structure, which includes only initial data files, the corresponding `DATA.bib` file, a BibTeX file with relevant bibliographic references, and the scripts that will run the analysis. These are set based on the example dataset, sol.27.4, and will need to be adapted for a different stock.

```
|-- bootstrap
|   |-- initial
|   |   `-- data
|   |       `-- sol274.rda  - Input dataset
|   |-- references.bib
|   `-- DATA.bib
|-- data.R -  Operating Model conditioning
|-- model.R - Evaluation of Management Procedures
|-- output.R - Quantification of performance, summary outputs
|-- report.R - Plotting
|-- report.Rmd
|-- trove.R - Collection of alternative
`-- utilities.R - Functions developed for this analysis
```

![](https://user-images.githubusercontent.com/1029847/249617706-e37724ad-f4a8-47d6-a481-c115cefd8b3b.png)

### bootstrap/initial

The essential inputs for the analysis include the result of the stock assessment model fit and the current set of reference points.

For example, `bootstrap/initial/data/sol274.rda` contains the following objects:

- `run`: The latest AAP stock assessment for sol.27.4 (WGNSSK 2023), as an object or class `FLStock`. The object does not reflect the WGNSSK 2023 sol.27.4 assessment, as it has been artificially depleted for demonstration purposes.

- `refpts`: An object of class `FLPar` containing the reference points, biological and operational, calculated for this stock in the last benchmark (ICES, 2020).

Uncertainty in past dynamics and initial conditions might have been already quantified, for example by employing the McMC sampling procedure available in the stock assessment model, by bootstrap of model inputs, or through a ensemble of model fits. If that is the case, the conditioning steps in `data.R` should be adapted accordingly. 

More information on the example analysis can be found in the [tutorial]().
