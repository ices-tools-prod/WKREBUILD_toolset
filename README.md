# WKREBUILD2 FLR toolset

This repository contains a template for an analysis using FLR-based tools for the evaluation of recovery plans for ICES stocks.

## Installation

The required [FLR](https:://flr-project.org) packages, and all their dependencies, can be installed in the standard R library by calling:

``` r
install.packages("icesTAF")

install.packages(icesTAF::deps(), repos=c(
  FLR="https://flr.r-universe.dev",
  CRAN="https://cloud.r-project.org/"))
```

They can also be installed from the source code in the [FLR github repositories](https://github.com/flr) by calling

```r
remotes::install_github(paste0("flr/", c("FLCore", ""ggplotFL", "FLFishery", "FLasher", "FLSRTMB", "mse", "mseviz")))
```

## Running the code

Once the repository has been cloned, only two steps are necessary to test run the example code, once `icesTAF` has been loaded:

```r
library(icesTAF)

# RUN the repository bootstrap steps (data)
taf.bootstrap()

# SOURCE all R scripts in order
make.all()
```

### Progress reporting

The [mse](https://flr-project.org/mse) package allows setting up notifications of process on long computations through [progressr](https://progressr.futureverse.org/). You can request a progress bar to be shown for the calls to the `mp()` and `mps()` functions in this way

```r
library(progressr)
handlers(global=TRUE)
handlers("progress")
```

### Parallelization

Calls to various functions in the [mse](https://flr-project.org/mse) package (`mp` and `mps`) can be speeded-up through parallelization. For a single `mp()` call, iterations are split in blocks across the number of cores available. In the case of `mps()`, each procedure is executed in a separate process across the available cores.

If the `cores` variable is defined in `data.R` or 'model.R', the call to [plan()](https://future.futureverse.org/reference/plan.html) in `utilities.R` will set up a 'multisession' plan. Please refer to the `plan` help page for further information.

The number of cores to use should be chosen by considering the available memory. Performance will be poor if too many cores are trying to use too little memory.

## Setup a full TAF repository

To setup and use a project library using the tools provided by the icesTAF package, the `bootstrap/SOFTWARE.bib` file can be created by calling

```r
icesTAF::draft.software(c("FLCore", ""ggplotFL", "FLFishery", "FLasher", "FLSRTMB", "mse", "mseviz"))
```

This will allow top start having a record of what version of the packages has been used.

An example SOFTWARE.bib file is present as `bootstrap/_SOFTWARE.bib`, but links to the most recent version of the packages, rather than an individual one.

When using the TAF package library, calls to `library(mse)` in the repository scripts should be substituted by a call to

```r
taf.libraries()
```

If packages are updated while working on the analysis, remember to update the relevant entry in `SOFTWARE.bib`.

## How to create a repository using this template

A new repository can be created using this one as a template by [following a series of steps](https://docs.github.com/en/repositories/creating-and-managing-repositories/creating-a-repository-from-a-template). The repository will contain a series of R scripts following the [TAF](https://www.ices.dk/data/assessment-tools/Pages/transparent-assessment-framework.aspx) format, setup for analysing recovery plans for a single stock.

## Repository description

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

## Setting up for a new stock

- Place a file, or files, containing the stock assessment results and reference points in `bootstrap/initial/data`.

  - rda, refpts

- Adapt the contents of `DATA.bib` file to match the stock and data file.
- Call `icesTAF::taf.boot()` to get data files places in `bootstrap/data`.
- Inspect and adapt each R script file.

![](https://user-images.githubusercontent.com/1029847/249617706-e37724ad-f4a8-47d6-a481-c115cefd8b3b.png)

- `trove.R` is a non-TAF file that contains alternative formulations and commands for some of the steps in the analysis. If the choice shown the example dataset does not suit your case study, look if there is any other option for that particular step that might work better.

## Getting help

- Problems or questions about the toolset are better reported as an [issue in this repository](https://github.com/iagomosqueira/WKREBUILD_toolset/issues).

- To inspect the code of S4 methods defined for the FLR classes, the following sequence could be used

```r
# WHAT gets called when we run (data.R:67)?
om <- fwdWindow(om, end=fy)

# TYPE a function name to see if it is a method
fwdWindow

# SHOW all methods defined for this function
showMethods('fwdWindow')

# CHECK classes of input, y is 'missing'
is(om)

# GET the method for the required class signature
getMethod('fwdWindow', c('FLom', 'missing'))

```

- To debug S4 methods, a call to trace with the following arguments, works as a call to `debug()` on a function

```
# TO DEBUG fwdWindow for 'FLom'
trace("fwdWindow", browser, exit=browser, signature=c('FLom', 'missing'))

# TO STOP debugging
untrace("fwdWindow", signature=c('FLom', 'missing'))
```
