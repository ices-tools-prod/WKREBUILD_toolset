# WKREBUILD2 FLR toolset

This repository contains a template analysis using FLR-based tools for the evaluation of recovery plans for ICES stocks.

## Installation

The required [FLR](https:://flr-project.org) and R packages can be installed in the standard R library by calling:

``` r
install.packages("icesTAF")

install.packages(icesTAF::deps(), repos=c(
  FLR="https://flr.r-universe.dev",
  CRAN="https://cloud.r-project.org/"))
```

or from the source code in the [FLR github repositories](https://github.com/flr)

```r
remotes::install_github(paste0("flr/", c("FLCore", ""ggplotFL", "FLFishery", "FLasher", "FLSRTMB", "mse", "mseviz")))
```

To setup and use a project library using the tools provided by the icesTAF package, the `bootstrap/SOFTWARE.bib` file can be created by calling

```r
icesTAF::draft.software(c("FLCore", ""ggplotFL", "FLFishery", "FLasher", "FLSRTMB", "mse", "mseviz"))
```

This will allow top start having a record of what version of the packages has been used.

An example SOFTWARE.bib file is present as `bootstrap/_SOFTWARE.bib`, but links to the most recent version of the packages, rather than an individual one. Rename to `SOFTWARE.bib` correct name to use it in `taf.boot()`.

When using the TAF package library, calls to `library(mse)` in the repository scripts should be substituted by a series of calls to the FLR packages following the dependency list

```r
taf.library(FLCore)
taf.library(ggplotFL)
taf.library(FLFishery)
taf.library(FLasher)
taf.library(FLSRTMB)
taf.library(mse)
taf.library(mseviz)
```

If packages are updated while working on the analysis, remember to update the relevant entry in `SOFTWARE.bib`.


```r
library(progressr)
handlers(global=TRUE)
handlers("txtprogressbar")
```

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
- Adapt the contents of `DATA.bib` file to match the stock and data file.
- Call `icesTAF::taf.boot()` to get data files places in `bootstrap/data`.
- Inspect and adapt each R script file.

![](https://user-images.githubusercontent.com/1029847/249617706-e37724ad-f4a8-47d6-a481-c115cefd8b3b.png)

- `trove.R` is a non-TAF file that contains alternative formulations and commands for some of the steps in the analysis. If the choice shown the example dataset does not suit your case study, look if there is any other option for that particular step that might work better.

## Getting help

- Problems or questions about the toolset are better reported as an [issue in this repository](https://github.com/iagomosqueira/WKREBUILD_toolset/issues).
