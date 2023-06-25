# WKREBUILD2 FLR toolset

This repository contains a template analysis using FLR-based tools for the evaluation of recovery plans for ICES stocks.

## Installation

The required FLR packages

``` r
install.packages("icesTAF")

install.packages(icesTAF::deps(), repos=c(
  FLR="https://flr.r-universe.net",
  CRAN="https://cloud.r-project.org/"))
```

## Description

```
|-- bootstrap
|   |-- initial
|   |   `-- data
|   |       `-- sol274.rda
|   `-- DATA.bib
|-- bootstrap.R
|-- data.R
|-- model.R
|-- output.R
|-- report.R
|-- report.Rmd
`-- utilities.R
```

- bootstrap
- data.R
- model.R
- output.R
- report.R
- utilities.R

- data loading, OM conditioning and OEM setup (`data.R`).
- MP(s) specification, running and tuning, if required (`model.R`).
- Performance evaluation (`output.R`).
- Generating graphical outputs (`report.R`).

## How to create a repository using this template

## Suggested workflow

## Citation

## More information
