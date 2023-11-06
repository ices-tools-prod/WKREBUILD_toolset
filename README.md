
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

# Tutorial

More information on the example analysis can be found in the [tutorial](https://htmlpreview.github.io/?https://github.com/ices-tools-prod/WKREBUILD_toolset/blob/main/tutorial.html).

# Release of analysis results

A github release containing the results of running this code can be downloaded [here](https://github.com/ices-tools-prod/WKREBUILD_toolset/releases/tag/results_20231106).
