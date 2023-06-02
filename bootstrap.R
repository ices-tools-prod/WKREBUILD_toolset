# bootstrap.R - Installs dependencies
# WKREBUILD_toolset/bootstrap.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# INSTALL from binary packages in r-universe and CRAN

install.packages(
  c("FLCore", "FLFishery", "FLasher", "ggplotFL", "mse", "mseviz", "FLSRTMB",
    "doFuture", "icesTAF"),
  repos=structure(c(CRAN="https://cloud.r-project.org/",
    FLR="https://flr.r-universe.dev")))

icesTAF::taf.bootstrap(software=FALSE)
