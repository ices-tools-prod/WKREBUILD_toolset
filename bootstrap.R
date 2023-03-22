# bootstrap.R - Installs dependencies
# WKREBUILD_toolset/bootstrap.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(icesTAF)

# INSTALL from binary packages in r-universe

install.packages(
  c("FLCore", "FLFishery", "FLasher", "ggplotFL", "mse", "mseviz", "FLSRTMB"),
  repos=structure(c(CRAN="https://cloud.r-project.org/",
    FLR="https://flr.r-universe.dev")))
