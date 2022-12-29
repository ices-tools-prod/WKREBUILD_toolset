# model.R - DESC
# /home/mosquia/NEW/Active/mse_rebuild_ICES/WKREBUILD_toolset/model.R

# Copyright (c) WUR, 2022.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# SETUP parallel

library(doFuture)
plan("multicore", workers=2)
registerDoFuture()
options(Future.rng.onMisuse = "ignore")

# PROGRESS reporting
library(progressr)
handlers(global = TRUE)
handlers("progress")



