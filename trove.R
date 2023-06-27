# trove.R - DESC
# WKREBUILD_toolset/trove.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# --- data.R

# - BOOTSTRAP and SELECT model

# based on relative likelihood
rel_srpars <- bootstrapSR(window(run, end=iy - 1), iters=it,
  models=c("bevholt", "segreg"), method="relative")

# based on proportion of log-likelihood
llk_srpars <- bootstrapSR(window(run, end=iy - 1), iters=it,
  models=c("bevholt", "segreg"), method="loglik")


# - GENERATE future deviances

# --- model.R
# (1) using icesControl

arule <- icesControl(SSBdevs=sdevs$SSB, Fdevs=sdevs$F,
  Btrigger=42838, Blim=0, Ftarget=0.207, Fmin=0, recyrs=-2)

# (2) or step by step



# --- output.R

