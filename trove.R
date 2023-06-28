# trove.R - DESC
# WKREBUILD_toolset/trove.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# --- data.R

# - BOOTSTRAP and SELECT model

# based on relative likelihood
srpars <- bootstrapSR(run, iters=it,
  models=c("bevholt", "segreg"), method="relative")

# based on proportion of log-likelihood
srpars <- bootstrapSR(run, iters=it,
  models=c("bevholt", "segreg"), method="loglik")

# - GENERATE future deviances


# - CONSTRUCT iem

# TAC overshoot truncated exponential (1 ~ 1.4)

overshoot <- FLQuant(1 - log(runif(it * 20, exp(1 - (1.4 - 1 + 20)), 1)) / 20,
  dimnames=list(year=seq(iy, fy), iter=seq(it)))

iem <- FLiem(method=noise.iem, args=list(noise=overshoot))

# If TAC_ay < TAC_ay-1, then F_ay = F_ay-1

iem <- FLiem(method=response.iem)


# --- model.R

# CONSTRUCT advice rule using icesControl

arule <- icesControl(SSBdevs=sdevs$SSB, Fdevs=sdevs$F,
  Btrigger=42838, Blim=0, Ftarget=0.207, Fmin=0, recyrs=-2)


# --- output.R
