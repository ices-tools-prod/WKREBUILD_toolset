# model.R - Running mse with NS sol.27.40
# WKREBUILD_toolset/model.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(icesTAF)
mkdir("model")

library(mse)

library(progressr)
handlers(global=TRUE)
handlers("txtprogressbar")

source("utilities.R")

# - SETUP

# USE parallel via doFuture

library(doFuture)
options(doFuture.rng.onMisuse="ignore")

# Linux
if(os.linux()) {
  plan(multicore, workers=4)
# Windows
} else {
  plan(multisession, workers=4)
}

registerDoFuture()

# LOAD oem and oem

load('data/data.rda')

# SET intermediate year, start of runs

mseargs <- list(iy=2022)

# F and SSB deviances

sdevs <- shortcut_devs(om, Fcv=0.212, Fphi=0.423, SSBcv=0.10)

# - RUN for F=0

runf0 <- fwd(om, control=fwdControl(year=2023:2042, quant="fbar", value=0))

# SETUP standard ICES advice rule

# (1) using icesControl

arule <- icesControl(SSBdevs=sdevs$SSB, Fdevs=sdevs$F,
  Btrigger=42838, Blim=0, Ftarget=0.207, Fmin=0, recyrs=-2)

# (2) or step by step

arule <- mpCtrl(list(

  # shortcut.sa + SSBdevs
  est = mseCtrl(method=shortcut.sa,
    args=list(SSBdevs=sdevs$SSB)),

  # hockeystick
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=0, trigger=42838, target=0.207, min=0,
    metric="ssb", output="fbar")),

  # tac.is + Fdevs
  isys = mseCtrl(method=tac.is,
    args=list(recyrs=-2, fmin=0, Fdevs=sdevs$F))
  ))

# - RUN ICES advice rule, change slopes and min Fs (AR_Steep + F below Blim)

runs <- mps(om, oem=oem, ctrl=arule, args=mseargs,
  hcr=combinations(lim=seq(0, c(refpts(om)$Btrigger), length=5),
    min=seq(0, 0.10, length=4)))

runs <- mps(om, oem=oem, ctrl=arule, args=mseargs,
  hcr=list(
    lim=seq(0, c(refpts(om)$Btrigger), length=5),
    min=seq(0, 0.20, length=5)))


# SAVE

save(runf0, runs, file="model/model_runs.rda", compress="xz")
