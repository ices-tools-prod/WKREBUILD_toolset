# model.R - Running rebuilding MPs
# WKREBUILD_toolset/model.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(icesTAF)
mkdir("model")

library(mse)

# CHOOSE number of cores for doFuture
cores <- 4

source("utilities.R")

# LOAD oem and oem
load('data/data.rda')

# SET intermediate year, start of runs
mseargs <- list(iy=2023)

# F and SSB deviances
# TODO: CALCULATE Fcv, Fphi
sdevs <- shortcut_devs(om, Fcv=0.212, Fphi=0.423, SSBcv=0.10)

# - RUN for F=0

runf0 <- fwd(om, control=fwdControl(year=2024:2042, quant="fbar", value=0))

# - RUN ICES advice rule

# SETUP standard ICES advice rule
arule <- mpCtrl(list(

  # (est)imation method: shortcut.sa + SSB deviances
  est = mseCtrl(method=shortcut.sa,
    args=list(SSBdevs=sdevs$SSB)),

  # hcr: hockeystick (fbar ~ ssb)
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=0, trigger=refpts(om)$Btrigger, target=refpts(om)$Fmsy,
    min=0, metric="ssb", output="fbar")),

  # (i)mplementation (sys)tem: tac.is (C ~ F) + F deviances
  isys = mseCtrl(method=tac.is,
    args=list(recyrs=-2, fmin=0, Fdevs=sdevs$F))
  ))

# RUN using advice rule
runar <- mp(om, ctrl=arule, args=mseargs)

# PLOT
plot(runf0, runar, window=FALSE)

# - RUN ICES advice rule, changing slopes and min Fs (AR_Steep + F below Blim)

runs <- mps(om, ctrl=arule, args=mseargs,
  hcr=combinations(lim=seq(0, c(refpts(om)$Btrigger) * 0.50, length=4),
    min=seq(0, 0.10, length=4)))

# SAVE
save(runf0, runs, file="model/model.rda", compress="xz")
