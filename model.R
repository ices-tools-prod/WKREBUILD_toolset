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

# ACTIVATE progressr bar
handlers(global=TRUE)

# LOAD oem and oem
load('data/data.rda')

# SET intermediate year, start of runs
mseargs <- list(iy=2023)

# - RUN for F=0

runf0 <- fwd(om, control=fwdControl(year=2024:2042, quant="fbar", value=0))

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

# - RUN applying ICES advice rule

run <- mp(om, iem=iem, ctrl=arule, args=mseargs)

# PLOT
plot(runf0, run, window=FALSE)

# --- RUNS changing slope and min F (AR_Steep + F below Blim)

# CREATE all combinations of lim(Blim) and min(Fmimn) values.
opts <- combinations(
  lim=seq(0, c(refpts(om)$Btrigger) * 0.50, length=4),
  min=seq(0, 0.10, length=4))

# RUN for all options on 'hcr' control element
runs <- mps(om, ctrl=arule, args=mseargs, hcr=opts)

# SAVE
save(runf0, runs, file="model/model.rda", compress="xz")


# --- RUNS with fleet response to TAC decrease, F > F_y-1 * min

fleetBehaviour(om) <- mseCtrl(method=response.fb, args=list(min=0.90))

run_fb <- mp(om, ctrl=arule, args=mseargs)

runs_fb <- mps(om, ctrl=arule, args=mseargs, hcr=opts)

# SAVE
save(runf0, runs, runs_fb, file="model/model.rda", compress="xz")
