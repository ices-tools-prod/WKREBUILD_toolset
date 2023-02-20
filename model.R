# model.R - Running mse with NS sol.27.40
# FLom/model.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

source("utilities.R")

# LOAD oem and oem

load('data/sol274.RData')

# NOTE: Overfish om

om <- fwd(om, control=fwdControl(year=2010:2022, quant="fbar", value=0.5))


# SET intermediate year, start of runs

mseargs <- list(iy=2022)

# F and SSB deviances

Fcv <- 0.212
Fphi <- 0.423
Fdevs <- ar1rlnorm(Fphi, dimnames(om)$year, dims(om)$iter, 0, Fcv)

SSBcv <- 0.10
SSBdevs <- ar1rlnorm(0, dimnames(om)$year, dims(om)$iter, 0, SSBcv)


# --- RUN for F=0

f0 <- fwd(om, control=fwdControl(year=2023:2042, quant="fbar", value=0))

# --- RUN for F=Fmin

fmin <- fwd(om, control=fwdControl(year=2023:2042, quant="fbar", value=0.01))


# --- RUN ICES adviced rule: shortcut.sa + hockeystick.hcr + tac.is

arule <- icesControl(SSBdevs, Fdevs, Btrigger=42838, Ftarget=0.207,
  Fmin=0.01, Blim=0, recyrs=30)

plot_hockeystick.hcr(arule$hcr)

system.time(
ices <- mp(om, oem=oem, ctrl=arule, args=mseargs)
)

plot(f0, ICESar=ices, window=FALSE)


# --- RUN with rebuild rule

rrule <- icesControl(SSBdevs, Fdevs, Btrigger=42838, Ftarget=0.207,
  Fmin=0.01, Blim=30828, recyrs=30)

system.time(
rebuild <- mp(om, oem=oem, ctrl=rrule, args=mseargs)
)

plot(f0, ICESar=ices, rebuild=rbld, window=FALSE)


# --- RUN for different slopes w/no min F

library(doParallel)
registerDoParallel(3)

system.time(
runs <- mps(om, oem=oem, ctrl=arule, args=mseargs,
  hcr=list(lim=seq(0, 30828, length=6)))
)

plot(f0, runs, window=FALSE)


# --- SAVE

save(f0, fmin, ices, rebuild, runs, file="model/runs.RData", compress="xz")
