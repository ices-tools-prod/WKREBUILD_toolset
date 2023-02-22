# model.R - Running mse with NS sol.27.40
# WKREBUILD_toolset/model.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

source("utilities.R")

# SETUP parallel

library(doParallel)
registerDoParallel(3)

# LOAD oem and oem

load('data/sol274.RData')

# SET intermediate year, start of runs

mseargs <- list(iy=2022)

# F and SSB deviances

sdevs <- shortcut_devs(om, Fcv=0.212, Fphi=0.423)

# - RUN for F=0

runf0 <- fwd(om, control=fwdControl(year=2023:2042, quant="fbar", value=0))

# SETUP ICES advice rule

arule <- icesControl(SSBdevs=sdevs$SSB, Fdevs=sdevs$F,
  Btrigger=42838, Blim=0, Ftarget=0.207, Fmin=0, recyrs=-2)

# - RUN ICES advice rule, 4 min over 3 cores

system.time(
run <- mp(om, oem=oem, ctrl=arule, args=mseargs)
)

# - RUN ICES advice rule for different slopes

runs <- mps(om, oem=oem, ctrl=arule, args=mseargs,
  hcr=list(lim=seq(0, 30828, length=6)))

# OR with different slopes and min Fs

runs_minfs <- mps(om, oem=oem, ctrl=arule, args=mseargs,
  hcr=list(lim=seq(0, 30828, length=6), min=seq(0, 0.05, length=6)))

# PLOT

plot(runf0, runs, window=FALSE)

# - SAVE

save(runf0, runs, file="model/runs.RData", compress="xz")
