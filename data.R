# data.R - condition OM(s)
# WKREBUILD_toolset/data.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(icesTAF)
mkdir("data")

library(mse)
library(FLSRTMB)

# CHOOSE number of cores for doFuture
cores <- 4

source("utilities.R")

# LOAD AAP SA results, 2022 ICES WGNSSK sol.27.4

load('bootstrap/data/sol274.rda')

# INTERMEDIATE year
iy <- 2022

# DATA year
dy <- iy - 1

# FINAL year
fy <- 2042

# NUMBER of iterations
it <- 500


# - SRRs

# FIT models
fits <- srrTMB(as.FLSRs(run, models=c("bevholt", "segreg")), 
  spr0=mean(spr0y(run)))

# PLOT
plotsrs(fits)

# BOOTSTRAP and SELECT model by logLik
srpars <- bootstrapSR(window(run, end=iy - 1), iters=it,
  models=c("bevholt", "segreg"), method="best")

# PLOT
plot_bootstrapSR(fits, srpars)

# SAVE
save(fits, srpars, file="data/bootstrap.rda", compress="xz")


# - CONSTRUCT om

# NOTE: overfishing stock
run <- fwd(run, sr=rec(run)[, ac(2013:2023)],
  fbar=fbar(run)[, ac(2013:2022)] * 1.6)

# GENERATE future deviances from bootstrap

srdevs <- rlnormar1(sdlog=srpars$sigmaR, rho=srpars$rho, years=seq(iy, fy))

plot(srdevs)

# OBS error 0.3

# BUILD FLom
om <- FLom(stock=propagate(run, 500), refpts=refpts, model='mixedsrr',
  params=srpars, deviances=srdevs)

# SETUP om future
om <- fwdWindow(om, end=fy)

# SET stochastic rec iy-1
stock.n(stock(om))[1,'2022'] <- rec(om)[,'2022'] * srdevs[,'2022']

# FWD for iy
om <- fwd(om, catch=catch(run)[,'2023'])

# SAVE
save(om, file="data/data.rda", compress="xz")
