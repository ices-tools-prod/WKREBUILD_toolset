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

# CHOOSE number of cores for doFuture / doParallel
cores <- 3

source("utilities.R")

# LOAD AAP SA results, 2022 ICES WGNSSK sol.27.4
load('bootstrap/data/sol274.rda')

# INTERMEDIATE year
iy <- 2023

# DATA year
dy <- iy - 1

# FINAL year
fy <- 2042

# NUMBER of iterations
it <- 500

set.seed(987)

# - Stock-recruitment relationship(s) **

# FIT models
fits <- srrTMB(as.FLSRs(run, models=c("bevholt", "segreg")), 
  spr0=mean(spr0y(run)))

# PLOT
plotsrs(fits)

# BOOTSTRAP and SELECT model by largest logLik **
srpars <- bootstrapSR(run, iters=it,
  models=c("bevholt", "segreg"), method="best")

# SAVE
save(fits, srpars, file="data/bootstrap.rda", compress="xz")


# - CONSTRUCT OM

# GENERATE future deviances: lognormal autocorrelated **
srdevs <- rlnormar1(sdlog=srpars$sigmaR, rho=srpars$rho, years=seq(dy, fy))

plot(srdevs)

# BUILD FLom
om <- FLom(stock=propagate(run, it), refpts=refpts, model='mixedsrr',
  params=srpars, deviances=srdevs)

# SETUP om future: average of last 3 years **
om <- fwdWindow(om, end=fy)

# SET stochastic rec dy
rec(stock(om))[, '2022'] <- rec(om)[1, '2022'] * srdevs[, '2022']

# PROJECT forward for iy assumption
om <- fwd(om, catch=FLQuant(4289.2, dimnames=list(year=2023)))

# F and SSB deviances
sdevs <- shortcut_devs(om, Fcv=0.212, Fphi=0.423, SSBcv=0.10)


# - CONSTRUCT iem, implementation error module w/10% noise **

iem <- FLiem(method=noise.iem,
  args=list(noise=rlnorm(500, rec(om) %=% 0, 0.1)))


# - SAVE

save(om, iem, sdevs, file="data/data.rda", compress="xz")

plan(sequential)
