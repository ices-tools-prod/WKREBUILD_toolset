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

cores <- 4

source("utilities.R")

# - args

# INTERMEDIATE year
iy <- 2023

# DATA year
dy <- iy - 1

# FINAL year
fy <- 2042

# NUMBER of iterations
it <- 500

# - LOAD AAP SA results, 2022 ICES WGNSSK sol.27.4

load('bootstrap/data/sol274.rda')

# - SRRs

# FIT models

fits <- srrTMB(as.FLSRs(run, models=c("bevholt", "ricker", "segreg")), 
  spr0=mean(spr0y(run)))

# INSPECT fits

plotsrs(fits)

# BOOTSTRAP fits of chosen models

srpars <- bootstrapSR(window(run, end=iy - 1), iters=it,
  models=c("bevholt", "segreg"))

srpars <- bootstrapSR(window(run, end=iy - 1), iters=it,
  models=c("bevholt", "segreg"), method="rejection")

plot_bootstrapSR(fits, srpars)

# GENERATE deviances

srdevs <- nar1rlnorm(sdlog=srpars$sigmaR, rho=srpars$rho, years=seq(iy, fy))

# - CONSTRUCT om

srr <- as.FLSR(run, model=mixedsrr, params=srpars)

om <- FLom(stock=run, refpts=refpts, sr=srr)

# SETUP om future

om <- propagate(fwdWindow(om, end=fy), it)

# TODO: SET deviances(om, start=2012) <- foo()
deviances(om)[, ac(2012:fy)] <- srdevs

# RUN hindcast w/deviances
# TODO: AVOID need for rec 2024
om <- fwd(om, sr=append(rec(run)[, ac(2012:2023)], 1),
  control=as(FLQuants(catch=catch(run)[, ac(2012:2023)]), 'fwdControl'))
plot(run, stock(om))

# COMPARE sigma(ssb)

#  - CONSTRUCT oem
# TODO: SIMPLIFY deviances, FLoem(om, deviances=list())

oem <- FLoem(
  observations=list(stk=fwdWindow(run, end=fy)),
  deviances=list(stk=FLQuants(catch.n=rlnorm(it, catch.n(om) %=% 0, 0.2))),
  method=perfect.oem
)

# - SAVE

save(om, oem, file="data/data.rda", compress="xz")
