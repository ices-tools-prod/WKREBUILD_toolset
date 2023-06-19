# data.R - CONDITION OM(s), adding initial uncertainty
# WKREBUILD_toolset/data.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(icesTAF)
mkdir("data")

library(mse)
library(FLSRTMB)

# SETUP progress bars

library(progressr)
handlers(global=TRUE)
handlers("txtprogressbar")

source("utilities.R")

# - args

# INTERMEDIATE year
iy <- 2023

# FINAL year
fy <- 2042

# NUMBER of iterations
it <- 500

# - LOAD AAP SA McMC results and retros, 2022 ICES WGNSSK sol.27.4

load('bootstrap/data/sol274.rda')

# - SRRs

# FIT 3 models up to 2022

srfits <- FLSRs(lapply(setNames(list(bevholtSV, rickerSV, segreg),
  nm=c("bevholt", "segreg")), function(x)
  srrTMB(as.FLSR(window(run, end=iy - 1), model=x), spr0=yearMeans(spr0y(run))))
)

# BOOTSTRAP

srpars <- bootstrapSR(window(run, end=iy - 1), iters=500,
  models=c("bevholt", "segreg"))

# GENERATE deviances

srdevs <- nar1rlnorm(sdlog=srpars$sigmaR, rho=srpars$rho, years=seq(2012, fy))

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
  control=as(FLQuants(fbar=fbar(run)[, ac(2012:2023)]), 'fwdControl'))

# COMPARE om ~ run

#  - CONSTRUCT oem
# TODO: SIMPLIFY deviances, FLoem(om, deviances=list())

oem <- FLoem(
  observations=list(stk=fwdWindow(run, end=fy)),
  deviances=list(stk=FLQuants(catch.n=rlnorm(it, catch.n(om) %=% 0, 0.2))),
  method=perfect.oem
)

bs <- brp(FLBRP(stock(om), sr=sr(om)))

# - SAVE

save(om, oem, file="data/data.rda", compress="xz")
