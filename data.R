# data.R - CONDITION OM(s)
# WKREBUILD_toolset/data.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(icesTAF)
mkdir("data")

library(mse)
library(FLSRTMB)

source("utilities.R")

# - args

# FINAL year
fy <- 2042

# NUMBER iterations
it <- 500

# - LOAD AAP SA McMC results and retros, 2022 ICES WGNSSK sol.27.4

load('bootstrap/data/sol274.rda')

# - SRRs

# FIT 3 models

srrs <- FLSRs(lapply(setNames(list(bevholtSV, rickerSV, segreg),
  nm=c("bevholt","ricker","segreg")), function(x)
  srrTMB(as.FLSR(run, model=x), spr0=yearMeans(spr0y(run))))
)

# TODO: CALCULATE weighting

# COMBINE into a single object
prs <- Reduce(combine, lapply(seq(3), function(x)
  rbind(propagate(params(srrs[[x]]), 200), FLPar(m=rep(x, 200)))))

srr <- as.FLSR(run, model=mixedsrr()$model, params=iter(prs, seq(500)))

# UPDATE intermediate year with Ftarget
# TODO: NEED 2023 for rec in om, FIX fwd()

run <- fwd(fwdWindow(run, end=2023),
  sr=expand(yearMeans(rec(run)), year=2022:2023), 
  control=fwdControl(quant='fbar', year=2022:2023, value=refpts$Fmsy))

# - CONSTRUCT om

om <- FLom(stock=run, refpts=refpts, sr=srr)

# SETUP om future

om <- propagate(fwdWindow(om, end=fy), it)

# SET SR deviances
# TODO: SET deviances(om, start=2022) <- foo()

deviances(om)[, ac(2012:fy)] <- ar1rlnorm(rho=0.04, years=2012:fy,
  iter=it, meanlog=0, sdlog=0.2)

# RUN hindcast w/deviances
# NOTE: Overfishing om for recovery

om <- fwd(om, sr=rec(run)[, ac(2012:2023)] * 0.3,
  control=as(FLQuants(fbar=fbar(run)[, ac(2012:2022)] * 3), 'fwdControl'))

#  - CONSTRUCT oem
# TODO: SIMPLIFY deviances, FLoem(om, deviances=list())

oem <- FLoem(
  observations=list(stk=fwdWindow(run, end=fy)),
  deviances=list(stk=FLQuants(catch.n=rlnorm(it, catch.n(om) %=% 0, 0.2))),
  method=perfect.oem
)

# - SAVE

save(om, oem, file="data/data.rda", compress="xz")
