# data.R - CONDITIONS a series of OMs
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

load('bootstrap/data/sol274.RData')

# TODO: DEAL with multiple iters (bootstrap, McMC)

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

# - CONSTRUCT om

om <- FLom(stock=run, refpts=refpts, sr=srr, 
  projection=mseCtrl(method=fwd.om))

# SETUP om future

om <- propagate(fwdWindow(om, end=fy), it)

# SET future om deviances ...

# TODO: SET deviances(om, start=2022) <- foo()
# TODO: UNIFY rlnorm & ar1rlnorm interfaces

# 1. AS constant at 0.4
deviances(om)[, ac(2022:fy)] <- rlnorm(it, rec(om)[, ac(2022:fy)] %=% 0, 0.4)

# 2. OR at individual iter SD
deviances(om)[, ac(2022:fy)] <- rlnorm(it, 0,
  expand(sqrt(yearVars(residuals(sr(om)))), year=2022:fy))

# 3. OR with rho
deviances(om)[, ac(2022:fy)] <- ar1rlnorm(rho=0.04, years=2022:fy,
  iter=it, meanlog=0, sdlog=0.4)

# UPDATE intermediate year with Ftarget

om <- fwd(om,
  control=fwdControl(quant='fbar', year=2022, value=refpts$Fmsy))

# NOTE: Overfishing om for recovery ---\

deviances(om)[, ac(1958:2021)] <- exp(deviances(om)[, ac(1958:2021)])

om <- fwd(om, control=fwdControl(year=2010:2022, quant="fbar", value=0.65),
  deviances=deviances(om))

# ---/

#  - CONSTRUCT oem

oem <- FLoem(
  observations=list(stk=stock(om)),
  deviances=list(stk=FLQuants(catch.n=rlnorm(it, catch.n(om) %=% 0, 0.2))),
  method=perfect.oem
)

# - SAVE

save(om, oem, file="data/data.RData", compress="xz")
