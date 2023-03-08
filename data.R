# data.R - CONDITIONS a series of OMs
# WKREBUILD_toolset/data.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)
library(FLSRTMB)
library(AAP)

source("utilities.R")

# - args

# FINAL year
fy <- 2042

# - LOAD AAP SA McMC results and retros, 2022 ICES WGNSSK sol.27.4

load('bootstrap/data/sol274.RData')

# - FIT SRRs

# segreg
sgrg <- fmle(as.FLSR(stock, model="segreg"), fixed=list(b=icespts$Blim))

srr <- sgrg

# - CONSTRUCT om

om <- FLom(stock=stock, refpts=icespts, sr=srr,
  projection=mseCtrl(method=fwd.om))

# SETUP om future

om <- fwdWindow(om, end=fy)


# SET future om deviances ...

# AS constant at 0.4
residuals(sr(om))[, ac(2022:fy)] <- rlnorm(500, rec(om) %=% 0, 0.4)

# OR at individual iter SD
deviances(om)[, ac(2022:fy)] <- rlnorm(500, 0,
  expand(sqrt(yearVars(residuals(sr(om)))), year=2022:fy))

# OR with rho
deviances(om)[, ac(2022:fy)] <- ar1rlnorm(rho=0.04, years=2022:fy,
  iter=500, meanlog=0, sdlog=0.4)

# UPDATE intermediate year with Ftarget

om <- fwd(om, control=fwdControl(quant='fbar', year=2022,
  value=icespts$Fmsy))

# NOTE: Overfishing om for recovery
deviances(om)[, ac(1958:2021)] <- exp(deviances(om)[, ac(1958:2021)])
om <- fwd(om, control=fwdControl(year=2010:2022, quant="fbar", value=0.65),
  deviances=deviances(om))


#  --- CONSTRUCT oem

oem <- FLoem(
  observations=list(stk=stock(om)),
  deviances=list(stk=FLQuants(catch.n=rlnorm(500, catch.n(om) %=% 0, 0.2))),
  method=perfect.oem
)

# SAVE

save(om, oem, file="data/sol274.RData", compress="xz")
