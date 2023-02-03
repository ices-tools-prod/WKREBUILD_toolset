# data.R - CONDITION a simple OM
# mseExamples/single_stock-sol.27.4/data.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)
library(FLSRTMB)
library(AAP)

source("utilities.R")

# --- LOAD data, inputs and results of 2022 ICES WGNSSK sol.27.4 SA

load('bootstrap/data/sol274.RData')

# SETUP McMC control
aapcontrol <- AAP.control(pGrp=TRUE, qplat.surveys=8, qplat.Fmatrix=9,
  Sage.knots=6, Fage.knots=8, Ftime.knots=28, mcmc=TRUE)

# RUN McMC fit
system.time(
  mcfit <- aap(stock, indices, control=aapcontrol, verbose=TRUE)
)

aapcontrol <- AAP.control(pGrp=TRUE, qplat.surveys=8, qplat.Fmatrix=9,
  Sage.knots=6, Fage.knots=8, Ftime.knots=28, mcmc=FALSE)

# RUN retros

retros <- FLStocks(lapply(seq(2021, 2021 - 5), function(x)
  aap(window(stock, end=x), window(indices, end=x), control=aapcontrol) +
    window(stock, end=x)
))

# SAVE
save(mcfit, retros, file="data/mcfit.RData", compress="xz")


# --- SETUP om & oem

load('data/mcfit.RData')

# FINAL year
fy <- 2042

# ASSEMBLE McMC FLStock
runmc <- iter(stock + mcfit, seq(100))

# - FIT SRR

# segreg
srr <- fmle(as.FLSR(runmc, model="segreg"), fixed=list(b=icespts$Blim))

# bevholt
srr <- as.FLSR(runmc, model="bevholtSV")
srr <- Reduce(combine, lapply(seq(dims(srr)$iter), function(i)
  srrTMB(iter(srr, i), spr0=spr0y(stock))))

# - CONSTRUCT om
om <- FLom(stock=runmc, refpts=icespts, sr=srr,
  projection=mseCtrl(method=fwd.om))

# SETUP om future
om <- fwdWindow(om, end=fy)

# SET future om deviances: RESAMPLE last 21 years of residuals
residuals(sr(om))[, ac(2022:fy)] <- exp(residuals(srr)[, sample(ac(2000:2021),
  21)])

# UPDATE intermediate year with Ftarget

om <- fwd(om, control=fwdControl(quant='fbar', year=2022,
  value=icespts$Fmsy))


#  --- CONSTRUCT oem

oem <- FLoem(
  observations=list(stk=stock(om)),
  deviances=list(stk=FLQuants(stock.n=retroErrorByAge(retros, stock.n(om)))),
  method=perfect.oem
)

# --- CONSTRUCT SA deviances

sadevs <- FLQuants(stock.n=retroErrorByAge(retros, stock.n(om)))

# SAVE

save(om, oem, icespts, file="data/sol274.RData", compress="xz")


# --- CONSTRUCT oem for full feedback

it <- dim(om)[6]

# observations: stk, idx, lens
index.q(indices$BTS)[] <- q.hat(mcfit)$BTS
index.q(indices$SNS)[] <- q.hat(mcfit)$SNS[1:6,]

obs <- list(stk=fwdWindow(runmc, end=fy),
    # TODO: propagate(FLlst)
    idx=lapply(fwdWindow(indices, end=fy), propagate, it))

# deviances
devs <- list(stk=list(
    catch.n=rlnorm(it, window(catch.n(runmc), end=fy) %=% 0, 0.15),
    stock.n=rlnorm(1000, window(catch.n(runmc), end=fy) %=% 0,
  expand(yearMeans(log(sqrt(iterVars(stock.n(om)[, ac(2002:2021)])))),
  year=1957:2042))),
  idx=lapply(obs$idx, function(x) rlnorm(it,
    window(index.q(x), end=fy) %=% 0, 0.20))
)

# lognormal w/age-specific errors
oem <- FLoem(observations=obs, deviances=devs, method=sampling.oem)
