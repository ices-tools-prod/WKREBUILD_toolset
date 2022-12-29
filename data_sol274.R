# data.R - CONDITION a simple OM
# mseExamples/single_stock-sol.27.4/data.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)
library(FLBRP)
library(FLSRTMB)
library(AAP)

# --- LOAD data, inputs and results of 2022 ICES WGNSSK sol.27.4 SA

load('bootstrap/data/sol274.RData')

# RUN McMC fit

control <- AAP.control(pGrp=TRUE, qplat.surveys=8, qplat.Fmatrix=9,
  Sage.knots=6, Fage.knots=8, Ftime.knots=28, mcmc=TRUE)

system.time(
  mcfit <- aap(stock, indices, control=control, verbose=TRUE)
)

save(mcfit, file="data/mcfit.RData", compress="xz")

# --- SETUP om & oem

load('data/mcfit.RData')

# FINAL year
fy <- 2042

# ASSEMBLE McMC FLStock
runmc <- stock + mcfit

# - FIT SRR

srr <- as.FLSR(runmc, model="bevholtSV")

system.time(
srr <- Reduce(combine, lapply(seq(dims(srr)$iter), function(i)
  srrTMB(iter(sr, i), spr0=spr0y(stock))))
)

# - GET OM biological refpts

brp <-  brp(FLBRP(runmc, sr=srr))

# EXTEND srr into future
srr <- window(srr, end=fy)

# RESAMPLE last 21 years of residuals
residuals(srr)[, ac(2022:fy)] <- exp(residuals(srr)[, sample(ac(2000:2021),
  21)])

# RESHAPE and subset refpts
refpts <- remap(refpts(brp))

# CONSTRUCT om
om <- FLom(stock=fwdWindow(runmc, end=fy), refpts=refpts, sr=srr,
  projection=mseCtrl(method=fwd.om))

# - CONSTRUCT oem
it <- 1000

# observations: stk, idx, lens
index.q(indices$BTS)[] <- q.hat(mcfit)$BTS
index.q(indices$SNS)[] <- q.hat(mcfit)$SNS[1:6,]

obs <- list(stk=fwdWindow(runmc, end=fy),
    # TODO: propagate(FLlst)
    idx=lapply(fwdWindow(indices, end=fy), propagate, it))

# length samples
vbpars <- FLPar(linf=38.4, k=0.306, t0=-1.70)
ialk <- invALK(vbpars, age=1:10, cv=0.15, lmax=1.25)
lsam <- lenSamples(window(catch.n(obs$stk), end=2021), ialk)
obs$len <- window(lsam, end=2042)

# deviances
devs <- list(stk=list(
    catch.n=rlnorm(it, window(catch.n(runmc), end=fy) %=% 0, 0.15)),
  idx=lapply(obs$idx, function(x) rlnorm(it,
    window(index.q(x), end=fy) %=% 0, 0.20))
  #, lens=lsam %=% 0
)

oem <- FLoem(observations=obs, deviances=devs, method=sampling.oem)

# TODO: verify(om, oem)


# SAVE

save(om, oem, icespts, file="data/om.RData", compress="xz")
