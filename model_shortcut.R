# model.R - Running mse with NS sol.27.40
# FLom/model.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

# SETUP parallel

library(doFuture)
plan("multicore", workers=2)
registerDoFuture()
options(Future.rng.onMisuse = "ignore")

# PROGRESS reporting
library(progressr)
handlers(global = TRUE)
handlers("progress")

# LOAD data
load('data/om.RData')

# SELECT performance statistics
data(statistics)

# SUBSET and propagate

om <- iter(om, 1:500)
oem <- iter(oem, 1:500)

# UPDATE intermediate year with Ftarget

om <- fwd(om, control=fwdControl(quant='fbar', year=2022, value=icespts$Fmsy))

# SET intermediate year, start of runs

mseargs <- list(iy=2022)

# --- RUN perfect.sa + hockeystick.hcr + tac.is with ICES refpts

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # hockeystick as ICES
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=c(icespts$Blim), trigger=c(icespts$Btrigger),
      target=c(icespts$Fmsy), min=0.001, metric="ssb", output="fbar")),
  # tac.is
  isys = mseCtrl(method=tac.is, args=list(recyrs=25, fmin=0.001))
))

system.time(
ices <- mp(om, oem=oem, ctrl=control, args=mseargs)
)

plot(om, ices)

# CHECK performance

performance(ices, years=list(2022:2030, 2022:2040))

# TRACK

tracking(ices)

# P(SB_2022<Blim)

sum(tracking(ices)['SB.om','2022'] < icespts$Blim) / 500
iterSums(tracking(ices)['SB.om',] / icespts$Blim < 1) / 500

# 

sum(tracking(ices)['SB.om','2022'] > icespts$Btrigger) / 500
sum(tracking(ices)['SB.est','2022'] > icespts$Btrigger) / 500

tracking(ices)

tracking(ices)['hcr',]


# --- RUN perfect.sa + hockeystick.hcr + tac.is with WKREBUILD setup

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # hockeystick as ICES
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=c(icespts$Blim), trigger=c(icespts$Btrigger),
      target=c(icespts$Fmsy), min=c(icespts$Fmsy) * 0.20,
      dlow=0.80, dupp=1.20, metric="ssb", output="fbar")),
  # tac.is
  isys = mseCtrl(method=tac.is, args=list(recyrs=25, fmin=0.001))
))

system.time(
rbld <- mp(om, oem=oem, ctrl=control, args=mseargs)
)


plot(om, ices, rbld)


# SAVE

save(ices, rbld, file="model/runs.RData", compress="xz")



# TODO: bias @deviances$stock.n

deviances(oem)$stk$stock.n <- deviances(oem)$stk$catch.n

icese <- mp(om, oem=oem, ctrl=control, args=mseargs)

plot(om, ices, icese)
plot(om, icese)

stock.n(stock(ices)) / stock.n(stock(icese))
stock.n(stock(ices)) / stock.n(stock(icese))

ssb(ices) / ssb(icese)


# TODO: ADD observations to plot
plot_hockeystick.hcr(control$hcr,
  labels=c(lim="Blim", trigger="Btrigger", min="", target="Ftarget")) +
  xlab("SSB") + ylab(expression(bar(F)))

plot_hockeystick.hcr(control$hcr, obs=stock,
  labels=c(lim="Blim", trigger="Btrigger", min="", target="Ftarget")) +
  xlab("SSB") + ylab(expression(bar(F)))

plot_hockeystick.hcr(control$hcr, obs=stock(ices), alpha=0.1,
  labels=c(lim="Blim", trigger="Btrigger", min="", target="Ftarget")) +
  xlab("SSB") + ylab(expression(bar(F)))






# --- cpue & catch + harvest





control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=catchSSB.hcr,
    args=list(MSY=14000))))



# --- APPLY CCSBT trend HCR

# control: perfect.sa + trend.hcr

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # CCSBT trend HCR
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1, k2=3, gamma=0.80, nyears=5, metric=ssb))
))

# RUN mp

trend <- mp(om, oem=oem, ctrl=control, args=mseargs)

# Same MP can be run every 3 years

trend3y <- mp(om, oem=oem, ctrl=control, args=list(iy=2020, frq=3))


# control: mlc + trend.hcr

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=len.ind, args=list(indicator=c("lbar", "lmax5" ),
    params=FLPar(linf=35, k=0.352, t0=-0.26), cv=0.2)),
  # CCSBT trend HCR
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1, k2=3, gamma=0.80, nyears=5, metric=ssb))
))

# RUN mp

ltrend <- mp(om, oem=oem, ctrl=control, args=mseargs)


# --- RUN mean length indicator + target level HCR

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=len.ind, args=list(indicator="mlc",
    params=FLPar(linf=35, k=0.352, t0=-0.26), cv=0.2)),
  # CCSBT trend HCR
  hcr = mseCtrl(method=target.hcr,
    args=list(lim=15, target=20, metric="mlc"))
))

length <- mp(om, oem=oem, ctrl=control, args=mseargs)


# --- APPLY ICES HCR and TAC short-term forecast

# control: perfect.sa + ices.hcr + tac.is

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=perfect.sa),
  # ICES HCR
  hcr = mseCtrl(method=ices.hcr,
    args=list(ftrg=c(refpts(om)$Fmsy) * 0.5, sblim=c(refpts(om)$SBlim),
      sbsafe=c(refpts(om)$Bpa))),
  # One-year forecast for TAC
  isys=mseCtrl(method=tac.is, args=list(dtaclow=0.85, dtacupp=1.15, recyrs=30))
  ))

# RUN mp

ices <- mp(om, oem=oem, ctrl=control, args=mseargs)


# --- TUNE MP for 60% P(SB = SBMSY) over years 2030:2040

# TODO: EXAMPLE on performance and probability

tun <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets, statistic=statistics["PSBMSY"], years=2030:2040,
  tune=list(sblim=c(10000, 50000)), prob=0.6, tol=0.01, maxit=12)


# --- ASSEMBLE MP runs

runs <- list(TREND=trend, TREND2Y=trend3y, LEN=length, ICES=ices)

plot(window(om, start=2005), runs)

save(runs, file="model/runs.Rdata", compress="xz")
