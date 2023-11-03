# trove.R - DESC
# WKREBUILD_toolset/trove.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# SETUP progress bar

library(progressr)

handlers(global=TRUE)
handlers("txtprogressbar")


# --- data.R

fy <- 2042

# - Stock-recruitment relationship(s)

# SET future recruitments as random walk from last estimated year

sr(om) <- rwalk(window(rec(om), end=2022), end=fy, sd=0.02, delta=0)

# with negative drift

sr(om) <- rwalk(window(rec(om), end=2022), end=fy, sd=0.02, delta=-0.01)


# BOOTSTRAP and SELECT model

# based on relative likelihood
srpars <- bootstrapSR(run, iters=it,
  models=c("bevholt", "segreg"), method="relative")

# based on proportion of log-likelihood
srpars <- bootstrapSR(run, iters=it,
  models=c("bevholt", "segreg"), method="loglik")

# - CONSTRUCT FLom

# GENERATE future deviances

# SETUP om future: use mean of last 5 years but resample 'wt' slots

om <- fwdWindow(window(om, end=2023), end=fy, nsq=5, fun=c("mean", wt="sample"))

# resample 10 years for wt and mat, 5 for catch.sel

om <- fwdWindow(om, end=fy, nsq=3,
  fun=c(wt='sample', mat='sample', catch.sel='sample'),
  years=c(wt=10, mat=10, catch.sel=10))

# - CONSTRUCT iem

# TAC overshoot truncated exponential (1 ~ 1.4)

overshoot <- FLQuant(1 - log(runif(it * 20, exp(1 - (1.4 - 1 + 20)), 1)) / 20,
  dimnames=list(year=seq(iy, fy), iter=seq(it)))

iem <- FLiem(method=noise.iem, args=list(noise=overshoot))

# If TAC_ay < TAC_ay-1, then F_ay = F_ay-1

iem <- FLiem(method=response.iem)

# - MULTIPLE OMs

oms <- list(
  # 3 year mean
  mean3y=fwdWindow(window(om, end=2023), end=fy, nsq=3, fun=c("mean")),
  # wts resampled over last 10 years
  sampwt=fwdWindow(window(om, end=2023), end=fy, nsq=3,
    fun=c("mean", wt="sample"), year=c(wt=10)))

library(future.apply)
plan(multicore, workers=2)

with_progress(
advice_runs <- future_lapply(oms, mp, iem=iem, ctrl=arule, args=mseargs,
  future.seed=NULL)
)

plan(sequential)

# --- model.R

#
rule <- mpCtrl(list(

  # (est)imation method: shortcut.sa + SSB deviances
  est = mseCtrl(method=shortcut.sa,
    args=list(SSBdevs=sdevs$SSB)),

  # hcr: hockeystick (fbar ~ ssb | lim, trigger, target, min)
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=8.34e+5, trigger=1.168e+6,
    target=0.075, min=0.015, metric="ssb", output="fbar")),

  # (i)mplementation (sys)tem: tac.is (C ~ F) + F deviances
  isys = mseCtrl(method=tac.is,
    args=list(recyrs=-2, fmin=0, Fdevs=sdevs$F))
  ))

plot_hockeystick.hcr(rule$hcr,
  labels=c(target=expression(F[target]), min=expression(0.2%.%F[target]),
    lim=expression(B[lim]), trigger=expression(B[trigger]))) +
  ylab("") + xlab("SSB")

# - EXAMPLE oem

oem <- FLoem(
  observations=list(stk=stock(om)),
  deviances=list(stk=FLQuants(stock.n=rlnorm(500, catch.n(om) %=% 0, 0.2))),
  method=shortcut.oem)

# RUN applying the OEM
error <- mp(om, oem=oem, ctrl=arule, args=mseargs)

# --- output.R

