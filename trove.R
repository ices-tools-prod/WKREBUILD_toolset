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


# --- output.R

