# model.R - Running rebuilding MPs
# WKREBUILD_toolset/model.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(icesTAF)
mkdir("model")

library(mse)

# CHOOSE number of cores for doFuture / doParallel
cores <- 1

source("utilities.R")

# ACTIVATE progressr bar
handlers(global=TRUE)

# LOAD oem and oem
load('data/data.rda')

# - RUN for F=0

runf0 <- fwd(om, control=fwdControl(year=2024:2042, quant="fbar", value=0))

# SET intermediate year + start of runs, lags and frequency
mseargs <- list(iy=2023, fy=2042, data_lag=1, management_lag=1, frq=1)

# SETUP standard ICES advice rule
arule <- mpCtrl(list(

  # (est)imation method: shortcut.sa + SSB deviances
  est = mseCtrl(method=shortcut.sa,
    args=list(SSBdevs=sdevs$SSB)),

  # hcr: hockeystick (fbar ~ ssb | lim, trigger, target, min)
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=0, trigger=refpts(om)$Btrigger, target=refpts(om)$Fmsy,
    min=0, metric="ssb", output="fbar")),

  # (i)mplementation (sys)tem: tac.is (C ~ F) + F deviances
  isys = mseCtrl(method=tac.is,
    args=list(recyrs=-2, fmin=0, Fdevs=sdevs$F))
  ))

# plot HCR
plot_hockeystick.hcr(arule$hcr, labels=c(lim="Blim", trigger="MSYBtrigger",
  min="", target="Ftarget")) +
  xlab("SSB") + ylab(expression(bar(F)))

# - RUN applying ICES advice rule
system.time(
mse <- mp(om, iem=iem, ctrl=arule, args=mseargs)
)

# PLOT
plot(runf0, mse, window=FALSE)


# --- RUNS changing slope and min F (AR_Steep + F below Blim)

# CREATE combinations of lim(Blim) and min(Fmin) values.
opts <- combinations(
  lim=seq(0, c(refpts(om)$Btrigger) * 0.50, length=4),
  min=seq(0, 0.10, length=4))

# RUN for all options on 'hcr' control element
system.time(
mses_opts <- mps(om, ctrl=arule, args=mseargs, hcr=opts)
)


# --- RUNS with fleet response to TAC decrease, keeps effort at 90%

# SET fleet behaviour response to TAC, !F_y < 0.90 * F_y-1
fleetBehaviour(om) <- mseCtrl(method=response.fb, args=list(min=0.90))

# RUN for all options on 'hcr' control element
system.time(
mses_fr <- mps(om, ctrl=arule, args=mseargs, hcr=opts)
)

# SAVE
save(runf0, mses_opts, mses_fr, file="model/model.rda", compress="xz")
