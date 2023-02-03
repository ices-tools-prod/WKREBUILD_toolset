# model.R - Running mse with NS sol.27.40
# FLom/model.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

# LOAD oem and oem

load('data/sol274.RData')

# SET intermediate year, start of runs

mseargs <- list(iy=2022)

# --- RUN perfect.sa + hockeystick.hcr + tac.is with ICES refpts

control <- mpCtrl(list(
  # perfect.sa
  est = mseCtrl(method=shortcut.sa, args=deviances(oem)$stk),
  # hockeystick as ICES
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=0, trigger=c(icespts$Btrigger),
      target=c(icespts$Fmsy), min=0.001, metric="ssb", output="fbar")),
  isys = mseCtrl(method=tac.is, args=list(recyrs=25, fmin=0.001))
))

plot_hockeystick.hcr(control$hcr,
  labels=c(lim="Blim", trigger="Btrigger", min="", target="Ftarget")) +
  xlab("SSB") + ylab(expression(bar(F)))

system.time(
  ices <- mp(om, oem=oem, ctrl=control, args=mseargs)
)

tracking(ices)[c('B.om', 'B.obs', 'B.est'), ]


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

plot_hockeystick.hcr(control$hcr,
  labels=c(lim="Blim", trigger="Btrigger", min="", target="Ftarget")) +
  xlab("SSB") + ylab(expression(bar(F)))

system.time(
rbld <- mp(om, oem=oem, ctrl=control, args=mseargs)
)

plot(om, ICES=ices, RBLD=rbld)



# --- ASSEMBLE MP runs

runs <- list(TREND=trend, TREND2Y=trend3y, LEN=length, ICES=ices)

plot(window(om, start=2005), runs)

save(runs, file="model/runs.Rdata", compress="xz")
