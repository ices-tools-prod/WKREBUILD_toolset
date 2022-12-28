# test.R - DESC
# mse_rebuild_ICES/test.R

# Copyright (c) WUR, 2022.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# COND (stk, sr=sr(stk), deviances=foo())
# - INPUT: FLStockR wo/, w/ iters + deviances
# - FIT SRR + CREATE deviances (eqsim trim)
# - OM refpts.
# TODO: STORE sigma and rho SRR

# TODO: fwd(-5 y) w/ rec deviances, ADD var in starting conditions.
# - inverse ramp: 

# MP mp(om, control=foo(...), args=list(iy))
# - OEM, ADD deviances oem (stk).
#   - SA historical bias ICES (ssb), 0.3 error
#   - f error ~ ssb error, for forecast.
# eqsim
#   - Fcv: Assessment error in the advisory year
#   - Fphi: Autocorrelation in assessment error in the advisory year
# - control: perfect.sa + hockeystick.hcr (recovery) + tac.is
# - IEM: error, TAC over/undershoot
# - mseargs: data_lag, management_lag

# mseviz
# - performance

install_github('flr/mse')

library(mse)

# EXAMPLE sol.27.4 OM + OEM

data(sol274)

# BUILD example plain ICES MP

control <- mpCtrl(list(
  # sa
  est = mseCtrl(method=perfect.sa),
  # hcr
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=refpts(om)$Blim, trigger=refpts(om)$Btrigger,
      target=refpts(om)$Fmsy, min=0, metric="ssb", output="fbar"))
))

mpr <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2040))

#' # Harvest Control Rules

#' - Horse mackerel recovery MP

control <- mpCtrl(list(
  # sa
  est = mseCtrl(method=perfect.sa),
  # hcr
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=refpts(om)$Blim, trigger=refpts(om)$Btrigger,
      target=refpts(om)$Fmsy, min=0.2 * refpts(om)$Fmsy,
      metric="ssb", output="fbar", dlow=0.20, dupp=0.20, all=FALSE))
  # ADD isys
))

rec <- mp(om, oem=oem, ctrl=control, args=list(iy=2021, fy=2040))

plot(om, mpr, rec)

#' ## Interesting to constraint the catch advice to reach certain SSB level at the end of the advice year (SSB y <= SSB y+1 or SSB = Blim/B ??? ) for example.

#' - FIND catch decrease that gives certain SSB level?

#' ## A HCR that is a combination of HCR RecPlan and HCR MSY . HCR MSY after K years with p(SSB>B lim ) >95% | p(SSB>B trigger ) > 95%

#' - meta.hcr by iter

#' # Performance Indicators.

#' ## Tmin

fw0 <- fwd(om, control=fwdControl(year=2022:2042, quant='fbar', value=1e-16))

plot(ssb(fw0)[, ac(2022:2042)] / refpts(om)$Blim) +
  geom_hline(yintercept=1, linetype=2) +
  ylim(c(0, NA)) + ylab(expression(SSB / B[lim]))

#' ## Recovery criteria:

#' - P(SSB>Blim) > 95% for K number of years.

iterMeans((ssb(fw0)[, ac(2022:2042)] / refpts(om)$Blim) > 1)

#' - P(SSB>Btrigger) > 95% for K number of years.

iterMeans((ssb(fw0)[, ac(2022:2042)] / refpts(om)$Btrigger) > 1)


#' # Operating model

#' - Uncertainty conditioning with some default options, like in eqSim.

#' - Option where the user can use whatever she wants (two different functions, conditioning & simulation).

#' - Several SR models, including steepness parameterization (√, available).


#' # Management Procedure

#' - Time lag between assessment and advice.
#' - Observation errors: Not a default option, not a priority.
#' - Full feedback: Not a default option, not a priority.
#' - Having the option to call to ‘something’ that returns an FLStock, simulate data (índices) (not a priority).
#' - Short cut: Option to modify the ‘real’ FLStock to account for bias in the assessment coming from retrospective or ohter sources. There is something on eqSim.
#' - Implementation error: A multiplier to account for overshoot/undershoot of the TAC whatever the reason.

