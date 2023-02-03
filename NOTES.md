---
title:
author: "Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>"
tags:
created: 2023-02-03T09:20:31+01:00
updated: 2023-02-03T09:29:13+01:00
---


- DISTRIBUTE as a template repository
  - [How to create it](https://docs.github.com/en/repositories/creating-and-managing-repositories/creating-a-template-repository)

# TODO

* ADD Depensation (sigmoid bevholt)
  - bevholdD <- rec~a*ssb^d/(b+ssb^b)

* UNDERSTAND eqsim::eqsim_run Fcv, Fphi, SSBcv (CM, DM)

* deviances(om), <-
  - FLom
  - FLombf


# WORKFLOW

## data.R

- GET SA output (FLStock + refpts + retro)
  - McMC if available

- (ADD status uncertainty)

### foo(stock, sigma, rho, ...) ---

- FIT SRR
  - One or more models
  1. DEFAULT fmle(hockeystick, fixed=list(b=Blim))
  2. FIT on McMC
  - RECDEVS
  3. rho, sigmaR(FLSR) for recdevs ar1rlnorm( sigmarho(FLSR))
  * ID options for WKREBUILD2 stocks

- FWD(iy, Fmsy | TAC)

- BUILD oem
  - method, perfect.oem
  - observations
    - FLStock
    - (FLIndices)
  - deviances
    - stock.n
      - CHECK catch.n corr code + ASK LK
      - USE retrospective bias, error + trend

- PREPARE future om
  - SRR deviances

- PREPARE future oem
  - SHORTCUT deviances

### ---

- PREPARE iem
  - Implementation error: A multiplier to account for over-shoot/undershoot of the TAC whatever the reason.

- SRR ensemble ?
  - CREATE 2 Oms
  - COMBINE FLmses

- ASSEMBLE base control
  - perfect.sa / shortcut.sa
  - hockeystick.hcr -> rebuild.hcr
    - ARG names in wkrebuild, HM
  * SWITCH from ices.hcr to rebuild.hcr
  - tac.is
    - CHOICE TAC / Ftarget

- ALTERNATIVE runs controls
  - ??

- RUN mp

- TUNE mp

## output.R

- COMPUTE performance

  - 95% P(B<Blim)
  - 80% Bpa

  - landings
  - AVV
  - 

- years = list(3, gt, 2*gt)
- gt from FLSRTMB:::productivity(runmc)
  - yearMeans(FLSRTMB:::productivity(runmc)[['gt']])

- T MIN : the time taken for the stock to rebuild with zero fishing to above Blim, or the agreed rebuilding target with 95% probability, or other level of probability depending on the state of depletion of the stock.
- T MAX : the maximum amount of time for rebuilding the stock, is usually specified by managers/requesters but could be expressed as x* T MIN with x > 1.
  - gt

Recovery criteria:
  - P(SSB>Blim) > 95/98% for K number of years.
  - P(SSB>Btrigger) > 80/95% for K number of years.

- INSPECT tracking
  - 

## report.R

- PLOTS
- TABLES
