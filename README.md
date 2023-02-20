# WKREBUILD_toolset: FLR-based tools for evaluating recovery plans in ICES.

- DISTRIBUTE as a template TAF repository
  - [How to create it](https://docs.github.com/en/repositories/creating-and-managing-repositories/creating-a-template-repository)

## data.R - OM conditioning

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

### iem

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

## model.R

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

## tracking

metric         2023
  F.om         2.0006e-01(1.72e-02)
  B.om         5.4902e+04(6.86e+03)
  SB.om        3.9090e+04(5.62e+03)
  C.om         9.9156e+03(8.90e+02)
  C.obs        9.9156e+03(8.90e+02)
  B.obs        5.4902e+04(6.86e+03)
  SB.obs       3.9090e+04(5.62e+03)
  F.est        2.0006e-01(1.72e-02)
  B.est        5.4902e+04(6.86e+03)
  SB.est       3.9090e+04(5.62e+03)
  C.est        9.9156e+03(8.90e+02)
  conv.est             NA(      NA)
  iem                  NA(      NA)
  hcr          2.0721e-01(0.00e+00)
  isys         9.9156e+03(8.90e+02)
  time         4.9754e-04(0.00e+00)
  fwd          9.9156e+03(8.90e+02)
  fb                   NA(      NA)
  met.hcr      3.9090e+04(5.62e+03)
  decision.hcr 2.0000e+00(0.00e+00)
  fbar.hcr     2.0721e-01(0.00e+00)
  pid          1.9109e+06(0.00e+00)
