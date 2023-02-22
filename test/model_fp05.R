# model_fp05.R - DESC
# /home/mosquia/NEW/Active/mse_rebuild_ICES/model_fp05.R

# Copyright (c) WUR, 2022.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(FLCore)
library(FLRef)

# LOAD NS plaoice (ple.27.40) dataset

data(ple4)

# FIT segreg SRR

hs <- srrTMB(as.FLSR(ple4, model=segreg), spr0=spr0y(ple4),
  lplim=0.05, uplim=0.25)

# SET 
blim <- params(hs)[[2]]

brp <- computeFbrp(ple4, hs, proxy=c("sprx", "f0.1", "msy"),
  x=40, blim=blim)

ploteq(brp)

fsim <- Fsim(brp,sigmaR=0.7,rho=0.3)

plotFsim(fsim)
plotFsim(fsim,panels=2)

# Get Stochastic Fmsy
fmmy = Fmmy(brp,sigmaR=0.7,rho=0.3)
getF(fmmy) # FMMY value (Stochastic Fmsy)
plotFsim(fmmy)
plotFsim(fmmy,panels=c(2:4))

# Get Fp.05
fp.05 = Fp05(fsim)
plotFsim(fp.05,panels=c(2,4)) # black line is Fp0.05
getF(fp.05) 

 getF(fmmy) # FMMY value (Stochastic Fmsy)

 getF(fp.05) 
