# p4mse.R - DESC
# /p4mse.R

# Copyright Iago MOSQUEIRA (WMR), 2019
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)
library(FLa4a)

library(doParallel)
registerDoParallel(3)

# OM

data(p4om)

# args

mpargs <- list(fy=2030, y0=1957, iy=2017, nsqy=3, seed = 1234)

# MP

ctrl <- mpCtrl(list(
	est = mseCtrl(method=sca.sa),
	phcr = mseCtrl(method=movingF.phcr, args=list(interval=5, frp='msy')),
	hcr = mseCtrl(method=movingF.hcr),
	isys = mseCtrl(method=tac.is)))

iem <- FLiem(method=noise.iem,
  args=list(fun="rlnorm", mean=0, sd=0.2, multiplicative=TRUE))

# MSE

mse <- mp(om, oem, iem, ctrl=ctrl, args=mpargs)

# SAVE

save(mse, file="../data/p4mse.RData", compress="xz")
