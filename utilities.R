# utilities.R - DESC
# WKREBUILD_toolset/utilities.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# retroErrorByAge {{{

retroErrorByAge <- function(retros, object) {

  # GET dims
  nret <- length(retros) - 1
  fy <- dims(retros[[1]])$maxyear

  # COMPUTE errors: N=stock.n, F=harvest
  dat <- do.call(rbind, lapply(seq(2, nret), function(y) {
  model.frame(FLQuants(
    N=stock.n(retros[[1]])[, ac(seq(fy - nret, fy - y + 1))] /
      stock.n(retros[[y]])[, ac(seq(fy - nret, fy - y + 1))],
    F=harvest(retros[[1]])[, ac(seq(fy - nret, fy - y + 1))] /
      harvest(retros[[y]])[, ac(seq(fy - nret, fy - y + 1))]))
  }))

  # CALCULATE covariance with age
  cab <- cov(dat[, c('age', 'N')])

  # COMPUTE mean and sd by age
  musig <- data.table(dat)[, .(mean=mean(N), sd=sd(N)), by=age]

  nsamp <- length(object[1,])

  out <- Reduce(qbind, FLQuants(lapply(musig$age, function(x) {
    FLQuant(rnorm(nsamp, musig[age == x]$mean, musig[age == x]$sd),
      dimnames=list(age=x, year=dimnames(object)$year))
  })))

  return(out)
}
# }}}

# icesControl {{{

icesControl <- function(SSBdevs, Fdevs, Btrigger, Ftarget, Blim=0, Fmin=0,
  recyrs=25, dtaclow=NA, dtacupp=NA) {

  # CHECK inputs

  # BUILD mseCtrl
  ctrl <- mpCtrl(list(

  # shortcut.sa + SSBdevs
  est = mseCtrl(method=shortcut.sa,
    args=list(SSBdevs=SSBdevs)),

  # hockeystick
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=Blim, trigger=Btrigger, target=Ftarget, min=Fmin,
    metric="ssb", output="fbar")),

  # tac.is + Fdevs
  isys = mseCtrl(method=tac.is,
    args=list(recyrs=recyrs, fmin=Fmin, dtaclow=dtaclow, dtacupp=dtacupp,
    Fdevs=Fdevs))
  ))

  return(ctrl)
} 
# }}}

# shortcut_devs {{{

shortcut_devs <- function(om, Fcv=0.212, Fphi=0.423, SSBcv=0, SSBphi=0) {
  
  devs <- FLQuants(
    F=ar1rlnorm(Fphi, dimnames(om)$year, dims(om)$iter, 0, Fcv),
    SSB=ar1rlnorm(0, dimnames(om)$year, dims(om)$iter, 0, SSBcv)
  )

  return(devs)
}
# }}}
