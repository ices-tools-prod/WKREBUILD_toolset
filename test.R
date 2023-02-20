# test.R - DESC
# /home/mosquia/Active/toolset_ICES_WKREBUILD2+rebuild/WKREBUILD_toolset/test.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# TODO: mps() to PARSE multiple arguments:
# - SINGLE vector with recycling?
# runs <- mps(om, oem=oem, ctrl=list(AR=arule, RB=rrule), args=mseargs)

# mps w/ ctrls {{{

mps <- function(om, oem, ctrl, args, ...) {

  # GET ... arguments
  opts <- list(...)

  # OPTIONS in opts?
  if(length(opts) > 0) {
  
  # OR ctrl
  } else {
    if(is(ctrl, "list") & !is(ctrl, "mpCtrl")) {
    a <- 1
    }
  }

  # DO options refer to ctrl elements?
  if(!names(opts) %in% names(ctrl))
    stop("options refer to modules not present in ctrl")

  # EXTRACT 1st option values
  module <- names(opts)[1]
  modarg <- names(opts[[1]])
  values <- opts[[module]][[modarg]]

  # TODO: PARSE all options and assemble one list

  res <- foreach(i = values) %dopar% {
    args(ctrl[[module]])[[modarg]] <- i
    tryCatch(mp(om, oem=oem, ctrl=ctrl, args=args, parallel=FALSE),
      error=function(e) {
        stop("")
      })
  }

  names(res) <- paste(module, modarg, round(values), sep="_")

  return(res)
}
# }}}

# COMBINE SRRs, weighted? {{{

# 1. mix SRR with param setting model, c(a,b,c,model)

s1 <- fmle(as.FLSR(iter(runmc,1), model="segreg"))
s2 <- fmle(as.FLSR(iter(runmc,1), model="bevholt"))
s3 <- fmle(as.FLSR(iter(runmc,1), model="ricker"))

pas <- rbind(cbind(params(s1), params(s2), params(s3)), FLPar(m=seq(3)))


mixed <- function(ssb, a, b, m) {

  rec <- ssb %=% as.numeric(NA)

  switch(m,
    # 1. segreg
    `1`=ifelse(ssb <= b, a * ssb, a * b),
    # 2. bevholt
    `2`=a*ssb/(b+ssb),
    # 3. ricker
    `3`=a*ssb*exp(-b*ssb)
  )
}

mod <- predictModel(
  model=rec ~ mixed(ssb, a, b, m),
  params=pas)

c(predict(mod, ssb=FLQuant(67330, dimnames=list(year=2000, iter=seq(3)))))

predict(s1, ssb=FLQuant(67330, dimnames=list(year=2000, iter=seq(1))))
predict(s2, ssb=FLQuant(67330, dimnames=list(year=2000, iter=seq(1))))
predict(s3, ssb=FLQuant(67330, dimnames=list(year=2000, iter=seq(1))))
# }}}

# COMPARE ffwd and fwd {{{

library(FLasher)

load('test/ffwd.RData')

fla <- fwd(fut, sr=srr, control=fctrl)

ffa <- ffwd(fut, sr=srr, control=fctrl)

all.equal(fla[, ac(2023:2024)], ffa[, ac(2023:2024)])


which(is.na(catch.wt(ffa)[,'2024']))

catch.wt(ffa)[,'2024',,,,14]

landings.wt(ffa)[,'2024',,,,14]
discards.wt(ffa)[,'2024',,,,14]

landings.n(ffa)[,'2024',,,,14]
discards.n(ffa)[,'2024',,,,14]


catch(fla)[, ac(2023:2024)]
catch(ffa)[, ac(2023:2024)]

all.equal(fla[, ac(2023)], ffa[, ac(2023)])

ffa1 <- ffwd(fut, sr=srr, control=fctrl[1,])
ffa2 <- ffwd(ffa1, sr=srr, control=fctrl[2,])

all.equal(fla[, ac(2023:2024)], ffa2[, ac(2023:2024)])

catch(ffa2)[, ac(2023:2024)]

stock(fla)[, ac(2023:2024)]
stock(ffa)[, ac(2023:2024)]

stock.n(fla)[,'2023']
stock.n(ffa)[,'2023']

stock.n(fla)[,'2024']
stock.n(ffa)[,'2024']

all.equal(catch.n(fla)[,'2023'], catch.n(ffa)[,'2023'])
all.equal(catch.n(fla)[,'2024'], catch.n(ffa)[,'2024'])

sum(is.na(catch.n(fla)))
sum(is.na(catch.n(ffa)))

catch.n(fla)[,'2024']
catch.n(ffa)[,'2024']

catch.wt(fla)[,'2023']
catch.wt(ffa)[,'2023']

catch.wt(fla)[,'2024']
catch.wt(ffa)[,'2024']

harvest(fla)[,'2023']
harvest(ffa)[,'2023']

harvest(fla)[,'2024']
harvest(ffa)[,'2024']


# }}}

# rules {{{

irule <- mpCtrl(list(
  # shortcut.sa + SSBdevs
  est = mseCtrl(method=shortcut.sa,
    args=list(SSBdevs=SSBdevs)),
  # hockeystick as ICES
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=0, trigger=c(icespts$Btrigger), target=c(icespts$Fmsy), min=0,
    metric="ssb", output="fbar")),
  #
  isys = mseCtrl(method=tac.is,
    args=list(recyrs=30, fmin=0, Fdevs=Fdevs))
))




# }}}


# retroError

load('~/Projects/ICES/SOL.27.4/2022/wgnssk/2022_sol.27.4_assessment/model/retro.Rdata')

retros <- sxval$retro
final <- retros[[1]]
retros <- retros[-1]
library(mvtnorm)

retroDevs <- function(retros, final=retros[[1]], iters=100) {

  # CHECK retros against final
  if(!missing(final)) {
    rryrs <- unlist(lapply(retros, function(x) dims(x)$maxyear))
    if(max(rryrs) == dims(final)$maxyear)
      retros <- retros[-which(rryrs == dims(final)$maxyear)]
  }

  #
  corr <- do.call(rbind, lapply(retros, function(x) {

    yr <- ac(dims(fbar(x))$maxyear)
    
    c(f=c(fbar(x)[, yr] / fbar(final)[, yr]),
      ssb=c(ssb(x)[, yr] / ssb(final)[, yr]))
  }))

  samp <- rmvnorm(iters, c(1, 1), cov(corr))

  dmns <- list(age='all', year=1, iter=seq(iters))

  Fdevs <- FLQuant(samp[,1], dimnames=dmns, units="")
  SSBdevs <- FLQuant(samp[,2], dimnames=dmns, units="")

  return(samp)
}

#

hind <- FLStocks(lapply(retros, function(x) {
  yrs <- ac(seq(dims(x)$maxyear + 1, 2021))
  fwd(fwdWindow(x, end=2021), sr=rec(final), catch=catch(final)[, yrs])
  }
))

# HINDCAST retros w/ deviances

# - fwd 10 year retro w/ final biology, rec, catch

back <- final
ret <- 5
yr <- dims(retros[[ret]])$maxyear
stock.n(back)[, ac(1957:yr)] <- stock.n(retros[[ret]])

devsr <- deviances(om)[, ac(seq(2022, length=ret))]
dimnames(devsr) <- list(year=yr:2021)

om0 <- fwd(propagate(back, 100), catch=catch(final)[, ac(yr:2021)], 
  sr=rec(final), deviances=devsr)

plot(om0)


hinds <- lapply(setNames(nm=5:10), function(y) {
  
  back <- final
  yr <- dims(retros[[y]])$maxyear
  stock.n(back)[, ac(1957:yr)] <- stock.n(retros[[y]])

  devsr <- deviances(om)[, ac(seq(2022, length=y))]
  dimnames(devsr) <- list(year=yr:2021)

  fwd(propagate(back, 100), catch=catch(final)[, ac(yr:2021)], 
    sr=rec(final), deviances=devsr)
  }
)

plot(FLStocks(hinds))

# HINDCAST of SA run w/ rec deviances
# - deviances correlated to rec?

devsr <- deviances(om)[, ac(seq(2022, length=6))]
dimnames(devsr) <- list(year=2016:2021)

hindf <- fwd(propagate(final, 100), catch=catch(final)[, ac(2016:2021)], 
  sr=rec(final), deviances=devsr)

plot(hindf)

