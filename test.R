# test.R - DESC
# /home/mosquia/Active/toolset_ICES_WKREBUILD2+rebuild/WKREBUILD_toolset/test.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# summaries {{{

load("model/runs.RData")

# WHAT to output by run?

# - metrics
metrics(runs[[1]])

# - refpts
refpts(runs[[1]])

# CREATE single FLQuants for metrics by run

# on unit

y <- lapply(runs, metrics)
z <- Reduce(ubind, lapply(y, '[[', 1))
dimnames(z)$unit <- names(y)
su <- FLQuants(lapply(setNames(nm=names(y[[1]])), function(i) {
  res <- Reduce(ubind, lapply(y, '[[', i))
  dimnames(res)$unit <- names(y)
  res}))

# on quant

z <- Reduce(qbind, lapply(y, '[[', 1))
quant(z) <- 'mp'
dimnames(z)$mp <- names(y)

mpsu <- FLQuants(lapply(setNames(nm=names(y[[1]])), function(i) {
  res <- Reduce(qbind, lapply(y, '[[', i))
  quant(res) <- 'mp'
  dimnames(res)$mp <- names(y)
  res}))

# DIFF in size

(an(object.size(mpsu)) / an(object.size(runs))) * 100

# OPERATE on refpts

mpsu$F %/% expand(as(refpts(om), 'FLQuant'), year=2022:2041)$Fmsy


omsu <- window(metrics(om), end=args(runs[[1]])$iy)

plot(omsu$F, mpsu$F[1,])

# divide for performance

divide(mpsu[[1]], 1)

# TODO: divide(mpsu, 1)

# tracking

# controls diffs

# summ

# CONVERT to data.table?


# }}}



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

# retroError {{{


# F~SSB correlation

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

corfb <- data.frame(retroDevs(retros, final=retros[[1]], iters=500))

colnames(corfb) <- c('F', 'SSB')

ggplot(corfb, aes(F, SSB)) + geom_point() +
  xlab("F") + ylab("SSB")

# AR1

# https://stats.stackexchange.com/questions/71540/how-to-simulate-two-correlated-ar1-time-series

n <- 100
rho <- -0.0021
rho1 <- 0.4
rho2 <- 1e-4
sigma1 <- 0.2
sigma2 <- 0.3

n <- 100
rho <- -0.0021
Fcv <- 0.212
Fphi <- 0.423
SSBcv <- 0.10
SSBphi <- 1e-6


library(mvtnorm)

.corrErr <- function(n, rho, rho1, rho2, sigma1, sigma2) {

  burnin <- n / 10

  sigma11 <- sqrt(sigma1 ^ 2 * (1 - rho1 ^ 2))
  sigma22 <- sqrt(sigma2 ^ 2 * (1 - rho2 ^ 2))
  
  q12 <- rho *(1 - rho1 * rho2) / sqrt((1 - rho1 ^ 2) * (1 - rho2 ^ 2))

  cov <- q12 * sigma11 * sigma22

  eps <- rmvnorm(n + burnin, mean=c(0, 0),
    sigma=cbind(c(sigma11 ^ 2, cov), c(cov, sigma22 ^ 2)))

  x <- arima.sim(list(ar=rho1), n, innov=eps[burnin + 1:n, 1],
    start.innov=eps[1:burnin, 1], sd=sigma1)

  y <- arima.sim(list(ar=rho2), n, innov=eps[burnin + 1:n, 2],
    start.innov=eps[1:burnin, 2], sd=sigma2)

  return(list(x=c(x), y=c(y)))
}


n <- 100
rho <- -0.0021
Fcv <- 0.212
Fphi <- 0.423
SSBcv <- 0.10
SSBphi <- 1e-6



des <- .corrErr(n = 100, rho = rho, rho1 = Fphi, rho2 = SSBphi,
  sigma1 = Fcv, sigma2 = SSBphi)

sd(des$x)
sd(des$y)
cov(des$x,des$y)
acf(des$x, plot=FALSE)['1']
acf(des$y, plot=FALSE)['1']



des <- foo(n = 100, rho = 0.6, rho1 = 0.4, rho2 = 1e-4, sigma1 = 0.2, sigma2 = 0.3)

# ---


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
# }}}



om1=herRet[[1]]
herErr=laply(herRet, function(x){
   yr=ac(dims(fbar(x))$maxyear)

   c(f  =c(fbar(x)[,yr]/fbar(om1)[,yr]),
     ssb=c(ssb(x)[,yr])/c(ssb(om1)[,yr]))})

rmvnorm(1000,c(1,1),cov(herErr[-1,]))


library(forecast)
auto.arima(x)
auto.arima(y)
cor(x,y) ## close to rho
sd(x)  ## close to sigma1
sd(y) ## close to sigma 2


# ---

set.seed(123)
calcrho<-function(rho,rho1,rho2, sigma1, sigma2) {
  rho*(1-rho1*rho2)/sqrt((1-rho1^2)*(1-rho2^2))
}
burn.in<-300
n<-5000 
rho<-0.8
rho1<-0.5
rho2<-0.7
sigma1 <- 2
sigma11 <- sqrt(sigma1^2*(1-rho1^2))
sigma2 <- 5
sigma22 <- sqrt(sigma2^2*(1-rho2^2))
q12<-calcrho(rho,rho1,rho2)

library(MASS)
mycov <- q12*sigma11*sigma22
eps<-mvrnorm(n+burn.in,mu=c(0,0),Sigma=cbind(c(sigma11^2,mycov),c(mycov,sigma22^2)))
cor(eps[,1], eps[,2])
plot(1:5300, eps[,2])
points(1:5300, eps[,1], col="red")

x<-arima.sim(list(ar=rho1),n,innov=eps[burn.in+1:n,1],start.innov=eps[1:burn.in,1], sd=2)
y<-arima.sim(list(ar=rho2),n,innov=eps[burn.in+1:n,2],start.innov=eps[1:burn.in,2], sd=5)
library(forecast)
auto.arima(x)
auto.arima(y)
cor(x,y) ## close to rho
sd(x)  ## close to sigma1
sd(y) ## close to sigma 2


# ---






set.seed(123)

calcrho <- function(rho,rho1,rho2) {
  rho * (1 - rho1 * rho2) / sqrt((1 - rho1 ^ 2) * (1 - rho2 ^ 2))
}

burn.in <- 100
n <- 100 

# cross-correlation
rho <- 0.6

# AC
rho1 <- 0.4
rho2 <- 0.001


q12 <- calcrho(rho, rho1, rho2)

corr <- cbind(c(1, q12), c(q12, 1))

eps <- rmvnorm(n + burn.in, mean = c(1, 1), sigma = corr)

x<-arima.sim(list(ar=rho1),n,innov=eps[burn.in+1:n,1],start.innov=eps[1:burn.in,1])
y<-arima.sim(list(ar=rho2),n,innov=eps[burn.in+1:n,2],start.innov=eps[1:burn.in,2])
cor(x,y)
acf(x, plot=FALSE)['1']
acf(y, plot=FALSE)['1']

var(x)
var(y)


     arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),


sigma1 <- 2
sigma2 <- 5
q12<-calcrho(rho,rho1,rho2,sigma1, sigma2)
library(MASS)
mycov <- q12*sigma1*sigma2
eps<-mvrnorm(n+burn.in,mu=c(0,0),Sigma=cbi

library(MASS)
mycov <- q12*sigma1*sigma2
eps<-mvrnorm(n+burn.in,mu=c(0,0),Sigma=cbind(c(sigma1^2,mycov),c(mycov,sigma2^2)))




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
# }}}



om1=herRet[[1]]
herErr=laply(herRet, function(x){
   yr=ac(dims(fbar(x))$maxyear)

   c(f  =c(fbar(x)[,yr]/fbar(om1)[,yr]),
     ssb=c(ssb(x)[,yr])/c(ssb(om1)[,yr]))})

rmvnorm(1000,c(1,1),cov(herErr[-1,]))


# TODO: --- CONSTRUCT oem for full feedback

it <- dim(om)[6]

# observations: stk, idx, lens
index.q(indices$BTS)[] <- q.hat(mcfit)$BTS
index.q(indices$SNS)[] <- q.hat(mcfit)$SNS[1:6,]

obs <- list(stk=fwdWindow(runmc, end=fy),
    # TODO: propagate(FLlst)
    idx=lapply(fwdWindow(indices, end=fy), propagate, it))

# deviances
devs <- list(stk=list(
    catch.n=rlnorm(it, window(catch.n(runmc), end=fy) %=% 0, 0.15),
    stock.n=rlnorm(1000, window(catch.n(runmc), end=fy) %=% 0,
  expand(yearMeans(log(sqrt(iterVars(stock.n(om)[, ac(2002:2021)])))),
  year=1957:2042))),
  idx=lapply(obs$idx, function(x) rlnorm(it,
    window(index.q(x), end=fy) %=% 0, 0.20))
)

# lognormal w/age-specific errors
oem <- FLoem(observations=obs, deviances=devs, method=sampling.oem)

# --- profvis

library(profvis)

profvis(source('model.R'))

profvis(
        mp(om, oem=oem, ctrl=arule, args=list(iy=2021, fy=2031))
)

library(bench)


stk <- window(stock(om), end=2021)

mark(stf(stk, end=2042))
mark(fwdWindow(stk, end=2042))

a <- stf(stk, end=2042)
b <- fwdWindow(stk, end=2042)

all.equal(a, b)

x <- expand(catch.n(stk),  unit=c('F', 'M'))

y <- unitMeans(x)

mark(apply(x, c(1:2,4:6), mean, na.rm=TRUE))

mark(unitMeans(x))

mark((x[,,1,,] + x[,,2,,]) / 2)

mark(rowMeans(aperm(x, c(1,2,4,5,6,3)), na.rm=TRUE, dim=5))



FLQuant(c(rowMeans(aperm(x, c(1,2,4,5,6,3)), na.rm=TRUE, dim=5)),
  dimnames=c(dimnames(x)[-3], unit='unique'))
