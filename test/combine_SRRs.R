# test.R - DESC
# /home/mosquia/Active/toolset_ICES_WKREBUILD2+rebuild/WKREBUILD_toolset/test.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# - COMPARE ffwd and fwd {{{

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


# TODO: COMBINE SRRs, weighted?
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



