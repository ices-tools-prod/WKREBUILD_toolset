# test.R - DESC
# /home/mosquia/Active/Doing/toolset_ICES_WKREBUILD2+rebuild/WKREBUILD_toolset/test.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# XX {{{
# }}}

#--- B ~ F errors

dat <- do.call(rbind, lapply(seq(2, 6), function(y)
  model.frame(FLQuants(B=ssb(retros[[1]])[, ac(seq(2021 - 5, 2021 - y + 1))] /
      ssb(retros[[y]])[, ac(seq(2021 - 5, 2021 - y + 1))],
    F=fbar(retros[[1]])[, ac(seq(2021 - 5, 2021 - y + 1))] /
      fbar(retros[[y]])[, ac(seq(2021 - 5, 2021 - y + 1))]))))[, c("B", "F")]

cbf <- cov(dat)

samp <- mvtnorm::rmvnorm(1000, c(1, 1), cbf)

plot(samp)

ssb(om)[,'2018'] * samp[1:50,1]


dmvnorm(x=c(0,0))
dmvnorm(x=c(0,0), mean=c(1,1))

sigma <- matrix(c(4,2,2,3), ncol=2)
x <- rmvnorm(n=500, mean=c(1,2), sigma=sigma)
colMeans(x)
var(x)

x <- rmvnorm(n=500, mean=c(1,2), sigma=sigma, method="chol")
colMeans(x)
var(x)

plot(x)






