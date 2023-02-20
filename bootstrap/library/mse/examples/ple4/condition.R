# condition.R - OM conditioning for ICES PLE4 using FLa4a
# mse/examples/ple4/condition.R

# Copyright European Union, 2018
# Authors: Finlay Scott (EC JRC)
#          Ernesto Jardim (EC JRC) <ernesto.jardim@ec.europa.eu>
#          Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# LOAD packages

library(mse)
library(FLa4a)
library(ggplotFL)

# LOAD data

data(ple4)
data(ple4.indices)

stk <- ple4
idx <- ple4.indices["BTS-Combined (all)"]

# VARIABLES

it <- 500 # iterations

fy <- 2030 # final year
y0 <- range(stk)["minyear"] # initial OM year
dy <- range(stk)["maxyear"] # final OM year
iy <- dy # initial year of projection (also intermediate)
vy <- ac(seq(y0, dy))
nsqy <- 3 # number of years to compute status quo metrics

mpargs <- list(fy=fy, y0=y0, iy=iy, nsqy=nsqy)

# FLa4a MCMC run 

mcsave <- 500
mcmc <- mcsave * it

fit <- sca(stk, idx, fit="MCMC",
  mcmc = SCAMCMC(mcmc = mcmc, mcsave = mcsave, mcprobe = 0.499))

# UPDATE stock

stk <- slim(stk + fit)

# skin to keep one iteration
stk0 <- qapply(stk, iterMedians)

# SRR

# average recruitment estimation sd
rv1 <- sqrt(mean(c(iterVars(log(rec(stk)))), na.rm=TRUE))

# average autocor lag1
ac1 <- mean(apply(window(rec(stk), end=2008)@.Data, 6, function(x)
  c(acf(c(x), plot=FALSE, lag.max=1)$acf[2])))

# BevHolt
srbh <- fmle(as.FLSR(stk0, model="bevholt"), method="L-BFGS-B", lower=c(1e-6, 1e-6), upper=c(max(rec(stk)) * 3, Inf))

# Residuals
resbh <- ar1rlnorm(rho=ac1, years=dy:fy, iters=it, margSD=rv1*2)
residuals(srbh) <- resbh

# REFPTS

brp <- brp(FLBRP(stk0, srbh))

# EXTEND

# Set up future assumptions - means of 3 years
stk <- fwdWindow(stk, brp, end=2030)

# Fleet behaviour

fb <- mseCtrl(method=hyperstability.fb, args=list(beta=0.8))

# OM projection method

proj <- mseCtrl(method=fwd.om, args=list(maxF=2))

# OM object

om <- FLom(stock=stk, sr=srbh, refpts=refpts(brp), projection=proj)#, fleetBehaviour=fb)


# OEM

stk <- stock(om)

#==============================================================================
# Estimate the indices catchability from the a4a fit (without simulation)
#==============================================================================

set.seed(0)

# Use all indices
idcs <- FLIndices()
for (i in 1:length(idx)){
	# this is a simplification as if index reflects 01 January abundances
	lst <- mcf(list(idx[[i]]@index, stock.n(stk0)))
	# log catchability of index 
	idx.lq <- log(lst[[1]]/lst[[2]]) 
	# empty quant
	idx.qmu <- idx.qsig <- stock.n(iter(stk,1)) 
	# Every year has the same mean catchability
	idx.qmu[] <- yearMeans(idx.lq) 
	idx.qsig[] <- sqrt(yearVars(idx.lq))
	idx.q <- FLQuant(NA, dimnames=dimnames(stock.n(stk)))
	# Build FLQ of index catchability based on lognormal distribution with mean and sd calculated above
	idx.q <- rlnorm(it, idx.qmu, idx.qsig) 
	#idx.q[,ac(y0:iy)] <- idx.q[,ac(y0:iy)]
	idx_temp <- idx.q * stock.n(stk)
	# generate initial index
	idx_temp <- FLIndex(index=idx_temp, index.q=idx.q) 
	range(idx_temp)[c("startf", "endf")] <- c(0, 0)
	idcs[[i]] <- idx_temp
}
names(idcs) <- names(idx)

#==============================================================================
# Deviances for catch.n
#==============================================================================

set.seed(0)

catch.dev <- log(catch.n(stk))
catch.dev <- catch.dev-iterMeans(catch.dev)
Sig <- apply(catch.dev[,ac(y0:dy),1,1,,drop=TRUE], 3, function(x) cov(t(x)))
Sig <- apply(Sig, 1, mean)
Sig <- matrix(Sig, ncol=dim(catch.dev)[1])
catch.dev[,ac(vy)][] <- t(mvrnorm(it * length(vy), rep(0, nrow(Sig)), Sig))
catch.dev <- exp(catch.dev)

#==============================================================================
# OEM object
#==============================================================================

idxDev <- lapply(idcs, index.q)
names(idxDev) <- "index.q"
stkDev <- FLQuants(catch.n=catch.dev)
dev <- list(idx=idxDev, stk=stkDev)
obs <- list(idx=idcs[1], stk=stk)

oem <- FLoem(method=sampling.oem, observations=obs, deviances=dev)

###############################################################################
# Implementation error
###############################################################################

iem <- FLiem(method=noise.iem, args=list(fun="rlnorm", mean=0, sd=0, multiplicative=TRUE))

save(om, oem, iem, mpargs, file="ple4OM.RData", compress="xz")
