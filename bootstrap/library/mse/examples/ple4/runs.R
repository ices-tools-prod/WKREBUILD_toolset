# example.R - DESC
# mse/tests/example.R

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
load("ple4OM.RData")


#------------------------------------------------------------------------------
# base
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=FLQuant(0.3)))))
res1 <- mse:::mp00(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel

# run new method in single core without foreach
mpargs$nblocks <- 1
resp1 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res1), stock(resp1))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 3
resp1a <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res1), stock(resp1a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 3
resp1b <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res1), stock(resp1b))

#------------------------------------------------------------------------------
# base with TAC
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(
	ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is)))

res2 <- mse:::mp00(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp2 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res2), stock(resp2))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp2a <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res2), stock(resp2a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp2b <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res2), stock(resp2b))

#------------------------------------------------------------------------------
# base with TAC and SA
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa)))

res3 <- mse:::mp00(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp3 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res3), stock(resp3))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp3a <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res3), stock(resp3a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp3b <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res3), stock(resp3b))

#------------------------------------------------------------------------------
# base with TAC and IEM
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is)))

res4 <- mse:::mp00(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp4 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res4), stock(resp4))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp4a <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res4), stock(resp4a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp4b <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res4), stock(resp4b))

#------------------------------------------------------------------------------
# base with TAC and SA and OEM and IEM
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa),
	ctrl.tm = mseCtrl(method=mpa.tm, args=list(sel.objective=FLModelSim(model=~1/(1+exp(-(a+b*x))), params=FLPar(a=-10, b=5))))))

res5 <- mse:::mp00(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp5 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res5), stock(resp5))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp5a <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res5), stock(resp5a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp5b <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res5), stock(resp5b))

#------------------------------------------------------------------------------
# testing biased assessment
#------------------------------------------------------------------------------
biased.sa <- function(stk, idx, bbias=1, fbias=1, ...){
	args <- list(...)
	dy <- dimnames(catch(stk))[[2]]
	dy <- dy[length(dy)]
	tracking <- args$tracking
	stock.n(stk)[, dy] <- stock.n(stk)[, dy]*bbias
	harvest(stk)[, dy] <- harvest(stk)[, dy]*fbias
	list(stk = stk, tracking = tracking)
}

ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.2)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=biased.sa, args=list(fbias=.5))))

res6 <- mse:::mp00(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp6 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res6), stock(resp6))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp6a <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res6), stock(resp6a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp6b <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res6), stock(resp6b))

#------------------------------------------------------------------------------
# base with TAC and separable SA
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sep.sa, args=list(fit="assessment", qmodel=list(~s(age, k=3), fmodel=~s(age, k=4) + s(year, k=20), update=FALSE)))))

res7 <- mse:::mp00(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp7 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res7), stock(resp7))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 3
resp7a <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res7), stock(resp7a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp7b <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res7), stock(resp7b))

#plot(FLStocks(om=window(stock(om), end=2017), sce1=stock(res1), sce2=stock(res2), sce3=stock(res3), sce4=stock(res4), sce5=stock(res5), sce6=stock(res6), sce7=stock(res7)))

#==============================================================================
# Test again with cluster
#==============================================================================

cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {library(mse)})
registerDoParallel(cl)

#------------------------------------------------------------------------------
# base
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3))))

# run new method in single core without foreach
mpargs$nblocks <- 1
resp1 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res1), stock(resp1))

# run new method in 1 core with sequential foreach
mpargs$nblocks <- 2
resp1a <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res1), stock(resp1a))

# run new method in 2 cores with foreach
stopCluster(cl)
cl <- makeCluster(2)
clusterEvalQ(cl = cl, expr = {library(mse)})
registerDoParallel(cl)
resp1b <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res1), stock(resp1b))
stopCluster(cl)

#------------------------------------------------------------------------------
# base with TAC
#------------------------------------------------------------------------------
cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {library(mse)})
registerDoParallel(cl)

ctrl <- mpCtrl(list(
	ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is)))

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp2 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res2), stock(resp2))

# run new method in 1 core with sequential foreach
mpargs$nblocks <- 2
resp2a <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res2), stock(resp2a))

# run new method in 2 cores with foreach
stopCluster(cl)
cl <- makeCluster(2)
clusterEvalQ(cl = cl, expr = {library(mse)})
registerDoParallel(cl)
mpargs$nblocks <- 2
resp2b <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res2), stock(resp2b))
stopCluster(cl)

#------------------------------------------------------------------------------
# base with TAC and SA
#------------------------------------------------------------------------------
cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa)))

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp3 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res3), stock(resp3))

# run new method in 1 core with sequential foreach
mpargs$nblocks <- 2
resp3a <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res3), stock(resp3a))

# run new method in 2 cores with foreach
stopCluster(cl)
cl <- makeCluster(2)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
mpargs$nblocks <- 2
resp3b <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res3), stock(resp3b))
stopCluster(cl)

#------------------------------------------------------------------------------
# base with TAC and IEM
#------------------------------------------------------------------------------
cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {library(mse)})
registerDoParallel(cl)
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is)))

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp4 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res4), stock(resp4))

# run new method in 1 core with sequential foreach
mpargs$nblocks <- 2
resp4a <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res4), stock(resp4a))

# run new method in 2 cores with foreach
stopCluster(cl)
cl <- makeCluster(2)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
mpargs$nblocks <- 2
resp4b <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res4), stock(resp4b))
stopCluster(cl)

#------------------------------------------------------------------------------
# base with TAC and SA and OEM and IEM
#------------------------------------------------------------------------------
cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa),
	ctrl.tm = mseCtrl(method=mpa.tm, args=list(sel.objective=FLModelSim(model=~1/(1+exp(-(a+b*x))), params=FLPar(a=-10, b=5))))))

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp5 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res5), stock(resp5))

# run new method in 1 core with sequential foreach
mpargs$nblocks <- 2
resp5a <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res5), stock(resp5a))

# run new method in 2 cores with foreach
stopCluster(cl)
cl <- makeCluster(2)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
mpargs$nblocks <- 2
resp5b <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res5), stock(resp5b))
stopCluster(cl)

#------------------------------------------------------------------------------
# testing biased assessment
#------------------------------------------------------------------------------
cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
biased.sa <- function(stk, idx, bbias=1, fbias=1, ...){
	args <- list(...)
	dy <- dimnames(catch(stk))[[2]]
	dy <- dy[length(dy)]
	tracking <- args$tracking
	stock.n(stk)[, dy] <- stock.n(stk)[, dy]*bbias
	harvest(stk)[, dy] <- harvest(stk)[, dy]*fbias
	list(stk = stk, tracking = tracking)
}

ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.2)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=biased.sa, args=list(fbias=.5))))

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp6 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res6), stock(resp6))

# run new method in 1 core with sequential foreach
mpargs$nblocks <- 2
resp6a <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res6), stock(resp6a))

# run new method in 2 cores with foreach
stopCluster(cl)
cl <- makeCluster(2)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
mpargs$nblocks <- 2
resp6b <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res6), stock(resp6b))
stopCluster(cl)

#------------------------------------------------------------------------------
# base with TAC and separable SA
#------------------------------------------------------------------------------
cl <- makeCluster(1)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=0.3)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sep.sa, args=list(fit="assessment", qmodel=list(~s(age, k=3), fmodel=~s(age, k=4) + s(year, k=20), update=FALSE)))))

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp7 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res7), stock(resp7))

# run new method in 1 core with sequential foreach
mpargs$nblocks <- 2
resp7a <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res7), stock(resp7a))

# run new method in 2 cores with foreach
stopCluster(cl)
cl <- makeCluster(2)
clusterEvalQ(cl = cl, expr = {library(mse);library(FLa4a)})
registerDoParallel(cl)
mpargs$nblocks <- 2
resp7b <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res7), stock(resp7b))
stopCluster(cl)

#==============================================================================
# More tests
#==============================================================================

#------------------------------------------------------------------------------
# Iters in args
#------------------------------------------------------------------------------
flq <- FLQuant(c(0.3,0.2,0.4), dim=c(1,1,1,1,1,3))
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(
	method=fixedF.hcr, 
	args=list(ftrg=flq)
	)))
res1 <- mse:::mp00(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel

# run new method in single core without foreach
mpargs$nblocks <- 1
resp1 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(stock(res1), stock(resp1))
all.equal(res1, resp1) # genArgs will be different 

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 3
resp1a <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res1, resp1a)
all.equal(stock(res1), stock(resp1a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 3
resp1b <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res1, resp1b)
all.equal(stock(res1), stock(resp1b))

#------------------------------------------------------------------------------
# base with TAC
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(
	ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=flq)),
	ctrl.is = mseCtrl(method=tac.is)))
res2 <- mse:::mp00(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp2 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res2, resp2)
all.equal(stock(res2), stock(resp2))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp2a <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res2, resp2a)
all.equal(stock(res2), stock(resp2a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp2b <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res2, resp2b)
all.equal(stock(res2), stock(resp2b))

#------------------------------------------------------------------------------
# base with TAC and SA
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=flq)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa)))
res3 <- mse:::mp00(om, oem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp3 <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res3, resp3)
all.equal(stock(res3), stock(resp3))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp3a <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res3, resp3a)
all.equal(stock(res3), stock(resp3a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp3b <- mp(om, oem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res3, resp3b)
all.equal(stock(res3), stock(resp3b))

#------------------------------------------------------------------------------
# base with TAC and IEM
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=flq)),
	ctrl.is = mseCtrl(method=tac.is)))
res4 <- mse:::mp00(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp4 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res4, resp4)
all.equal(stock(res4), stock(resp4))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp4a <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res4, resp4a)
all.equal(stock(res4), stock(resp4a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp4b <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res4, resp4b)
all.equal(stock(res4), stock(resp4b))

#------------------------------------------------------------------------------
# base with TAC and SA and OEM and IEM
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=flq)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sca.sa),
	ctrl.tm = mseCtrl(method=mpa.tm, args=list(sel.objective=FLModelSim(model=~1/(1+exp(-(a+b*x))), params=FLPar(a=-10, b=5))))))
res5 <- mse:::mp00(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp5 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res5, resp5)
all.equal(stock(res5), stock(resp5))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp5a <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res5, resp5a)
all.equal(stock(res5), stock(resp5a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp5b <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res5, resp5b)
all.equal(stock(res5), stock(resp5b))

#------------------------------------------------------------------------------
# testing biased assessment
#------------------------------------------------------------------------------
biased.sa <- function(stk, idx, bbias=1, fbias=1, ...){
	args <- list(...)
	dy <- dimnames(catch(stk))[[2]]
	dy <- dy[length(dy)]
	tracking <- args$tracking
	stock.n(stk)[, dy] <- stock.n(stk)[, dy]*bbias
	harvest(stk)[, dy] <- harvest(stk)[, dy]*fbias
	list(stk = stk, tracking = tracking)
}

ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=flq)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=biased.sa, args=list(fbias=flq))))
res6 <- mse:::mp00(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp6 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res6, resp6)
all.equal(stock(res6), stock(resp6))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp6a <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res6, resp6a)
all.equal(stock(res6), stock(resp6a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp6b <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res6, resp6b)
all.equal(stock(res6), stock(resp6b))

#------------------------------------------------------------------------------
# base with TAC and separable SA
#------------------------------------------------------------------------------
ctrl <- mpCtrl(list(ctrl.hcr = mseCtrl(method=fixedF.hcr, args=list(ftrg=flq)),
	ctrl.is = mseCtrl(method=tac.is),
	ctrl.est = mseCtrl(method=sep.sa)))
res7 <- mse:::mp00(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)

# test parallel
# run new method in single core without foreach
mpargs$nblocks <- 1
resp7 <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res7, resp7)
all.equal(stock(res7), stock(resp7))

# run new method in 1 core with sequential foreach
registerDoParallel(1)
mpargs$nblocks <- 2
resp7a <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res7, resp7a)
all.equal(stock(res7), stock(resp7a))

# run new method in 2 cores with foreach
registerDoParallel(2)
mpargs$nblocks <- 2
resp7b <- mp(om, oem, iem, ctrl.mp=ctrl, genArgs=mpargs)
all.equal(res7, resp7b)
all.equal(stock(res7), stock(resp7b))


