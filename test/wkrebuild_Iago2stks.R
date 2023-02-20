
setwd("C:/Work/Research/MSrefpts/R")
load("../Data/Updated_stks_n81.rdata",verbose = T)

#><>><>><>><>><>><>><>><>
# Western baltic cod
#><>><>><>><>><>><>><>><>
stk = stks$`cod.27.22-24`
refs = stk@benchmark
stk = FLStockR(stk)
stk@refpts = FLPar(refs)
# ICES hockey stick
hs = fmle(as.FLSR(stk,model=segreg),fixed=list(b=an(stk@refpts["Blim"])))
brp.hs = brp(FLBRP(stk,hs))
r0 = an(refpts(brp.hs)["virgin","rec"])
b0 = an(refpts(brp.hs)["virgin","ssb"])
# add to stk
stk@refpts = rbind(stk@refpts,FLPar(B0=b0,R0=r0))
plotAdvice(stk)                   
# bevholt conditioned on R0 from hs
bh = srrTMB(as.FLSR(stk,model=bevholtSV),spr0=mean((spr0y(stk))),r0=c(r0,0.2))
bh@SV
srs = FLSRs(hs=hs,bh=bh)
plotsrs(srs)
save(stk,srs,file="WBCod.rdata")

#><>><>><>><>><>><>><>
# NS horse mackeral
#><>><>><>><>><>><>><>
stk = stks$`hom.27.2a4a5b6a7a-ce-k8`
refs = stk@benchmark
stk = FLStockR(stk)
stk@refpts = FLPar(refs)
# ICES hockey stick
hs = fmle(as.FLSR(stk,model=segreg),fixed=list(b=an(stk@refpts["Blim"])))
brp.hs = brp(FLBRP(stk,hs))
r0 = an(refpts(brp.hs)["virgin","rec"])
b0 = an(refpts(brp.hs)["virgin","ssb"])
# add to stk
stk@refpts = rbind(stk@refpts,FLPar(B0=b0,R0=r0))
plotAdvice(stk)                   
# bevholt conditioned on R0 from hs
bh = srrTMB(as.FLSR(stk,model=bevholtSV),spr0=mean((spr0y(stk))),r0=c(r0,0.2))
bh@SV
# bevholt based on SS3 s = 0.99, r0 = 3.78474e+06 (crazy)
r0.ss = 3.78474e+06
bh.ss = srrTMB(as.FLSR(stk,model=bevholtSV),spr0=mean((spr0y(stk))),
            s=0.99,s.est=F,r0=c(r0.ss,0.0001))
bh.ss@SV
srs = FLSRs(hs=hs,bh=bh,bh.ss=bh.ss)
plotsrs(srs)
save(stk,srs,file="NSHom.rdata")




