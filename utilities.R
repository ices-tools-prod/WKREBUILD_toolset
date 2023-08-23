# utilities.R - Extra functions
# WKREBUILD_toolset/utilities.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# PARALLEL setup via doFuture

if(exists("cores")) {
  plan(multisession, workers=cores)
  options(doFuture.rng.onMisuse="ignore")
}


# icesmetrics {{{

# NAME = function ~ refpt, e.g. FMSY = fbar(om) / refpts(om)$Fmsy

icesmetrics <- list(FMSY=fbar~Fmsy, SBMSY=ssb~Btrigger,
  SBPA=ssb~Bpa, SBlim=ssb~Blim)

# }}}

# WKREBUILD2 performance statistics {{{

annualstats <- list(

  # P(SB>SBlim)
  PBlim=list(~iterMeans((SB/Blim) > 1), name="P(SB>SB[lim])",
    desc="Probability that spawner biomass is above Blim"),

  # P(SB>SBtrigger)
  PBtrigger=list(~iterMeans((SB/Btrigger) > 1), name="P(SB>B[trigger])",
    desc="Probability that spawner biomass is above Btrigger"),

  # mean(C)
  C=list(~iterMeans(C), name="mean(C)",
    desc="Mean catch over years"),

  # cv(C)
  cvC=list(~sqrt(iterVars(C)) / iterMeans(C), name="cv(C)",
    desc="CV of catch over years")
)

fullstats <- list(

  # mean(C)
  C=list(~yearMeans(C), name="mean(C)",
    desc="Mean catch over years"),

  # AVVC
  # AVVC
  AAVC=list(~yearMeans(abs(C[, -1] - C[, -dim(C)[2]])/C[, -1]),
    name="AAV(C)", desc="Average annual variability in catch"),
  
  # IACC
  IACC=list(~100 * yearSums(abs(C[, -1] - C[, -dim(C)[2]]))/yearSums(C),
    name="IAC(C)",
    desc="Percentage inter-annual change in catch"),

  # P(SB < SBlim) at least once
  risk2=list(~yearMeans(iterMeans(((SB/Blim) < 1) > 0)),
    name="once(P(SB<B[limit]))",
    desc="ICES Risk 2, probability that spawner biomass is above Blim once"),

  # 1st year
  firstyear=list(~firstYear(iterMeans(SB/Blim > 1) >= 0.95),
  name="recovery", desc="First year in which P(SB/SBlim) >= 0.95")
)

# }}}

# firstyear {{{

# firstYear(iterMeans(SB/Blim > 1) >= 0.95)

firstYear <- function(x) {
  year <- as.numeric(dimnames(x)$year[match(TRUE, x)])
  return(FLQuant(year))
}
# }}}

# decisions {{{

decisions <- function(x, year=1, iter=NULL) {

  trac <- tracking(x)
  args <- args(x)

  year <- as.numeric(year)

  if(is.null(iter))
    iter <- seq(dims(x)$iter)

  .table <- function(d) {

    its <- dims(d)$iter
    dmns <- dimnames(d)

    if(its == 1) {
    data.frame(metric=dmns$metric, year=dmns$year, value=prettyNum(d))
    } else {
      data.frame(metric=dmns$metric, year=dmns$year,
        value=sprintf("%s (%s)", 
          prettyNum(apply(d, 1:5, median, na.rm=TRUE)),
          prettyNum(apply(d, 1:5, mad, na.rm=TRUE))))
    }
  }

  res <- lapply(year, function(y) {
  
    ay  <-  y
    dy <- ay - args$data_lag
    my  <- ay + args$management_lag

    dmet <- c("SB.om", "SB.obs", "SB.est")
    amet <- c("met.hcr", "decision.hcr", "fbar.hcr", "hcr", "fbar.isys", "isys",
      "fwd", "C.om")

    dout <- trac[dmet, ac(dy),,,, iter]
    aout <- trac[amet, ac(ay),,,, iter]
    mout <- trac["SB.om", ac(my),,,,iter] / trac["SB.om", ac(ay),,,,iter]
    dimnames(mout)$metric <- "diff(SB.om)"

    rbind(.table(dout), .table(aout), .table(mout))      
  })

  do.call(cbind, res)
}
# }}}


# TODO: CODE showMethod
