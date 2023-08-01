# utilities.R - Extra functions
# WKREBUILD_toolset/utilities.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# PARALLEL setup via doFuture

if(exists("cores")) {
  # Linux
  if(os.linux() | os.macos()) {
    plan(multicore, workers=cores)
  # Windows
  } else if(os.windows()) {
    plan(multisession, workers=cores)
  }
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
