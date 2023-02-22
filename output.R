# output.R - Performance evaluation
# WKREBUILD_toolset/output.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# TODO: USE names in years list, e.g. list(short=2000:2005, long=2000:2030)


library(mse)

load("model/runs.RData")

# CHECK metrics and refpts

# "SB" "C"  "F"
names(metrics(runs[[1]]))

# "Btrigger" "Fmsy" "Blim" "Bpa" "Flim" "Fpa" "lFmsy" "uFmsy" "F05" "F05noAR"
dimnames(refpts(runs[[1]]))$params

# DEFINE performance statistics

stats <- list(

  # P(SB>SBlim)
  PBlim=list(~iterMeans((SB/Blim) > 1), name="P(SB>SB[lim])",
    desc="Probability that spawner biomass is above Blim"),

  # mean(C)
  C=list(~yearMeans(C), name="mean(C)",
    desc="Mean catch over years"),

  # AVVC
  AAVC=list(~yearMeans(abs(C[, -1] - C[, -dim(C)[2]])/C[, -1]),
    name="AAV(C)", desc="Average annual variability in catch"),

  # P(SB < SBlim) at least once
  risk2=list(~yearMeans(iterMeans(((SB/Blim) < 1) > 0)),
    name="once(P(SB<B[limit]))",
    desc="ICES Risk 2, probability that spawner biomass is above Blim once")
)

# COMPOUTE yearly permformance statistics

perf_byear <- performance(runs, statistics=stats, years=2023:2041)

# First year where P(B>Blim) > 95% by MP

perf_byear[statistic == "PBlim" & data > 0.95, .SD[1], by=mp]

# PBlim by year and mp

perf_byear[statistic == "PBlim", .(PBlim=mean(data)), by=.(mp, year)]

# First year where P(B>Blim) > 95% if F=0

performance(runf0, statistics=stats, years=2023:2041)[statistic == "PBlim" & 
  data > 0.95, .SD[1]]

# 

perf_end <- performance(runs, statistics=stats)

# TABLE

perf_end[, .(value=mean(data)), by=.(mp, statistic)]

# 2-way TABLE

dcast(perf_end[, .(value=mean(data)), by=.(mp, statistic)],
  mp~statistic)


# --- TRACKING

tracking(ices)

# P(SB_2022 < Blim)

sum(tracking(ices)['SB.om','2022'] < icespts$Blim) / 500

# P(SB < Blim)
iterSums(tracking(ices)['SB.om',] / icespts$Blim < 1) / 500
iterSums(ssb(ices) / icespts$Blim < 1) / 500

# P(SB(om) > Btrigger) 

iterSums((tracking(ices)['SB.om',] / icespts$Btrigger) < 1) / 500
iterSums((tracking(ices)['SB.est',] / icespts$Btrigger) < 1) / 500

tracking(ices)['hcr',]

#
