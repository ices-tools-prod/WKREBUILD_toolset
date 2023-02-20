# output.R - DESC
# /home/mosquia/Active/Doing/toolset_ICES_WKREBUILD2+rebuild/WKREBUILD_toolset/output.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# TODO: USE names in years list, e.g. list(short=2000:2005, long=2000:2030)

library(mse)

data(statistics)

load("model/runs.RData")

# DEFINE performance statistics

stats <- list(
  PBlim=list(~iterMeans((SB/Blim) > 1), name="P(SB>SB[lim])",
    desc="Probability that spawner biomass is above Blim"),
  C=list(~yearMeans(C), name="mean(C)",
    desc="Mean catch over years")
)

# CALCULATE recovery, P(B>Blim) > 95%

perf_byear <- performance(runs, statistics=stats, years=2022:2041)

# First year where P(B>Blim) > 95% by MP

perf_byear[data > 0.95, .SD[1], by=mp]

# 

perf_end <- performance(runs, statistics=stats)

# TABLE
perf_end[, .(value=mean(data)), by=.(mp, statistic)]

# 2-way TABLE
dcast(perf_end[, .(value=mean(data)), by=.(mp, statistic)],
  mp~statistic)


# --- TRACK

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

