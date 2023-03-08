# output.R - Performance evaluation
# WKREBUILD_toolset/output.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# TODO: USE names in years list, e.g. list(short=2000:2005, long=2000:2030)


library(mse)

# LOAD model.R outputs

load("model/runf0.RData")
load("model/runs.RData")
load("model/runsminfs.RData")


# ---/

# DEFINE performance statistics

icesstats <- list(

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

# COMP base line performance
perf_f0 <- performance(runf0, statistics=stats, years=2023:2041)

# COMPUTE yearly performance statistics

perf_byear <- performance(runs, statistics=stats[-3], years=2023:2041)

# COMPUTE final performance statistics (2023-2041)

perf_end <- performance(runs, statistics=stats, years=list(2035:2041))

# SAVE

save(perf_byear, perf_end, perf_f0, file="output/performance.RData")
