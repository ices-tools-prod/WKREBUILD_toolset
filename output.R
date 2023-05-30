# output.R - Performance evaluation
# WKREBUILD_toolset/output.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# TODO: USE names in years list, e.g. list(short=2000:2005, long=2000:2030)


library(icesTAF)
mkdir("output")

library(mse)

# LOAD model.R outputs

load("model/model_runs.rda")

# ADD F=0 run
runs$F0 <- FLmse(om=runf0)

# COMPUTE yearly performance statistics

perf_year <- performance(runs, statistics=annualstats, years=2023:2041)

# COMPUTE final performance statistics (2035-2041)

perf_end <- performance(runs, statistics=stats, years=list(2035:2041))

# SAVE

save(perf_year, perf_end, file="output/output.rda")
