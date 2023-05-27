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

# COMP base line performance
perf_f0 <- performance(runf0, statistics=stats, years=2023:2041)

# COMPUTE yearly performance statistics

perf_byear <- performance(runs, statistics=stats[-3], years=2023:2041)

# COMPUTE final performance statistics (2023-2041)

perf_end <- performance(runs, statistics=stats, years=list(2035:2041))

# SAVE

save(perf_byear, perf_end, perf_f0, file="output/output.rda")
