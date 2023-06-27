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

source("utilities.R")

# LOAD model.R outputs

load("model/model.rda")

# ADD runf0 for performance calculations TODO: SIMPLIFY

runs <- c(runs, F0=FLmse(om=runf0))

# COMPUTE yearly performance statistics

perf_year <- performance(runs, statistics=annualstats, years=2023:2041)

# COMPUTE performance statistics (2024-2041)

perf <- performance(runs, statistics=stats, years=list(2024:2041))

# TODO:

# COMPUTE first year where P(SB>=SBlim) >= 0.95
perf_year[statistic == 'PBlim', .(PBlim=mean(data)), by=.(year, mp)][PBlim > 0.95, .(first=min(year)), by=mp]

# TODO:
compute(perf_year, min(year), where=PBlim > 0.95) 

perf_year[statistic == 'PBlim' & year == max(year), mean(data), by=.(mp, year)]

# mp ~ year | C

dcast(perf_year[statistic == 'C', .(data=mean(data)),
  by=.(year, mp, name, desc)], mp ~ year)

# statistic + mp ~ year

dcast(perf_year[, .(data=mean(data)), by=.(year, mp, name, desc)], name + mp ~ year)

# SAVE

save(perf_year, perf_end, file="output/output.rda")
