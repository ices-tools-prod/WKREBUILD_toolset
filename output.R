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

mses_opts <- c(mses_opts, F0=FLmse(om=runf0))

# COMPUTE yearly performance statistics

perf_year <- performance(mses_opts, statistics=annualstats, years=2023:2041)

# COMPUTE performance statistics (2024-2041)

perf_all <- performance(mses_opts, statistics=fullstats,
  years=list(all=2024:2041))

perf_periods <- performance(mses_opts, statistics=fullstats,
  years=list(short=2024:2028, medium=2028:2034, long=2034:2041, all=2024:2041))

# mp ~ year | C

dcast(perf_year[statistic == 'C', .(data=mean(data)),
  by=.(year, mp, name, desc)], mp ~ year)

# statistic + mp ~ year

dcast(perf_year[, .(data=mean(data)), by=.(year, mp, name, desc)],
  name + mp ~ year)

# SAVE

save(perf_year, perf_all, file="output/output.rda", compress="xz")
