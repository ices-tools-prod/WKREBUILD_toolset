# report.R - DESC
# WKREBUILD_toolset/report.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mseviz)


# --- OM (data.R)

load('data/sol274.RData')

# OM
pubpng("report/om_metrics.png",
plot(window(om, end=2021)) +
  ggtitle("sol.27.4 OM + high F")
)

# OM /refpts
pubpng("report/om_icesmetrics.png",
plot(window(om, end=2021), metrics=icesmetrics) +
  ggtitle("sol.27.4 OM + high F") +
  geom_hline(yintercept=1, linetype=2)
)

# --- MPs (model.R)

load("model/runf0.RData")
load("model/runs.RData")
load("model/runsminfs.RData")

# BASELINE run: F=0
pubpng("report/run_f0.png",
plot(runf0) +
  geom_vline(xintercept=2023, linetype=3)
)

# PLOT RUNS

pubpng("report/runs.png",
plot(runf0, runs, window=FALSE) +
  geom_vline(xintercept=2023, linetype=3)
)

pubpng("report/runs_hcrs.png",
Reduce('+', lapply(names(runs), function(x)
  plot_hockeystick.hcr(control(runs[[x]])$hcr,
  labels=c(trigger="Btrigger", target="Ftarget")) +
  xlab("SSB") + ylab(expression(bar(F))) +
  ggtitle(x)))
)

# PLOT RUNS minfs

pubpng("report/runs_minfs.png",
plot(runf0, runs_minfs, window=FALSE) +
  geom_vline(xintercept=2023, linetype=3)
)

pubpng("report/runs_minfs_hcrs.png",
Reduce('+', lapply(names(runs_minfs), function(x)
  plot_hockeystick.hcr(control(runs_minfs[[x]])$hcr,
  labels=c(trigger="Btrigger", target="Ftarget")) +
  xlab("SSB") + ylab(expression(bar(F))) +
  ggtitle(x)))
)


# --- Performance (output.R)

load("output/performance.RData")

# PLOT long term performance

pubpng("report/perf_bps.png",
plotBPs(perf_end) + ylim(c(0, NA))
)

# PLOT trade-offs

pubpng("report/perf_tos.png",
plotTOs(perf_end, x="C", y=c("AAVC", "PBlim"))
)

# First year where P(B>Blim) > 95% if F=0

perf_f0[statistic == "PBlim" & data > 0.95, .SD[1]]

# First year where P(B>Blim) > 95% by MP

perf_byear[statistic == "PBlim" & data > 0.95, .SD[1], by=mp]

# PLOT PBlim by year and mp

dat <- perf_byear[statistic == "PBlim", .(PBlim=mean(data)), by=.(mp, year)]

pubpng("report/perf_pblim_mp.png",
ggplot(dat, aes(x=year, y=PBlim, group=mp, colour=mp)) +
  geom_line(linewidth=2) +
  geom_point(size=4, colour="white") + geom_point(size=3)
)
