# report.R - DESC
# WKREBUILD_toolset/report.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

library(icesTAF)
mkdir("report")

library(mse)
library(FLSRTMB)
library(mseviz)
library(scales)

source("utilities.R")

# --- OM (data.R)

# SRR fits and bootstrap

load('data/bootstrap.rda')

taf.png(file="data_srrfits.png")
plotsrs(fits)
dev.off()

taf.png(file="data_srbootstrap.png", width=1400)
plot_bootstrapSR(fits, srpars)
dev.off()

# OM

load('data/data.rda')

taf.png("om_metrics.png")
plot(window(om, end=2023)) +
  ggtitle("sol.27.4 OM")
dev.off()

# OM /refpts
taf.png("om_icesmetrics.png")
plot(window(om, end=2023), metrics=icesmetrics) +
  ggtitle("sol.27.4 OM") +
  geom_hline(yintercept=1, linetype=2)
dev.off()


# --- MPs (model.R)

load("model/model.rda")

# BASELINE run: F=0
taf.png("run_f0.png")
plot(runf0) +
  geom_vline(xintercept=2023, linetype=3)
dev.off()

# ADVICE rule run
taf.png("model_advice_relative.png")
plot(om, advice, metrics=icesmetrics) +
  geom_hline(yintercept=1, linetype=2, alpha=0.5)
dev.off()

# PLOT RUNS
taf.png("plans.png")
plot(runf0, plans, window=FALSE) +
  geom_vline(xintercept=2023, linetype=3)
dev.off()

taf.png("plans_fr.png")
plot(runf0, plans_fr, window=FALSE) +
  geom_vline(xintercept=2023, linetype=3)
dev.off()

# PLOT HCRs
taf.png("runs_hcrs.png")
Reduce('+', Map(function(x, y)
  plot_hockeystick.hcr(control(x)$hcr,
  labels=c(trigger="Btrigger", target="Ftarget")) +
  xlab("SSB") + ylab(expression(bar(F))) +
  ggtitle(y), x=plans[1:4], y=names(plans[1:4])))
dev.off()

# --- Performance (output.R)

load("output/output.rda")

# PLOT long term performance
taf.png("perf_bps.png")
plotBPs(perf[year=='all']) + ylim(c(0, NA))
plotBPs(perf[year=='long']) + ylim(c(0, NA))
dev.off()

plotBPs(perf[year=='short'], statistics=c("AAVC", "C", "risk2")) +
  ylim(c(0, NA))

plotBPs(perf[year=='medium'], statistics=c("AAVC", "C", "risk2")) +
  ylim(c(0, NA))

# PLOT trade-offs

taf.png("perf_tos.png")
plotTOs(perf[year=='short'], x="C", y=c("AAVC", "risk2"))
dev.off()

# PLOT PBlim by year and mp

dat <- perf_year[statistic == "PBlim", .(PBlim=mean(data)), by=.(mp, year)]

pubpng("perf_pblim_mp.png",
ggplot(dat, aes(x=year, y=PBlim, group=mp, colour=mp)) +
  geom_line(linewidth=0.5) +
  geom_point(size=4, colour="white") + geom_point(size=2) +
  geom_hline(yintercept=0.95, linetype=2)
)

# PLOT PBtrigger by year and mp

dat <- perf_year[statistic == "PBtrigger", .(PBtrigger=mean(data)), by=.(mp, year)]

pubpng("perf_pbtrigger_mp.png",
ggplot(dat, aes(x=year, y=PBtrigger, group=mp, colour=mp)) +
  geom_line(linewidth=0.5) +
  geom_point(size=4, colour="white") + geom_point(size=2) +
  geom_hline(yintercept=0.50, linetype=2)
)

# RENDER report.Rmd

# rmarkdown::render("report.Rmd", output_dir="report")
