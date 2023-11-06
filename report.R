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


# DEVIANCES
taf.png("om_deviances.png")
plot(deviances(om)) +
  ggtitle("SRR deviances ~ LNAR1(0, sigmaR, rho)")
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
  geom_vline(xintercept=2023.5, linetype=3) +
  ggtitle("fwd(F=0), 2024-2042")
dev.off()

# ADVICE rule run
taf.png("model_advice_relative.png")
plot(om, advice, metrics=icesmetrics) +
  geom_hline(yintercept=1, linetype=2, alpha=0.5)
dev.off()

# PLOT AR hockeystick

taf.png("advice_hcr.png")
plot_hockeystick.hcr(control(advice)$hcr,
  labels=c(trigger="MSYBtrigger", limit="", min="", target="Ftarget")) +
  xlab("SSB") + ylab("F")
dev.off()


# PLOT RUNS
taf.png("plans.png")
plot(runf0, plans, window=FALSE) +
  geom_vline(xintercept=2023, linetype=3) +
  ggtitle("F=0 + MPs")
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

tracking(advice)[c('SB.om', 'met.hcr', 'decision.hcr', 'hcr',
  'fbar.isys', 'isys'), ac(2024:2026)]

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

# PLOT om + plan ssb
taf.png("onruns.png")
plotOMruns(window(ssb(om), end=2023), FLQuants(lapply(plans, ssb))) +
  ggtitle("SSB (t)")
dev.off()

# PLOT PBlim by year and mp

dat <- perf_year[statistic == "PBlim", .(PBlim=mean(data)), by=.(mp, year)]

taf.png("perf_pblim_mp.png")
ggplot(dat, aes(x=year, y=PBlim, group=mp, colour=mp)) +
  geom_line(linewidth=0.5) +
  geom_point(size=4, colour="white") + geom_point(size=2) +
  geom_hline(yintercept=0.95, linetype=2)
dev.off()

# PLOT PBtrigger by year and mp

dat <- perf_year[statistic == "PBtrigger", .(PBtrigger=mean(data)), by=.(mp, year)]

taf.png("perf_pbtrigger_mp.png")
ggplot(dat, aes(x=year, y=PBtrigger, group=mp, colour=mp)) +
  geom_line(linewidth=0.5) +
  geom_point(size=4, colour="white") + geom_point(size=2) +
  geom_hline(yintercept=0.50, linetype=2)
dev.off()

# RENDER tutorial.Rmd as tutorial.html

rmarkdown::render('tutorial.Rmd', output_file='tutorial.html')
