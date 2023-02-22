# report.R - DESC
# /home/mosquia/Active/toolset_ICES_WKREBUILD2+rebuild/WKREBUILD_toolset/report.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mseviz)


# PLOT HCRs

# PLOT RUNS

# PLOT


plot_hockeystick.hcr(arule$hcr,
  labels=c(trigger="Btrigger", target="Ftarget")) +
  xlab("SSB") + ylab(expression(bar(F)))

plot_hockeystick.hcr(rrule$hcr,
  labels=c(trigger="Btrigger", target="Ftarget")) +
  xlab("SSB") + ylab(expression(bar(F)))






# --- PDF

pdf(file="report/demo.pdf")

# ICES arule

plot_hockeystick.hcr(arule$hcr, labels=c(trigger="Btrigger",
  target="Ftarget")) + 
  xlab(expression(hat(SSB))) + ylab("F")

plot(om, ices)

# REBUILD, lim=Blim

plot_hockeystick.hcr(rrule$hcr, labels=c(trigger="Btrigger",
  target="Ftarget", lim="Blim", min="Fmin")) + 
  xlab(expression(hat(SSB))) + ylab("F")

plot(om, AR=ices, rebuild=rbld)

# RUNS w/ diff slopes

plot(om, runs)

dev.off()
# ---



