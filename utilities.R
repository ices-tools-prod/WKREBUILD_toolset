# utilities.R - 
# WKREBUILD_toolset/utilities.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# PARALLEL setup via doFuture

library(doFuture)
options(doFuture.rng.onMisuse="ignore")

# Linux
if(os.linux() | os.macos()) {
  plan(multicore, workers=cores)
# Windows
} else if(os.windows()) {
  plan(multisession, workers=cores)
}

registerDoFuture()

# SETUP progress bars

library(progressr)
handlers(global=TRUE)
handlers("txtprogressbar")

# icesControl {{{

#' Creates an `mpCtrl` object for the standard ICES MP evaluation
#'
#' This function simplifies the construction of the control object necessary to
#' run the ICES advice rule, either in its standard form and for rebuilding
#' management plans. It returns an object of class `mpCtrl` containing three
#' modules: stock assessment (*sa*), harvest control rule (*hcr*) and
#' implementation system (*isys*).
#'
#' An stock assessment shortcut is invoked which obtains an observation of SSB
#' from the `FLStock` object returned by the `oem` step. Deviances in this
#' 'estimate' of abundance (`SSBdevs`) are added as a multiplicative error.
#' 
#' ICES advice rule, as implementated by function `hockeystick.hcr`, is setup
#' with the provided operational points: *Btrigger*, *Blim*, *Ftarget* and 
#' *Fmin*. The advice rule uses and input the SSB indicator provided by the
#' `shortcut.sa` module, and return a proposal for a fishing mortality (*fbar*)
#' to be applied in the first management year.
#'
#' A short-term forecast (STF) is the carried out to establish stock status at 
#' the end of the intermediate year, if management lag is one year or greater.
#' In  this case, the stock provided by the estimation step is projected by
#' applying on the current year ('ay') a fishing mortality equal to that
#' estimated in the previous year ('Fsq', or F status quo), and then for the
#' fishing mortality returned by the 'hcr' step on the first advice year. The 
#' resulting catch is output as TAC proposal for the next management year.
#'
#' Recruitment in the STF projection is assumed to be equal to a geometric mean
#' of those estimated in the past, with the last two years not included by
#' default. The `recyrs` argument can take various forms. A single positive 
#' value (e.g. `recyrs=30`) indicates how many years from the last are used in 
#' the calculation. A vector of length two is used to set the number of years
#' and how of them at the end of the series to exclude. So `recyrs=c(40, 2))`
#' will compute the mean over the last 40 years but exclude the last two. To use
#' the full time series but exclude a number of values at the end, a negative
#' number can be used, e.g. `recyrs=-2`.
#'
#' @param SSBdevs
#' @param Fdevs
#' @param Btrigger
#' @param Blim
#' @param Ftarget
#' @param Fmin
#' @param recyrs Specifies years to be used for geometric mean recruitment calculation applied in the short-term forecast. See **Details** for syntax.
#' @param dtaclow
#' @param dtacupp
#' @seealso mse::hockeystick.hcr, mse::tac.is, mse::shortcut.sa

icesControl <- function(SSBdevs, Fdevs, Btrigger, Ftarget, Blim=0, Fmin=0,
  recyrs=-2, dtaclow=NA, dtacupp=NA) {

  # CHECK inputs

  # BUILD mseCtrl
  ctrl <- mpCtrl(list(

  # shortcut.sa + SSBdevs
  est = mseCtrl(method=shortcut.sa,
    args=list(SSBdevs=SSBdevs)),

  # hockeystick
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=Blim, trigger=Btrigger, target=Ftarget, min=Fmin,
    metric="ssb", output="fbar")),

  # tac.is + Fdevs
  isys = mseCtrl(method=tac.is,
    args=list(recyrs=recyrs, fmin=Fmin, dtaclow=dtaclow, dtacupp=dtacupp,
    Fdevs=Fdevs))
  ))

  return(ctrl)
} 
# }}}

# shortcut_devs {{{

# GET new Fphi, Fcv values.

shortcut_devs <- function(om, Fcv=0.212, Fphi=0.423, SSBcv=0, SSBphi=0) {
  
  devs <- FLQuants(
    F=ar1rlnorm(Fphi, dimnames(om)$year, dims(om)$iter, 0, Fcv),
    SSB=ar1rlnorm(0, dimnames(om)$year, dims(om)$iter, 0, SSBcv)
  )

  return(devs)
}
# }}}

# icesmetrics {{{
icesmetrics <- list(
  FMSY=function(x) unitSums(fbar(x)) %/% refpts(x)$Fmsy,
  SBMSY=function(x) unitSums(ssb(x)) %/% refpts(x)$Btrigger,
  SBPA=function(x) unitSums(ssb(x)) %/% refpts(x)$Bpa,
  SBlim=function(x) unitSums(ssb(x)) %/% refpts(x)$Blim
)
# }}}

# performance statistics {{{

annualstats <- list(

  # P(SB>SBlim)
  PBlim=list(~iterMeans((SB/Blim) > 1), name="P(SB>SB[lim])",
    desc="Probability that spawner biomass is above Blim"),

  # P(SB>SBtrigger)
  PBtrigger=list(~iterMeans((SB/Btrigger) > 1), name="P(SB>B[trigger])",
    desc="Probability that spawner biomass is above Btrigger"),

  # mean(C)
  C=list(~iterMeans(C), name="mean(C)",
    desc="Mean catch over years"),

  # cv(C)
  cvC=list(~sqrt(iterVars(C)) / iterMeans(C), name="cv(C)",
    desc="CV of catch over years")
)

stats <- list(

  # mean(C)
  C=list(~yearMeans(C), name="mean(C)",
    desc="Mean catch over years"),

  # AVVC
  # AVVC
  AAVC=list(~yearMeans(abs(C[, -1] - C[, -dim(C)[2]])/C[, -1]),
    name="AAV(C)", desc="Average annual variability in catch"),

  # P(SB < SBlim) at least once
  risk2=list(~yearMeans(iterMeans(((SB/Blim) < 1) > 0)),
    name="once(P(SB<B[limit]))",
    desc="ICES Risk 2, probability that spawner biomass is above Blim once")
)

# }}}

# retroErrorByAge {{{

retroErrorByAge <- function(retros, object) {

  # GET dims
  nret <- length(retros) - 1
  fy <- dims(retros[[1]])$maxyear

  # COMPUTE errors: N=stock.n, F=harvest
  dat <- do.call(rbind, lapply(seq(2, nret), function(y) {
  model.frame(FLQuants(
    N=stock.n(retros[[1]])[, ac(seq(fy - nret, fy - y + 1))] /
      stock.n(retros[[y]])[, ac(seq(fy - nret, fy - y + 1))],
    F=harvest(retros[[1]])[, ac(seq(fy - nret, fy - y + 1))] /
      harvest(retros[[y]])[, ac(seq(fy - nret, fy - y + 1))]))
  }))

  # CALCULATE covariance with age
  cab <- cov(dat[, c('age', 'N')])

  # COMPUTE mean and sd by age
  musig <- data.table(dat)[, .(mean=mean(N), sd=sd(N)), by=age]

  nsamp <- length(object[1,])

  out <- Reduce(qbind, FLQuants(lapply(musig$age, function(x) {
    FLQuant(rnorm(nsamp, musig[age == x]$mean, musig[age == x]$sd),
      dimnames=list(age=x, year=dimnames(object)$year))
  })))

  return(out)
}
# }}}

# include_graphics {{{
include_graphics <- function(path, ...) {
  if(!grepl("/", path) && !grepl("report$", getwd()) && dir.exists("report"))
    path <- file.path("report", path)
  knitr::include_graphics(path, ...)
}
# }}}

table_srmodels <- function(x) {

  tab <- table(x$m)
  mod <- c("Bevholt:", "Ricker:", "Segreg:")[as.numeric(names(tab))]
  val <- round(c(tab) / sum(tab), 2)

  paste(mod, val, collapse="\n")
}

# plot_bootstrapSR {{{

plot_bootstrapSR <- function(fits, params) {

  # CREATE ssb vector for range
  ssbs <- FLQuant(seq(1, max(ssb(fits[[1]])), length=100))
  
  # PREDICT rec at ssbs 
  recs <- predict(predictModel(params=srpars, model=mixedsrr()$model),
    ssb=ssbs)

  # ADD error
  recs <- exp(log(recs) + rnorm(500, recs %=% 0, sd=(c(srpars$sigmaR))))

  # CREATE df for plotting
  dat <- model.frame(FLQuants(rec=recs, ssb=ssbs), drop=TRUE)
  dat <- data.table(subset(dat, rec <= max(rec(run)) * 1.5))

  # PLOT fits
  plotsrs(fits[c("bevholt", "segreg")]) +
    annotate("text", x=-Inf, y=Inf, hjust = -0.2, vjust = 1.5,
      label=table_srmodels(srpars)) +
    # QUANTILE smoothers
    geom_smooth(data=dat[, .(rec=quantile(rec, 0.50)), by=ssb],
      colour="black", fill=NA, linewidth=0.5,
      method='loess', formula=y~x, se=FALSE) +
    geom_smooth(data=dat[, .(rec=quantile(rec, 0.05)), by=ssb],
      colour="black", fill=NA, linewidth=0.5, linetype=2,
      method='loess', formula=y~x, se=FALSE) +
    geom_smooth(data=dat[, .(rec=quantile(rec, 0.95)), by=ssb],
      colour="black", fill=NA, linewidth=0.5, linetype=2,
      method='loess', formula=y~x, se=FALSE) -
    # POINTS
    geom_point(data=dat, aes(x=jitter(ssb, 2), rec), 
      colour="gray", fill="white", alpha=0.1, size=0.5) +
    theme(legend.position="none")
}
# }}}

# Roxygen template {{{

#' Header
#'
#' Definition ...
#'
#' @param NAME Description, class
#'
#' @return RETURN Description, class
#'
#' @name FUNCTION
#' @rdname FUNCTION
#' @aliases FUNCTION
#'
#' @author WMR (2023)
#' @seealso [FL-class] [function()] [pkg::function()]
#' @keywords classes
#' @examples
#'

# }}}
