# utilities.R - DESC
# /home/mosquia/Active/Doing/toolset_ICES_WKREBUILD2+rebuild/WKREBUILD_toolset/utilities.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


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

# update(FLStock, ...) {{{

setMethod("update", signature(object="FLStock"),
  function(object, ...) {

    res <- callNextMethod()

    slots <- names(list(...))

    # RECALCULATE aggregates
    if(!"landings" %in% slots)
      landings(res) <- computeLandings(res)
    if(!"discards" %in% slots)
      discards(res) <- computeDiscards(res)
    if(!"catch" %in% slots)
      catch(res) <- computeCatch(res)
    if(!"stock" %in% slots)
      stock(res) <- computeStock(res)

    return(res)
  }
)
# }}}
