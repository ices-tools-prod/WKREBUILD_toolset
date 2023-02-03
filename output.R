# output.R - DESC
# /home/mosquia/Active/Doing/toolset_ICES_WKREBUILD2+rebuild/WKREBUILD_toolset/output.R

# Copyright (c) WUR, 2023.
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

# XX {{{
# }}}


# SELECT performance statistics
data(statistics)


# TODO: USE names in years list, e.g. list(short=2000:2005, long=2000:2030)

performance(ices, years=list(2022:2030, 2022:2040))

# TRACK

tracking(ices)

# P(SB_2022 < Blim)

sum(tracking(ices)['SB.om','2022'] < icespts$Blim) / 500

# P(SB < Blim)
iterSums(tracking(ices)['SB.om',] / icespts$Blim < 1) / 500
iterSums(ssb(ices) / icespts$Blim < 1) / 500

# P(SB(om) > Btrigger) 

iterSums((tracking(ices)['SB.om',] / icespts$Btrigger) < 1) / 500
iterSums((tracking(ices)['SB.est',] / icespts$Btrigger) < 1) / 500

tracking(ices)['hcr',]


#

