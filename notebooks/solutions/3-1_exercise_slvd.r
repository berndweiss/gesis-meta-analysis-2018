
source("run_me_first.R")

## Solution.
library(metafor)
dat.bcg <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
forest(x = dat.bcg$yi, vi = dat.bcg$vi)

dat.bcg

## Solution.
ma.fem <- rma(yi = yi, vi = vi, method = "FE", data = dat.bcg)
ma.rem <- rma(yi = yi, vi = vi, method = "DL", data = dat.bcg)
# "DL"= DerSimonian and Laird method

## Okay, let's get fancy...
dat.bcg_byyear <- dat.bcg[order(dat.bcg$year),]
dat.bcg_byyear$author_year <- paste(dat.bcg_byyear$author, ", ", 
                                    dat.bcg_byyear$year)
with(dat.bcg_byyear,
     forest(x = yi, vi = vi, slab = author_year, ylim = c(-2, 16), 
            xlim = c(-8, 5.5), xlab = "Effect size log(RR)"))

## Add FE and RE model estimates (you have to add space manually via
## ylim = c(-2,22))
addpoly(ma.fem, row = -1)
addpoly(ma.rem, row = -2)
### Add column headings
text(-7, 15, "Study")
text(2.5, 15, "log(RR)")
text(4, 15, "95% CI")

## Solution.
## It is a heterogeneous effect-size distribution; some of the confidence intervals do not 
## overlap.

## Solution.
library(metafor)
dat.bcg <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
(res.dl <- rma(yi = yi, vi = vi, method = "DL", data = dat.bcg))

## Solution.
## The I^2 statistic describes the percentage of variation across studies that is due to
## heterogeneity rather than chance (similar to ICC). It's an intuitive and simple expression of
## the inconsistency of studies’ results and our study faces a large heterogeneity.

(res.dl <- rma(yi = yi, vi = vi, method = "DL", data = dat.bcg))

(res.dl <- rma(yi = yi, vi = vi, method = "REML", data = dat.bcg))
