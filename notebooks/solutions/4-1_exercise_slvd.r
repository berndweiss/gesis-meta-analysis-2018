
source("run_me_first.R")

## Solution.
library(metafor)
dat.bcg <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
fit.0 <- rma(yi = yi, vi = vi, method = "REML", data = dat.bcg)
fit.0

## Solution.
library(metafor)
dat.bcg <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
res.abl <- (rma(yi ~ ablat, vi, data = dat.bcg, digits = 3) )
res.abl

forest(res.abl)
dat.bcg

## Solution.
## This moderator has negative, statistically significant effect. That is, the colder 
## (more north or south,  it is the absolute value), the better the vaccine works. 
## At 0° (intercept), the effect is positive ($b_0 = 0.251$)

## Solution.
## The value for R^2 ("R^2 (amount of heterogeneity accounted for):") can be manually 
## calculated as follows:
(fit.0$tau2 - res.abl$tau2 )/ fit.0$tau2 

table(dat.bcg$ablat)

library(car)
dat.bcg$ablat_gr <- recode(dat.bcg$ablat, "0:33 = '[0,34)'; 24:90 = '[34,90)'")
dat.bcg$ablat_gr <- factor(dat.bcg$ablat_gr)
table(dat.bcg$ablat_gr)

## Solution.
mdl_grouped <- rma(yi ~ ablat_gr, vi, data = dat.bcg, digits = 3)
summary(mdl_grouped)

mdl_grouped <- rma(yi ~ relevel(ablat_gr, ref = "[34,90)"), vi, data = dat.bcg, digits = 3)
summary(mdl_grouped)

dat.bcg$ablat_gr2 <- relevel(dat.bcg$ablat_gr, ref = "[34,90)")
head(dat.bcg)

mdl_grouped <- rma(yi ~ ablat_gr2, vi, data = dat.bcg, digits = 3)
summary(mdl_grouped)

## Solution.
mdl3_grouped <- rma(yi ~ ablat_gr - 1, vi, data = dat.bcg, digits = 3)
summary(mdl3_grouped)



## - a negative value means that the vaccine indeed works.
## - 0 means there is no effect at all, yi > 0 means the vaccine is harmful.
###  - vaccine works better in cntries for lattitudes bw. 34 and 90 

