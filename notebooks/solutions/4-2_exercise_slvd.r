
source("run_me_first.R")

## Solution.
library(metafor)
dat.bcg <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
res <- (rma(yi, vi, data=dat.bcg, digits=3) )
res

## Solution.
inf <- influence(res)
inf

### Solution.
### Plot influence diagnostics.
plot(inf)

## Solution.
## Normal Quantile Plot: Plotting the quartiles of the effect size distribution against 
## the quartiles of the normal distribution.
qqnorm(res, type = "rstandard", pch = 19, envelope = TRUE, bonferroni = FALSE, 
       reps = 1000, smooth = TRUE, bass = 0, label = FALSE, offset = 0.3,
       main = "Normal Quantile Plot", xlab = "Normal Quantile", 
       ylab = "Standarized Effect Size")

## Solution.
## Random effects Model
## Random effects Model
library(metafor)
res <- (rma(yi, vi, data = dat.raudenbush1985, digits=3, 
            slab = paste(dat.raudenbush1985$author, ", ", 
                         dat.raudenbush1985$year)))

## Solution.
### estimate influence diagnostics
inf <- influence(res)
inf

### Interpretation ###
## Studentized residual: Rule of thumb: should not be larger than 3 -> study 4 and 10 are 
## probably outliers 
## cook's d: shouldn’t  be 3 times larger than mean -> study 10 is an outlier 
## we should exclude study 10

## Solution.
### plot the influence diagnostics
plot(inf)

## Solution.
## Normal Quantile Plot: Plotting the quartiles of the effect size distribution against 
## the quartiles of the normal distribution.
qqnorm(res, type = "rstandard", pch = 19, envelope = TRUE, bonferroni = FALSE, 
       reps = 1000, smooth = TRUE, bass = 0, label = FALSE, offset = 0.3,
       main = "Normal Quantile Plot", xlab = "Normal Quantile", 
       ylab = "Standarized Effect Size")
