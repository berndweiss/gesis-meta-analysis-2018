
source("run_me_first.R")
trial	numeric	trial number
author	character	author(s)
year	numeric	publication year
tpos	numeric	number of TB positive cases in the treated (vaccinated) group
tneg	numeric	number of TB negative cases in the treated (vaccinated) group
cpos	numeric	number of TB positive cases in the control (non-vaccinated) group
cneg	numeric	number of TB negative cases in the control (non-vaccinated) group
ablat	numeric	absolute latitude of the study location (in degrees)
alloc	character	method of treatment allocation (random, alternate, or systematic assignment)
## Solution.
library(metafor)
dat.bcg <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)

## Solution.
dat.bcg

head(dat.bcg)

## Solution.
## yi is the newly calculated effect size, it is a log(relative risk). In terms of interpretation:
## - a negative value means that the vaccine indeed works.
## - yi = 0 means there is no effect at all, yi > 0 means the vaccine is harmful.
## vi is the corresponding variance.

## Solution.
exp(dat.bcg$yi)

## Solution.
library(metafor)
dat.bcg <- escalc(measure = "OR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
dat.bcg

## Solution.
dat.bcg <- escalc(measure = "OR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
exp(dat.bcg$yi)

## Solution.
## We are interested in verbal abilities and the other two correlation coefficient 
## refer to other subscales/subsets. 
"The first one, r = 0.234"

## Solution.
## Perform a Fisher's z transformation on r for the most general measures,

r <- 0.234
n <- 76
z.r <- 0.5 * log((1 + r)/(1 - r))
z.r

## Solution.
var.z.r <- 1 / (n - 3)
var.z.r

## Solution.
(se.z.r <- sqrt(var.z.r)) ## (...) is a shortcut for print(...)

## Solution.
## You also can use escalc() to calculate the Fisher's r-to-z transformed correlation 
## coefficient.
escalc(measure="ZCOR", ri = 0.234, ni = 76)

## Solution.
## Here is the variance and SE for the raw correlation coefficient
(var.r <- (1-r^2)^2/(n-1))
sqrt(var.r)
