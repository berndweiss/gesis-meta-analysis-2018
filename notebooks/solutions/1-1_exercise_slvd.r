
source("run_me_first.R")

## Solution.
13*17*19

## Ave Caesar, morituri te salutant.

## Solution.
library(metafor)

## Remove the ## in the next line to execute the help command. 
## ?dat.bcg 
trial	numeric	trial number
author	character	author(s)
year	numeric	publication year
tpos	numeric	number of TB positive cases in the treated (vaccinated) group
tneg	numeric	number of TB negative cases in the treated (vaccinated) group
cpos	numeric	number of TB positive cases in the control (non-vaccinated) group
cneg	numeric	number of TB negative cases in the control (non-vaccinated) group
ablat	numeric	absolute latitude of the study location (in degrees)
alloc	character	method of treatment allocation (random, alternate, or systematic assignment)
dat.bcg <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)

## Solution.
library(metafor)
head(dat.bcg)

head(dat.bcg)

## Solution.
mean(dat.bcg$yi)
var(dat.bcg$yi)

## Solution.
m <- mean(dat.bcg$yi)
v <- var(dat.bcg$yi)

## Solution.
se <- sqrt(v)
se_2 <- sqrt(var(dat.bcg$yi))
se
se_2

## Solution.
se^2

## Solution.
hist(dat.bcg$yi)

## Solution.
cor(dat.bcg$yi, dat.bcg$ablat)

## Solution.
summary(lm(yi ~ ablat, data = dat.bcg))
