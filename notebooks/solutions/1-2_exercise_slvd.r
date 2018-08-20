
source("run_me_first.R")

## Solution.
library(metafor)
dat.bcg

## Solution.
head(dat.bcg, n = 3)

## Solution.
names(dat.bcg)

## Solution.
dim(dat.bcg)

## Solution.
str(dat.bcg)

## This is a test... not sure that works on the server...
## fix(dat.bcg)

## Solution.
library(metafor)
dat.bcg

## Solution.
dat.bcg$trial

## Solution.
dat.bcg[, "trial"]

## Solution.
dat.bcg[1, ]

## Solution.
dat.bcg[dat.bcg$author == "Stein & Aronson", ]

## Solution.
dat.bcg[dat.bcg$year >= 1970, ]

## Solution.
dat.bcg_bf1970 <- dat.bcg[dat.bcg$year < 1970, ]
dim(dat.bcg_bf1970) ## number of rows/cases and number of columns
nrow(dat.bcg_bf1970)

## Solution.
dat.bcg[dat.bcg$year > 1970 & dat.bcg$tpos >= 100, ]

## Solution.
subset(dat.bcg, year > 1970 & tpos >= 100)
