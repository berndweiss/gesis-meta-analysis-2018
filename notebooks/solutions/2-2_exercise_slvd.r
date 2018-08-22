
source("run_me_first.R")

## Solution.

library(metafor)
dat.bcg <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)

## First, we need to calculate the weights w
wi <- 1/dat.bcg$vi

## Next, calculate the weighted mean
ma.fem <- sum(wi * dat.bcg$yi) / sum(wi)
ma.fem

## Solution.
## The variance can be determined as follows:
var.ma.fem <- 1/sum(wi)
var.ma.fem

## Solution.
## ... and the standard error is
se.ma.fem <- sqrt(var.ma.fem)
se.ma.fem

## Solution.
## Finally, we can compute the confidence intervall:
ma.fem.ll <- ma.fem - 1.96 * se.ma.fem
ma.fem.ul <- ma.fem + 1.96 * se.ma.fem
ma.fem.ll

## Solution.
ma.fem.ul

## Solution.
library(metafor)
dat.bcg <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
rma(yi = yi, vi = vi, method = "FE", data = dat.bcg)

## Solution.
library(metafor)
dat.bcg <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
ma.rem <- rma(yi = yi, vi = vi, method = "DL", data = dat.bcg)
ma.rem

## Solution.
## Here is another ways to directly include the study outcomes without using escalc().
library(metafor)
rma.out <- rma(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, method = "DL", 
               data = dat.bcg)
rma.out

## Solution.

## Since the average effect size is a log(RR), for interpretation and publication, we need to 
## retransform it to the RR metric. 
## Source: http://www.metafor-project.org/doku.php/tips:assembling_data_or
predict(rma.out, transf = exp, digits = 2)

## Solution. 

## Another, more technical approach is to assign the result of the rma() command  
## to an object called rma.out. We now can access all the results in rma.out, for instance, 
## the the avergage effect size. The variable name of this average effect size is "b". 
## Hence, we can use:
exp(rma.out$b)
## Use str(rma.out) to learn about other statistics that can be found in the rma object. 

## Solution according to http://www.metafor-project.org/doku.php/analyses:berkey1995.
## IMPORTANT: Different metehod ("EB") applied! 
library(metafor)
dat.bcg <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
dat.bcg$vi <- with(dat.bcg, sum(tneg/tpos)/(13*(tneg+tpos)) + 
                    sum(cneg/cpos)/(13*(cneg+cpos)))
ma.rem <- rma(yi = yi, vi = vi, method = "EB", data = dat.bcg)
ma.rem

## Solution.
library(metafor)
dat.bcg <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
## First, we need to calculate the between-study variance tau^2
## (using the method-of-moments approach) (see excercise 1 for ma.fem)
Q.T <- sum((dat.bcg$yi - ma.fem)^2 / dat.bcg$vi)
Q.T

## Solution.
## Now, let's estimate tau^2
w <- 1/dat.bcg$vi
tau2 <- (Q.T - (length(dat.bcg$yi) - 1)) /( sum(wi) - (sum(wi^2)/sum(wi)) )
tau2

## Solution.
## Next, we need to calculate w*
w.star <- 1/(dat.bcg$vi + tau2)
ma.rem <- sum(dat.bcg$yi * w.star) / sum(w.star)
ma.rem

## Solution.
## Finally, we need the standard error for the RE model estimate
se.ma.rem <- sqrt(1/sum(w.star))
se.ma.rem

## Solution.
rma(yi = yi, vi = vi, method = "DL", data = dat.bcg)
