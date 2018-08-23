
## source("run_me_first.R")

## Solution.
library(metafor)
dat.bcg <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
res.fem <- rma(yi = yi, vi = vi, method = "FE", data = dat.bcg)
funnel(res.fem, xlab = "log(RR)")

## Solution.
library(metafor)
dat.bcg <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
summary(lm(I(yi/sqrt(vi)) ~ I(1/sqrt(vi)), data = dat.bcg))

## Solution.
library(metafor)
dat.bcg <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
res.fem <- rma(yi = yi, vi = vi, method = "FE", data = dat.bcg)
regtest(res.fem, model = "lm")

## Calculate the SND and the precision
snd <- with(dat.bcg, yi/sqrt(vi))
prec <- with(dat.bcg, 1/sqrt(vi))
## Then create a new data frame called dER
dER <- data.frame(snd, prec)
## Next, we need to run the Egger's regression test
egg.reg <- lm(snd ~ prec, data = dER)
summary(egg.reg)

## To add confidence lines to the plot, we are using the
## predict function.
x.new <- seq(0, 20, 0.1)
egg.reg.pred <- predict(egg.reg,
newdata = data.frame(prec = x.new),
interval="confidence")

## Then, create the plot
plot(prec, snd)
abline(egg.reg)
lines(x.new, egg.reg.pred[ ,3], lty = 'dashed', col = 'red')
lines(x.new, egg.reg.pred[ ,2], lty = 'dashed', col = 'red')

## Solution.
library(metafor)
dat.bcg <- escalc(measure = "RR", ai = tpos, bi = tneg, ci = cpos, di = cneg, data = dat.bcg)
res.rem <- rma(yi = yi, vi = vi, method = "REML", data = dat.bcg)

## Solution.
res.fem <- rma(yi = yi, vi = vi, method = "FE", data = dat.bcg)
res.rem
(res.fem.trimfill1 <- trimfill(res.fem))

## Solution.
funnel(res.fem.trimfill1)
