
source("run_me_first.R")

## Solution.
library(metafor)
data("dat.konstantopoulos2011")

## Use a shorter name for the dataset by creating a copy.
dk <- dat.konstantopoulos2011

## Solution.
hist(dk$yi)

## Solution.
(dk_rma <- rma(yi, vi, method = "REML", data = dk))

## Solution.
forest(dk_rma)

## Solution.
(dk_rma_ordered <- rma(yi, vi, method = "REML", data = dk))

## Solution.
forest(dk_rma_ordered, order = "obs")

## Solution.
dk_fem <- rma(yi = yi, vi = vi, method = "FE", data = dk)
funnel(dk_fem, xlab = "d")

## Solution.
regtest(dk_fem, model = "lm")

## Solution.
(dk_rma_tf <- trimfill(dk_rma))

## Solution.
funnel(dk_rma_tf)

## Solution.
rma(yi ~ year, vi, method = "REML", data = dk)

## Solution.
plot(dk$year, dk$yi, xlab = "Year", ylab = "d")

## Solution.
influence(dk_rma)

## Solution.
plot(influence(dk_rma))

## Solution.
(rma_ml <- rma.mv(yi, vi, random = ~ 1 | district/study, data = dk))

## Solution.
round(rma_ml$sigma2[1] / sum(rma_ml$sigma2), 3)

## Solution.
profile(rma_ml, progbar = FALSE)

## Solution.
(rma_ml <- rma.mv(yi ~ year, vi, random = ~ 1 | district/study, data = dk))
