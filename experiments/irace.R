

library("mlr")
library("ecr")
library("mlrCPO")
library("magrittr")
devtools::load_all("..")

spheretask <- create.hypersphere.data(3, 200, radius = 1) %>%
  create.classif.task(id = "sphere") %>%
  task.add.permuted.cols(5)

## lrn <- makeLearner("classif.ksvm", predict.type = "prob")
##
## lrn.ps <- pSS(
##   C: numeric[10^(-3), 10^3], # according to Fröhlich et al.
##   sigma: numeric[10^(-3), 10^3]
## )

lrn <- makeLearner("classif.rpart", maxsurrogate = 0)
lrn.ps <- pSS(
  maxdepth: integer[1, 30],
  minsplit: integer[2, 30],
  cp: numeric[0.001, 0.999])

efun <- constructEvalSetting(
    spheretask, lrn, lrn.ps, measure = mlr::mmce, evals = 1e4,
    savedir = ".")

input <- sampleValue(getParamSet(efun), discrete.names = TRUE, trafo = TRUE)

# rl <- readLines("/projects/user/mosmafstraces/run_22.out")
# input <- eval(parse(text = rl[1]))


# parallelMap::parallelStart
input <- valuesFromNames(getParamSet(efun), input)

returned <- efun(input)

