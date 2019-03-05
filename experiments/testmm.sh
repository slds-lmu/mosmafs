#!/bin/sh
export RNG=$1
Rscript - > "/projects/user/mosmafstraces/run_${RNG}.out" 2>&1 <<EOF
options(warn = 1)
suppressMessages({
library("mlr")
library("ecr")
library("mlrCPO")
library("magrittr")
library("mosmafs")
})
set.seed(as.numeric(Sys.getenv("RNG")))
spheretask <- create.hypersphere.data(3, 200, radius = 1) %>%
  create.classif.task(id = "sphere") %>%
  task.add.permuted.cols(5)

lrn <- makeLearner("classif.ksvm", predict.type = "prob")

lrn.ps <- pSS(
  C: numeric[10^(-3), 10^3], # according to Fröhlich et al.
  sigma: numeric[10^(-3), 10^3]
)


lrn <- makeLearner("classif.rpart", maxsurrogate = 0)
lrn.ps <- pSS(
  maxdepth: integer[1, 30],
  minsplit: integer[2, 30],
  cp: numeric[0.001, 0.999])
efun <- constructEvalSetting(
    spheretask, lrn, lrn.ps, measure = mlr::mmce, evals = 1e4,
    savedir = "/projects/user/mosmafstraces")

input <- sampleValue(getParamSet(efun), discrete.names = TRUE, trafo = TRUE)
BBmisc::catf("list(%s)\n", BBmisc::collapse(sprintf("%s = %s", names(input), BBmisc::vcapply(input, deparse, nlines = 1, width.cutoff = 500)), ", "))
input <- valuesFromNames(getParamSet(efun), input)
print(digest::digest(input))
print(system.time(res <- efun(input)))
print(res)
q(save = "no", status = 111)
EOF
exit $?
