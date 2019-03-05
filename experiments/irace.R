


devtools::load_all("..")

spheretask <- create.hypersphere.data(3, 200, radius = 1) %>%
  create.classif.task(id = "sphere") %>%
  task.add.permuted.cols(5)

lrn <- makeLearner("classif.ksvm", predict.type = "prob")

lrn.ps <- pSS(
  C: numeric[10^(-3), 10^3], # according to Fröhlich et al.
  sigma: numeric[10^(-3), 10^3]
)

lrn <- makeLearner("classif.rpart", maxsurrogate = 0)

efun <- constructEvalSetting(
    spheretask, lrn, lrn.ps, measure = mlr::auc, evals = 1e3)

input <- sampleValue(getParamSet(efun), discrete.names = TRUE, trafo = TRUE)
input <- valuesFromNames(getParamSet(efun), input)

returned <- efun(input)


parallelMap::parallelStop()
