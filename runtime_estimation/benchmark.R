## Rough Runtime Estimation

library(batchtools)

registry_name = "registry"
OVERWRITE = FALSE

if (file.exists(registry_name)) {
  if (OVERWRITE) {
    unlink(registry_name, recursive = TRUE)
    reg = makeRegistry(file.dir = registry_name, seed = 123L,
      packages = c("mlr", "data.table"), conf.file = ".batchtools.conf.R", source = "def_runtime_eval.R")
  } else {
    reg = loadRegistry(registry_name, writeable = TRUE)
  }
} else {
    reg = makeRegistry(file.dir = registry_name, seed = 123L,
      packages = c("mlr", "data.table"), conf.file = ".batchtools.conf.R", source = "def_runtime_eval.R")
}


ids = batchMap(function(i) {

  cv.iters = 10L 

  arg = args[i, ]

  lrn = makeLearner("classif.xgboost", id = "classif.xgboost", eval_metric = "error", objective = "binary:logistic")

  instance = readDataAndRinst(paste("../benchmark/data/", arg$dataset, sep = ""), 1)

  train.task = instance$train.task # training
  test.task = instance$test.task # for outer evaluation

  inner = makeResampleDesc("CV", iters = cv.iters, stratify = TRUE)

  ps = makeParamSet(
      makeNumericParam("eta", lower = 0.01, upper = 0.2),
      makeNumericParam("gamma", lower = -7, upper = 6, trafo = function(x) 2^x),
      makeNumericParam("colsample_bytree", lower = 0.5, upper = 1),
      makeNumericParam("colsample_bylevel", lower = 0.5, upper = 1),
      makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x) 2^x),
      makeNumericParam("alpha", lower = -10, upper = 10, trafo = function(x) 2^x),
      makeNumericParam("subsample", lower = 0.5, upper = 1)
  )

  x = trafoValue(ps, sampleValues(ps, 1)[[1]])
  x$nrounds = arg$nrounds
  x$max_depth = arg$max_depth

  if (arg$early_stopping) {
    x$early_stopping_rounds = 10L
  }

  configureMlr(show.info = TRUE, show.learner.output = TRUE)

  lrn2 = setHyperPars2(lrn, par.vals = x)

  p = getTaskNFeats(train.task)

  time = system.time({
    
    filtered.train.task = subsetTask(train.task, features = 1:round(arg$per_feats / 100 * p)) 
    filtered.test.task = subsetTask(test.task, features = 1:round(arg$per_feats / 100 * p)) 

    lrn2 = setHyperPars2(lrn, par.vals = x)
        
    res = resample(lrn2, filtered.train.task, inner, show.info = TRUE)$aggr
  })  
  
  return(time)

}, 1:nrow(args))

ids[, chunk := chunk(job.id, chunk.size = 9)]

submitJobs(ids, resources = resources.serial)


library(batchtools)

reg = loadRegistry("test", writeable = FALSE)

res = reduceResultsDataTable(fun = function(x) x[1] / 60)
res$result = unlist(res$result)

args$job.id = 1:nrow(args)

res = ijoin(args, res, by = "job.id")

dir.create("")

saveRDS(res, "res_runtime_AP_Breast_Colon.rds")
