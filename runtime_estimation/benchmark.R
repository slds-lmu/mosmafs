## Rough Runtime Estimation

library(batchtools)

registry_name = "registry"
OVERWRITE = FALSE

if (file.exists(registry_name)) {
  if (OVERWRITE) {
    unlink(registry_name, recursive = TRUE)
    reg = makeRegistry(file.dir = registry_name, seed = 123L,
      packages = c("mlr", "data.table"), conf.file = ".batchtools.conf.R", source = "def.R")
  } else {
    reg = loadRegistry(registry_name, writeable = TRUE)
  }
} else {
    reg = makeRegistry(file.dir = registry_name, seed = 123L,
      packages = c("mlr", "data.table"), conf.file = ".batchtools.conf.R", source = "def.R")
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

ids[, chunk := chunk(job.id, chunk.size = 45)]

resources.serial = list(
  walltime = 3600L * 96L, memory = 1024L * 4L,
  clusters = "serial", max.concurrent.jobs = 1000L # get name from lrz homepage)
)

submitJobs(ids, resources = resources.serial)


library(batchtools)

reg = loadRegistry(registry_name, writeable = FALSE)

res = reduceResultsDataTable(fun = function(x) x[1] / 60)
res$result = unlist(res$result)

args$job.id = 1:nrow(args)

res = ijoin(args, res, by = "job.id")

perf = lapply(res$job.id, function(x) {
  file = getLog(x)
  blub = lapply(file, function(z) {
    if (grepl("mmce.test.mean", z)) {
      as.numeric(strsplit(z, "mmce.test.mean=")[[1]][2])
    }
  })
  blub[lengths(blub) != 0][[1]]
})

perf = do.call(rbind, perf)
res$perf = perf

dir.create("results")

saveRDS(res, "results/runtime_results.rds")

## 

# visualize

library(ggplot2)
library(data.table)

res = readRDS("results/runtime_results.rds")
res = setDT(res)
res$nrounds = as.character(res$nrounds)
res[early_stopping == TRUE, ]$nrounds = "earlystop"
res$nrounds = as.factor(res$nrounds)

p = ggplot(data = res, aes(x = nrounds, y = result)) + geom_boxplot()
p = p + facet_grid(rows = vars(per_feats), cols = vars(dataset))
p

p = ggplot(data = res, aes(x = nrounds, y = perf)) + geom_boxplot()
p = p + facet_grid(rows = vars(per_feats), cols = vars(dataset))
p

ggsave(p, file = "results/perf_datasets.pdf")

