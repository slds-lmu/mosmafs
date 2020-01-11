# --- TESTS IF BENCHMARK IS CONFIGURED CORRECTLY

library(batchtools)

reg = loadRegistry("registry_temp", writeable = TRUE)

testdata = "sonar"

tab = summarizeExperiments(by = c("job.id", "algorithm", 
	"problem", "learner", "maxeval", "filter", "initialization", 
	"lambda", "mu", "parent.sel", "chw.bitflip", "adaptive.filter.weights",
	"filter.during.run", "surrogate", "infill", "propose.points", "maxtime", 
  "multi.objective"))
tab = tab[problem %in% testdata, ]


# --- TEST RANDOMSEARCH

tosubmit = tab[algorithm %in% "randomsearch", ]

# uniform vs. non-uniform 
binom = testJob(tosubmit[1, ])
unif = testJob(tosubmit[2, ])

sapply(binom$result$last.population, function (x) mean(x$selector.selection))
sapply(unif$result$last.population, function (x) mean(x$selector.selection))
	
# filter initialization
res = testJob(tosubmit[5, ]) # filter 
# expect 1 for sonar task
mean(sapply(res$result$last.population, function (x) x$selector.selection[1]))

# filter + nonuniform initialization 
res = testJob(tosubmit[6, ])
sapply(res$result$last.population, function (x) mean(x$selector.selection))
mean(sapply(res$result$last.population, function (x) x$selector.selection[1]))

# --- TEST MBO?! 

# pure MBO without feature selection
tosubmit = tab[algorithm %in% "no_feature_sel", ]
res = testJob(tosubmit[1, ])
res = testJob(tosubmit[4, ])

res = testJob(tosubmit[2, ])


# --- TEST MOSMAFS
tosubmit = tab[algorithm %in% "mosmafs", ]

# uniform vs. non-uniform 
binom = testJob(61)
sapply(binom$result$last.population, function (x) mean(x$selector.selection))

unif = testJob(63)
sapply(unif$result$last.population, function (x) mean(x$selector.selection))


# --- test single objective
tosubmit = tab[multi.objective == FALSE & parent.sel == "selTournament", ]
res = testJob(tosubmit[1, ])


# --- test mbo multicrit
tosubmit = tab[algorithm == "mbo_multicrit", ]
res = testJob(tosubmit[1, ])





bla = lapply(object$result.pf, function(x) {
    res = doNondominatedSorting(t(as.matrix(x)))
    which(res$ranks == 1)
}
)


  
  perflist <- mapply(FUN = function(train, hold) {
    res = doNondominatedSorting(t(as.matrix(train)))
    idx = which(res$ranks == 1)

    res = cbind(train[idx, ], hold[idx, 2])
    res$diff = res[, 3] - res[, 2]
    res

    
  }, object$result.pf, object$result.pf.test, SIMPLIFY = FALSE)

sapply(perflist, function(x) mean(x$diff))


# how does something like that look like for NSGA-II? 



## --- CHECKOUT HOW LONG A SINGLE XGBOOST RUN TAKES ON THE DIFFERENT TASKS 

reg = makeRegistry(file.dir = "test")

reg$default.resources = list(
  walltime = 3600L * 96L,
  memory = memory = 1024L * 4L,,
  clusters = "serial")

args = CJ(
  nrounds = c(10, 100, 1000, 2000),
  max_depth = c(3, 10, 20), 
  nfeats = c(100, 1000, 10000)
  )

batchMap(function(i) {
  
  dataset = "AP_Breast_Colon"
  learner = "xgboost"
  cv.iters = 10L 

  lrn = makeLearner("classif.xgboost", id = "classif.xgboost", eval_metric = "error", objective = "binary:logistic")

  readDataAndRinst = function(data, rinst.iter, ...) {
    task = readRDS(file.path(data, "task.rds"))
    rin = readRDS(file.path(data, "rin.rds"))
    # hyperparams = readRDS(file.path(data, "hyperparams_500.rds"))[[rinst.iter]]

    train.task = subsetTask(task, rin$train.inds[[rinst.iter]])
    test.task = subsetTask(task, rin$test.inds[[rinst.iter]])

    list(train.task = train.task, test.task = test.task)#, hyperparams = hyperparams)
  }

  instance = readDataAndRinst(paste("data/", dataset, sep = ""), 1)

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

  configureMlr(show.info = TRUE, show.learner.output = TRUE)

  lrn2 = setHyperPars2(lrn, par.vals = x)

  time = system.time({
    model = train(lrn2, train.task)
    prd = predict(model, test.task)
  })  
  
  time
}, 1:length(args))






