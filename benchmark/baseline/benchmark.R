library("batchtools")
library("ecr")
library("magrittr")
library("ParamHelpers")
library("parallelMap")
library("mlr")
library("mlrCPO")
library("mlrMBO")
library("RWeka")
library("mosmafs")
library("mlrMBO")

source("def.R")

if (file.exists("registry")) {
  if (OVERWRITE) {
    unlink("registry", recursive = TRUE)
    reg = makeExperimentRegistry(file.dir = "registry", seed = 123L,
      packages = packages, source = "def.R")
  } else {
    reg = loadRegistry("registry", writeable = TRUE)
  }
} else {
  reg = makeExperimentRegistry(file.dir = "registry", seed = 123L,
    packages = packages, source = "def.R")
}

# return the filepath for each 
for (ds in datasets) {  
  addProblem(name = ds, data = paste(datafolder, ds, sep = "/"), reg = reg)
}


randomsearch = function(data, job, instance, learner, maxeval, filter, initialization) {

  id = strsplit(data, "/")[[1]][3]

  # --- task and learner ---
  train.data = read.arff(file.path(data, "train.arff"))
  task.train = makeClassifTask(id = id, data = train.data, target = "class")
  test.data = read.arff(file.path(data, "test.arff"))  
  task.test = makeClassifTask(id = id, data = test.data, target = "class")
  
  lrn = LEARNERS[[learner]]

  # --- paramset ---
  ps = PAR.SETS[[learner]]
  ps = c(ps, pSS(selector.selection: logical^getTaskNFeats(task.train)))

  # --- inner resampling ---
  stratcv10 = makeResampleDesc("CV", iters = 10, stratify = TRUE)
  
  filter.strat = NULL
  fima = NULL

  if (filter != "none") {
      fima = makeFilterMat(task.train, filters = FILTER[[filter]])
      ps = c(ps, pSS(filterweights: numeric[0, ~1]^length(FILTER[[filter]])))
      filter.strat = makeFilterStrategy(fima, "filterweights")
  }

  # --- initialization ---
  initials = sampleValues(ps, maxeval, discrete.names = TRUE)

  print("sample values")

  if (initialization == "unif") 
    distribution = function() floor(runif(1, 0, length(initials[[1]]$selector.selection) + 1))
  else 
    distribution = function() rbinom(1, length(initials[[1]]$selector.selection), 0.5)

  initials = initSelector(initials, distribution = distribution, soften.op.strategy = filterstrat) 
  fitness.fun = makeObjective(learner = lrn, task = task.train, ps = ps, resampling = stratcv10, holdout.data = task.test)

  mutator = combine.operators(ps,
    numeric = mutGaussScaled,
    logical = mutBitflip,
    integer = mutUniformInt,
    discrete = mutRandomChoice,
    selector.selection = mutBitflip
  )

  crossover = combine.operators(ps,
    numeric = recSBX,
    integer = recPCrossover,
    discrete = recPCrossover,
    logical = recUnifCrossover
  )

  time = proc.time()

  # --- fitness function --- 
  result = slickEcr(
    fitness.fun = fitness.fun,
    lambda = 1L,
    population = initials,
    mutator = mutator,
    recombinator = crossover,
    generations = 0L
  )

  runtime = proc.time() - time

  # do nondom sorting for every step
  pops = getPopulations(result$log)
  ranks = lapply(pops, function(x) doNondominatedSorting(x$fitness)) 
  paretofront = lapply(ranks, function(x) which(x$ranks == 1))
  domhypervol = lapply(seq_along(pops), function(x) computeHV(as.matrix(pops[[x]]$fitness[, paretofront[[x]]]), ref.point = c(1, 1)))

  return(list(result = result, task.test = task.test, task.train = task.train, runtime = runtime, ps = ps, paretofront = paretofront,
              domhypervol = domhypervol, filtermat = fima))
} 


MBObaseline = function(data, job, instance, learner, maxeval, filter, MBMOmethod, infill, surrogate) {

  id = strsplit(data, "/")[[1]][3]

  # --- task and learner ---
  train.data = read.arff(file.path(data, "train.arff"))
  task.train = makeClassifTask(id = id, data = train.data, target = "class")
  test.data = read.arff(file.path(data, "test.arff"))  
  task.test = makeClassifTask(id = id, data = test.data, target = "class")
  
  lrn = LEARNERS[[learner]]

  # --- paramset ---
  ps = PAR.SETS[[learner]]

  # --- inner resampling ---
  stratcv10 = makeResampleDesc("CV", iters = 10, stratify = TRUE)

  # --- create baseline objective
  # --- no explicit number of features --> fix
  obj = makeBaselineObjective(lrn, task.train,
    filters = FILTER[[filter]],
    ps = ps, resampling = stratcv10,
    holdout.data = task.test)
  attr(obj, "noisy") = FALSE

  ctrl = makeMBOControl(n.objectives = 2) 
  ctrl = setMBOControlMultiObj(ctrl, method = MBMOmethod)
  ctrl = setMBOControlInfill(ctrl, INFILL[[infill]])
  ctrl = setMBOControlTermination(ctrl, max.evals = maxeval)
 
  time = proc.time()

  result = mbo(obj, control = ctrl, learner = SURROGATE[[surrogate]])

  runtime = proc.time() - time

  # do nondom sorting for every step
  opt.path = data.frame(result$opt.path)
  ranks = lapply(1:nrow(opt.path), function(i) doNondominatedSorting(t(as.matrix(opt.path[1:i, c("y_1", "y_2")]))))
  paretofront = lapply(ranks, function(x) which(x$ranks == 1))
  domhypervol = lapply(1:nrow(opt.path), function(i) computeHV(t(as.matrix(opt.path[paretofront[[i]], c("y_1", "y_2")])), ref.point = c(1, 1)))

  return(list(result = result, task.test = task.test, task.train = task.train, runtime = runtime, ps = ps, paretofront = paretofront, 
    domhypervol = domhypervol))
}

addAlgorithm(name = "randomsearch", reg = reg, fun = randomsearch)
addAlgorithm(name = "MBObaseline", reg = reg, fun = MBObaseline)

addExperiments(reg = reg, 
  algo.designs = list(randomsearch = ades.random, 
                      MBObaseline = ades.mbo),
  repls = REPLICATIONS)

