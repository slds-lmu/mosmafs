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


MBObaseline = function(data, job, instance, learner, maxeval, filter, MBMOmethod, infill, surrogate, propose.points) {

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

  ctrl = makeMBOControl(n.objectives = 2, propose.points = propose.points) 
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


mosmafs = function(data, job, instance, learner, maxeval, filter, initialization,
  lambda, mu, parent.sel, feature.mut) {

  id = strsplit(data, "/")[[1]][3]

  # --- task and learner ---
  train.data = read.arff(file.path(data, "train.arff"))
  task.train = makeClassifTask(id = id, data = train.data, target = "class")
  test.data = read.arff(file.path(data, "test.arff"))  
  task.test = makeClassifTask(id = id, data = test.data, target = "class")
 
  # --- inner resampling ---  
  lrn = LEARNERS[[learner]]
  
  # --- inner resampling ---
  stratcv10 = makeResampleDesc("CV", iters = 10, stratify = TRUE)

  # --- parameter set ---
  ps = PAR.SETS[[learner]]
  ps = c(ps, pSS(selector.selection: logical^getTaskNFeats(task.train)))

  # --- strategy parameters ---
  ps = c(ps,  pSS(.strategy.numeric: numeric[0, 1],
                  .strategy.logical: numeric[0, 1],
                  .strategy.integer: numeric[0, 1],
                  .strategy.discrete: numeric[0, 1],
                  .strategy.selector.selection: numeric[0, 1]))
  
  filter.strat = NULL
  fima = NULL

  if (filter != "none") {
      fima = makeFilterMat(task.train, filters = FILTER[[filter]])
      ps = c(ps, pSS(filterweights: numeric[0, ~1]^length(FILTER[[filter]])))
      filter.strat = makeFilterStrategy(fima, "filterweights")
  }

  # --- initialization ---
  initials = sampleValues(ps, mu, discrete.names = TRUE)

  if (initialization == "unif") 
    distribution = function() floor(runif(1, 0, length(initials[[1]]$selector.selection) + 1))
  else 
    distribution = function() rbinom(1, length(initials[[1]]$selector.selection), 0.5)

  initials = initSelector(initials, distribution = distribution, soften.op.strategy = filterstrat) 
  fitness.fun = makeObjective(learner = lrn, task = task.train, ps = ps, resampling = stratcv10, holdout.data = task.test)
  
  # --- EA operators ---
  mutator = combine.operators(ps,
    numeric = mutGaussScaled,
    logical = mutBitflip,
    integer = mutUniformInt,
    discrete = mutRandomChoice,
    selector.selection = FEATURE_MUT[[feature.mut]],
    .strategy.numeric = makeMutationStrategyNumeric(".strategy.numeric", "sdev", lr = 1 / sqrt(2 * lambda), lower = 0, upper = 1),
    .strategy.logical = makeMutationStrategyNumeric(".strategy.logical", "p", lr = 1 / sqrt(2 * lambda), lower = 0, upper = 1),
    .strategy.discrete = makeMutationStrategyNumeric(".strategy.integer", "p", lr = 1 / sqrt(2 * lambda), lower = 0, upper = 1),  
    .strategy.selector.selection = makeMutationStrategyNumeric(".strategy.integer", "p", lr = 1 / sqrt(2 * lambda), lower = 0, upper = 1)
  )

  crossover = combine.operators(ps,
    numeric = recSBX,
    integer = recPCrossover,
    discrete = recPCrossover,
    logical = recUnifCrossover,
    selector.selection = recPCrossover 
  )

  time = proc.time()

  # --- fitness function --- 
  result = slickEcr(
    fitness.fun = fitness.fun,
    lambda = lambda,
    population = initials,
    mutator = mutator,
    recombinator = crossover,
    generations = floor((maxeval - mu) / lambda)
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

addAlgorithm(name = "randomsearch", reg = reg, fun = randomsearch)
addAlgorithm(name = "MBObaseline", reg = reg, fun = MBObaseline)
addAlgorithm(name = "mosmafs", reg = reg, fun = mosmafs)

addExperiments(reg = reg, 
  algo.designs = list(randomsearch = ades.random, 
                      MBObaseline = ades.mbo,
                      mosmafs = ades.mosmafs),
  repls = REPLICATIONS)

