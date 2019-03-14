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

if (file.exists("registry_temp")) {
  if (OVERWRITE) {
    unlink("registry_temp", recursive = TRUE)
    reg = makeExperimentRegistry(file.dir = "registry_temp", seed = 123L,
      packages = packages, source = "def.R")
  } else {
    reg = loadRegistry("registry_temp", writeable = TRUE)
  }
} else {
  reg = makeExperimentRegistry(file.dir = "registry_temp", seed = 123L,
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

  if (initialization == "unif") 
    distribution = function() floor(runif(1, 0, length(initials[[1]]$selector.selection) + 1))
  else 
    distribution = function() rbinom(1, length(initials[[1]]$selector.selection), 0.5)

  if (filter == "none") 
    initials = initSelector(initials, distribution = distribution)
  else  
    initials = initSelector(initials, distribution = distribution, soften.op = ecr::setup(mutUniformMetaResetSHW, p = 1), soften.op.strategy = filter.strat) 
  
  fitness.fun = makeObjective(learner = lrn, task = task.train, ps = ps, resampling = stratcv10, holdout.data = task.test)

  time = proc.time()

  # --- fitness function --- 
  result = initEcr(
    fitness.fun = fitness.fun,
    population = initials
  )

  runtime = proc.time() - time

  return(list(result = result, task.test = task.test, task.train = task.train, runtime = runtime, ps = ps, filtermat = fima))
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

  return(list(result = result, task.test = task.test, task.train = task.train, runtime = runtime, ps = ps))
}


mosmafs = function(data, job, instance, learner, maxeval, filter, initialization,
  lambda, mu, parent.sel, chw.bitflip, adaptive.filter.weights, filter.during.run) {

  id = strsplit(data, "/")[[1]][3]

  # --- task and learner ---
  train.data = readRDS(file.path(data, "train.arff.rds"))
  task.train = makeClassifTask(id = id, data = train.data, target = "class")
  test.data = readRDS(file.path(data, "test.arff.rds"))  
  task.test = makeClassifTask(id = id, data = test.data, target = "class")
 
  time = proc.time()

  # --- learner ---  
  lrn = LEARNERS[[learner]]
  
  # --- inner resampling ---
  stratcv10 = makeResampleDesc("CV", iters = 10, stratify = TRUE)

  # --- parameter set w/ feature vector---
  ps = PAR.SETS[[learner]]

  num.discrete <- sum(BBmisc::viapply(ps$pars[sapply(ps$pars, isDiscrete)], getParamLengths))
  num.numeric <- sum(BBmisc::viapply(ps$pars[sapply(ps$pars, isNumeric)], getParamLengths))

  ps = c(ps, pSS(selector.selection: logical^getTaskNFeats(task.train)))

  # --- strategy parameters for mutation ---
  ps = c(ps,  pSS(stratparm.numeric: numeric[, ], # no bounds here
                  stratparm.discrete: numeric[0, 1]))

  # --- initialization of of filter matrix ---
  fima = NULL

  getFilterStrat <- function(usefilter) {
    if (!usefilter) {
      filterstrat = function(ind) list()
    } else {
       if (adaptive.filter.weights) {
        filterstrat = makeFilterStrategy(reset.dists = fima, weight.param.name = "filterweights")
      } else {
        filterstrat = function(ind) {
          list(reset.dists = fima, reset.dist.weights = rep(0.5, ncol(fima)))
        }
      }
    } 
    filterstrat
  }

  if (filter != "none") {
    fima = makeFilterMat(task.train, filters = FILTER[[filter]])
    ps = c(ps, pSS(filterweights: numeric[0, ~1]^length(FILTER[[filter]])))
  }
  if (filter != "none" && filter.during.run) {
    # create paramset for filterweights
    if (chw.bitflip) {
      sbitflip = mutUniformMetaResetSHW
    } else {
      sbitflip = mutUniformMetaReset
    }
  } else {
    if (chw.bitflip) {
      sbitflip = mutBitflipCHW
    } else {
      sbitflip = mutBitflip
    }
  }



  Ttrafo <- function(x, a, b) {
    if (a >= b) {
      return(b)
    }
    y <- (x - a) / (b - a)
    if (floor(y) %% 2 == 0) {
      y = abs(y - floor(y))
    } else {
      y = 1 - abs(y - floor(y))
    }
    a + (b - a) * y
  }

  learningrate.discrete <- 1 / sqrt(2 * num.discrete)
  learningrate.numeric <- 1 / sqrt(2 * num.numeric)
  mutP <- makeMutator(function(ind, ..., lower, upper) {
    p <- ind
    p <- 1 / (1 + (1 - p) / p * exp(- learningrate.discrete * rnorm(length(p))))
    Ttrafo(p, 1 / num.discrete, 0.5)
  }, supported = "float")

  # --- mutation and recombination operators ---
  mutator = combine.operators(ps,
      numeric = mutGaussScaled,
      logical = mutBitflip,
      integer = mutGaussIntScaled,
      discrete = mutRandomChoice,
      selector.selection = sbitflip,
      # setting strategy parameters
      .strategy.numeric = function(ind) { list(sdev = exp(ind$stratparm.numeric)) },
      .strategy.logical = function(ind) { list(p = ind$stratparm.discrete) },
      .strategy.discrete = function(ind) { list(p = ind$stratparm.discrete) },
      .strategy.integer = function(ind) { list(sdev = exp(ind$stratparm.numeric)) },
      # mutation operators for strategy parameters
      stratparm.numeric = ecr::setup(mutGauss, p = 1, sdev = learningrate.numeric),
      stratparm.discrete = mutP,
      # if we adapt filterweights  
      .strategy.selector.selection = getFilterStrat(filter != "none" && filter.during.run)
    )
 
  crossover = combine.operators(ps,
    numeric = recSBX,
    integer = recIntSBX,
    discrete = recPCrossover,
    logical = recPCrossover,
    selector.selection = recPCrossover 
  )

  # --- initialization ---
  ps.init = ps
  ps.init$pars$stratparm.numeric$lower = log(0.1)
  ps.init$pars$stratparm.numeric$upper = log(0.1)
  ps.init$pars$stratparm.discrete$lower = 0.1
  ps.init$pars$stratparm.discrete$upper = 0.1

  initials = sampleValues(ps.init, mu, discrete.names = TRUE)

  if (initialization == "unif") 
    distribution = function() floor(runif(1, 0, length(initials[[1]]$selector.selection) + 1))
  else 
    distribution = function() rbinom(1, length(initials[[1]]$selector.selection), 0.5)

  if (filter == "none") 
    initials = initSelector(initials, distribution = distribution)
  else  
    initials = initSelector(initials, distribution = distribution, soften.op = ecr::setup(mutUniformMetaResetSHW, p = 1), soften.op.strategy = getFilterStrat(TRUE)) 

  # --- fitness function --- 
  fitness.fun = makeObjective(learner = lrn, task = task.train, ps = ps, resampling = stratcv10, holdout.data = task.test)

  result = slickEcr(
    fitness.fun = fitness.fun,
    lambda = lambda,
    population = initials,
    mutator = mutator,
    recombinator = crossover,
    generations = ceiling((maxeval - mu) / lambda)
  )

  runtime = proc.time() - time

  return(list(result = result, task.test = task.test, task.train = task.train, runtime = runtime, ps = ps, filtermat = fima))
} 

addAlgorithm(name = "randomsearch", reg = reg, fun = randomsearch)
addAlgorithm(name = "MBObaseline", reg = reg, fun = MBObaseline)
addAlgorithm(name = "mosmafs", reg = reg, fun = mosmafs)

addExperiments(reg = reg, 
  algo.designs = list(randomsearch = ades.random, 
                      MBObaseline = ades.mbo,
                      mosmafs = ades.mosmafs),
  repls = REPLICATIONS)

addExperiments(reg = reg, 
  algo.designs = list(mosmafs = ades.mosmafs),
  repls = 1L)

tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "initialization", "maxeval", 
  "filter", "learner", "chw.bitflip", "adaptive.filter.weights", "filter.during.run" ))
tab = tab[algorithm == "mosmafs" & problem == "sonar" & learner == "SVM", ]

for (dirs in dir()) for (item in c("train.arff", "test.arff")) {
  dat <- read.arff(file.path(dirs, item))
  saveRDS(dat, file.path(dirs, paste0(item, ".rds")))
}
