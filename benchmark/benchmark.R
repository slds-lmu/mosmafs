library("batchtools")
library("ecr")
library("magrittr")
library("ParamHelpers")
library("parallelMap")
library("mlr")
library("mlrCPO")
library("mosmafs")

source("def.R")

if (file.exists("registry2")) {
  if (OVERWRITE) {
    unlink("registry2", recursive = TRUE)
    reg = makeExperimentRegistry(file.dir = "registry2", seed = 123L,
      packages = packages, source = "def.R")
  } else {
    reg = loadRegistry("registry2", writeable = TRUE)
  }
} else {
  reg = makeExperimentRegistry(file.dir = "registry2", seed = 123L,
    packages = packages, source = "def.R")
}

# return the filepath for each 
for (ds in datasets) {  
  addProblem(name = ds, data = paste(datafolder, ds, "task.rds", sep = "/"), reg = reg)
}

mosmafs = function(data, job, instance, learner, 
  lambda, mu, maxeval, filter.method, initialization, 
  parent.sel, feature.mut) {

  # --- task and learner ---
  task = readRDS(instance)
  lrn = LEARNERS[[learner]]
  n = getTaskSize(task)

  # --- split of a test set for outer resampling ---
  split = round(0.9 * n)
  id.train = sample(n, size = split)
  id.test = setdiff(seq(1, n), id.train)
  task.train = subsetTask(task, subset = id.train)
  task.test = subsetTask(task, subset = id.test)  


  if (type == "mlrMBO") {
    # do mlr MBO baseline
  }


  # --- parameter set ---
  ps = PAR.SETS[[learner]]
  ps = c(ps, 
    pSS(selector.selection: logical^getTaskNFeats(task),
        .strategy.numeric: numeric[0, 1],
        .strategy.logical: numeric[0, 1],
        .strategy.integer: numeric[0, 1],
        .strategy.discrete: numeric[0, 1],
        .strategy.selector.selection: numeric[0, 1]))

  # --- EA operators ---
  mutator = combine.operators(ps,
    numeric = mutGaussScaled,
    logical = mutBitflip,
    integer = mutUniformInt,
    discrete = mutRandomChoice,
    selector.selection = FEATURE_MUT[[feature.mut]],
    .strategy.numeric = makeMutationStrategyNumeric(".strategy.numeric", "sdev", lr = 1 / sqrt(2 * lambda), lower = 0, upper = 1),
    .strategy.logical = makeMutationStrategyNumeric(".strategy.logical", "p", lr = 1 / sqrt(2 * lambda), lower = 0, upper = 1),
    # .strategy.integer = makeMutationStrategyNumeric(".strategy.logical", "p", lr = 1 / sqrt(2 * lambda), lower = 0, upper = 1),    
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

  # --- initialization of population
  initials = sampleValues(ps, mu, discrete.names = TRUE)
  probs = NULL
  FILTERMAT = NULL

  time = proc.time()

  if (filter.method != "none") {
    n.measures = length(FILTER_METHOD[[filter.method]])
    FILTERMAT = makeFilterMat(task.train, FILTER_METHOD[[filter.method]])
    probs = FILTERMAT %*% rep(1 / n.measures, n.measures)       
  } 

  if (initialization != "none"){
    sample.pars = INITIALIZATION[[initialization]]
    args = sample.pars[-1]
    if (is.null(args))
      args = list()
    if (initialization == "unif")
      args$max = ps$pars$selector.selection$len
    sampler = sample.pars[[1]]
    initials = resamplePopulationFeatures(inds = initials, ps = ps, sampler = sampler, args = args, probs = probs) 
  }

  # parallelMap::parallelStartMulticore(cpus = lambda, level =  "ecr2.evaluateFitness")
  
  results = slickEcr(
    fitness.fun = makeObjective(learner = lrn, task = task.train, ps = ps, resampling = cv10, holdout.data = task.test),
    lambda = lambda,
    population = initials,
    mutator = mutator,
    recombinator = crossover,
    generations = floor((maxeval - mu) / lambda), 
    parent.selector = PARENTSEL[[parent.sel]]
  )

  # parallelMap::parallelStop()

  runtime = proc.time() - time

  # do nondom sorting for every step
  pops = getPopulations(results$log)
  ranks = lapply(pops, function(x) doNondominatedSorting(x$fitness)) 
  paretofront = lapply(ranks, function(x) which(x$ranks == 1))
  domhypervol = lapply(seq_along(pops), function(x) computeHV(as.matrix(pops[[x]]$fitness[, paretofront[[x]]]), ref.point = c(1, 1)))

  return(list(results = results, task.test = task.test, task.train = task.train, runtime = runtime, ps = ps, paretofront = paretofront, 
    domhypervol = domhypervol, filtermat = FILTERMAT))
}

addAlgorithm(name = "mosmafs", reg = reg, fun = mosmafs)

addExperiments(reg = reg, 
  algo.designs = list(mosmafs = ades),
  repls = REPLICATIONS)

