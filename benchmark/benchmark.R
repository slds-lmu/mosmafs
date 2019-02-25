library(batchtools)
library(magrittr)
library(OpenML)
library(ecr)
library(mlrCPO)

source("def.R")

if (file.exists("registry")) {
  if (OVERWRITE) {
    unlink("registry", recursive = TRUE)
    reg = makeExperimentRegistry(seed = 123L,
      packages = packages, source = "def.R")
  } else {
    reg = loadRegistry("registry", writeable = TRUE)
  }
} else {
  reg = makeExperimentRegistry(seed = 123L,
    packages = packages, source = "def.R")
}

# return the filepath for each 
for (ds in datasets) {  
  addProblem(name = ds, data = paste(datafolder, ds, "task.rds", sep = "/"), reg = reg)
}


mosmafs = function(data, job, instance, learner, lambda, mu, maxeval, filter.method, resampling, initialization, parent.sel) {

  # --- task and learner ---
  task = readRDS(instance)
  lrn = LEARNERS[[learner]]
  n = getTaskSize(task)

  # --- nested resampling for proper evaluation ---
  split = round(0.9 * n)
  id.train = sample(n, size = split)
  id.test = setdiff(seq(1, n), id.train)
  task.train = subsetTask(task, subset = id.train)
  task.test = subsetTask(task, subset = id.test)  
  resinner = makeResampleInstance(RESAMPLING[[resampling]], task = task.train) 

  # --- parameter set ---
  ps = PAR.SETS[[learner]]
  ps = c(ps, 
        pSS(selector.selection: logical^getTaskNFeats(task),
            .strategy.numeric: numeric[10^(-3), 10],
            .strategy.logical: numeric[0, 1],
            .strategy.numeric: numeric[0, 1],
            .strategy.selector.selection: numeric[0, 1])
        )
  
  # --- create fitness function ---
  fitness.fun = function(args, task = task.train, resampling = resinner) {
    args = args[intersect(names(args), getParamIds(getParamSet(lrn)))]
    val = resample(setHyperPars(lrn, par.vals = args), task, resampling, show.info = FALSE)$aggr
    propfeat = mean(args$selector)
    c(perf = val, feat = propfeat)
  }

  initials = sampleValues(ps, mu, discrete.names = TRUE)
  initials = setStrategyParametersFixed(initials, ".strategy.numeric", 0.1 * (getUpper(ps)[".strategy.numeric"] - getLower(ps)[".strategy.numeric"]))
  initials = setStrategyParametersFixed(initials, ".strategy.logical", 0.1)
  initials = setStrategyParametersFixed(initials, ".strategy.discrete", 0.1)
  initials = setStrategyParametersFixed(initials, ".strategy.selector.selection", 1 /  getParamLengths(ps)["selector.selection"]) # recommendation of jakob bossek

  probs = NULL
  FILTERMAT = NULL

  if (filter.method != "none") {
    filtervals = generateFilterValuesData(task.train, method = FILTER_METHOD[[filter.method]])
    filtervals = filtervals$data[-(1:2)]

    FILTERMAT = apply(filtervals, 2, function(col) {
      col = col - mean(col)
      col = (col - min(col)) / (max(col) - min(col))
    })

    probs = FILTERMAT %*% c(0.9, 0.1)       
  } 

  if (initialization != "none"){
    sample.pars = INITIALIZATION[[initialization]]
    args = sample.pars[-1]
    if (is.null(args))
      args = list()
    sampler = sample.pars[[1]]
    initials = resamplePopulationFeatures(inds = initials, ps = ps, sampler = sampler, args = args, probs = probs) 
  }

  mutator = combine.operators(ps,
    numeric = mutGauss,
    logical = mutBitflip,
    integer = mutUniformInt,
    discrete = mutRandomChoice,
    selector.selection = mutBitflip,
    .strategy.numeric = makeMutationStrategyNumeric(".strategy.numeric", "sdev", lr = 1 / sqrt(2 * lambda), lower = getLower(ps$pars$.strategy.numeric), upper = getUpper(ps$pars$.strategy.numeric)),
    .strategy.logical = makeMutationStrategyNumeric(".strategy.logical", "p", lr = 1 / sqrt(2 * lambda), lower = 0, upper = 1),
    .strategy.discrete = makeMutationStrategyNumeric(".strategy.integer", "p", lr = 1 / sqrt(2 * lambda), lower = 0, upper = 1),  
    .strategy.selector.selection = makeMutationStrategyNumeric(".strategy.integer", "p", lr = 1 / sqrt(2 * lambda), lower = 0, upper = 1)
  )

  crossover = combine.operators(ps,
    numeric = recSBX,
    integer = recIntSBX,
    discrete = recPCrossover,
    logical = recUnifCrossover)

  time = proc.time()

  results = my.nsga2(
    fitness.fun = fitness.fun, n.objectives = 2L, minimize = TRUE,
    mu = mu, lambda = lambda,
    mutator = mutator, recombinator = crossover,
    representation = "custom",
    initial.solutions = initials,
    log.pop = TRUE, 
    terminators = list(stopOnEvals(maxeval)), parent.selector = PARENTSEL[[parent.sel]])

  runtime = proc.time() - time

  # do nondom sorting for every step
  pops = getPopulations(results$log)
  ranks = lapply(pops, function(x) doNondominatedSorting(x$fitness)) 
  paretofront = lapply(ranks, function(x) which(x$ranks == 1))
  domhypervol = lapply(seq_along(pops), function(x) computeHV(as.matrix(pops[[x]]$fitness[, paretofront[[x]]]), ref.point = c(1, 1)))

  # evaluate the final candidates on the testset
  eval.outer = function(args) {
    args = args[intersect(names(args), getParamIds(getParamSet(lrn)))]
    mod = train(setHyperPars(lrn, par.vals = args), task.train)
    pred = predict(mod, task.test)
    perf = performance(pred)
    propfeat = mean(args$selector)
    c(perf = perf, feat = propfeat)
  }  

  pareto.front.test = lapply(results$pareto.set, eval.outer)

  return(list(results = results, task.test = task.test, task.train = task.train, runtime = runtime, ps = ps, paretofront = paretofront, 
    pareto.front.test = pareto.front.test, domhypervol = domhypervol, filtermat = FILTERMAT))
}

addAlgorithm(name = "mosmafs", reg = reg, fun = mosmafs)

addExperiments(reg = reg, 
  algo.designs = list(mosmafs = ades),
  repls = REPLICATIONS)

