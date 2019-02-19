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

fun = function(job, data, p.inf, p.noise, n, ...) {
    task = create.hypersphere.data(p.inf, n) %>% create.classif.task(id = "hypersphere") %>% task.add.random.cols(num = p.noise)   
}
addProblem("hypersphere", fun = fun, reg = reg)

fun = function(job, data, id) convertOMLTaskToMlr(getOMLTask(task.id = id))$mlr.task
addProblem("ionosphere", fun = fun, reg = reg)

fun = function(job, data, id) convertOMLTaskToMlr(getOMLTask(task.id = id))$mlr.task
addProblem("australian", fun = fun, reg = reg)

fun = function(job, data, id) convertOMLTaskToMlr(getOMLTask(task.id = id))$mlr.task
addProblem("heart", fun = fun, reg = reg)

fun = function(job, data, id) convertOMLTaskToMlr(getOMLTask(task.id = id))$mlr.task
addProblem("pima", fun = fun, reg = reg)

fun = function(job, data, id) convertOMLTaskToMlr(getOMLTask(task.id = id))$mlr.task
addProblem("glass", fun = fun, reg = reg)



mosmafs = function(data, job, instance, learner, lambda, mu, maxeval, filter.method, resampling, initialization) {

  # --- task and learner ---
  task = instance
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
  ps = c(ps, pSS(selector.selection: logical^getTaskNFeats(task)))
  
  # --- create fitness function ---
  fitness.fun = function(args, task = task.train, resampling = resinner) {
    args = args[intersect(names(args), getParamIds(getParamSet(lrn)))]
    val = resample(setHyperPars(lrn, par.vals = args), task, resampling, show.info = FALSE)$aggr
    propfeat = mean(args$selector)
    c(perf = val, feat = propfeat)
  }

  initials = sampleValues(ps, mu, discrete.names = TRUE)
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
  selector.selection = mutBitflip)

  crossover = combine.operators(ps,
    numeric = recSBX,
    integer = recIntSBX,
    discrete = recPCrossover,
    logical = recUnifCrossover)

  time = proc.time()

  results = my.nsga2(
    fitness.fun = fitness.fun, n.objectives = 2L, minimize = TRUE,
    mu = mu, lambda = round(lambda * mu),
    mutator = mutator, recombinator = crossover,
    representation = "custom",
    initial.solutions = initials,
    log.pop = TRUE, 
    terminators = list(stopOnEvals(maxeval)))

  runtime = proc.time() - time

  # do nondom sorting for every step
  pops = getPopulations(results$log)
  ranks = lapply(pops, function(x) doNondominatedSorting(x$fitness)) 
  paretofront = lapply(ranks, function(x) which(x$ranks == 1))
  domhypervol = lapply(seq_along(pops), function(x) computeHV(pops[[x]]$fitness[, paretofront[[x]]], ref.point = c(1, 1)))

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
  prob.designs = pdes, 
  algo.designs = list(mosmafs = ades),
  repls = REPLICATIONS)

