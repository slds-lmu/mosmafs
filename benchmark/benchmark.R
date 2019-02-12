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

fun = function(job, data, p.inf, p.noise, n, ...) {
    create.linear.toy.data(n) %>% create.classif.task(id = "lin.toy.task")
}
addProblem("lin.toy.data", fun = fun, reg = reg)

fun = function(job, data, id) convertOMLTaskToMlr(getOMLTask(task.id = id))$mlr.task
addProblem("ionosphere", fun = fun, reg = reg)



mosmafs = function(data, job, instance, learner, lambda, mu, maxeval, filter.method, resampling) {

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
  fitness.fun = function(args) {
    args = args[intersect(names(args), getParamIds(getParamSet(lrn)))]
    val = resample(setHyperPars(lrn, par.vals = args), task.train, resinner, show.info = FALSE)$aggr
    propfeat = mean(args$selector)
    c(perf = val, feat = propfeat)
  }

  initials = sampleValues(ps, mu, discrete.names = TRUE)

  if (filter.method != "none") {
    filtervals = generateFilterValuesData(task.train, method = FILTER_METHOD[[filter.method]])
    filtervals = filtervals$data[-(1:2)]

    FILTERMAT = apply(filtervals, 2, function(col) {
      col = col - mean(col)
      col = (col - min(col)) / (max(col) - min(col))
    })
        
    initials = lapply(initials, function(x) {
      x$selector.selection = sapply(FILTERMAT[, 1], function(x) sample(c(FALSE, TRUE), size = 1, p = c(1 - x, x)))
      x
    })
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

  results = my.nsga2(
    fitness.fun = fitness.fun, n.objectives = 2L, minimize = TRUE,
    mu = mu, lambda = round(lambda * mu),
    mutator = mutator, recombinator = crossover,
    representation = "custom",
    initial.solutions = initials,
    log.pop = TRUE, 
    terminators = list(stopOnEvals(maxeval)))

  return(list(results = results, task.test = task.test))
}

addAlgorithm(name = "mosmafs", reg = reg, fun = mosmafs)

addExperiments(reg = reg, 
  prob.designs = pdes, 
  algo.designs = list(mosmafs = ades),
  repls = REPLICATIONS)

