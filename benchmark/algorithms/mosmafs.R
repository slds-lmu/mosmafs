mosmafs = function(data, job, instance, learner, maxeval, filter, initialization,
  lambda, mu, parent.sel, chw.bitflip, adaptive.filter.weights, filter.during.run) {

  PARALLELIZE = TRUE

  id = strsplit(data, "/")[[1]][2]

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

  if (PARALLELIZE) {
    parallelStartMulticore(cpus = 15L)
  }

  result = slickEcr(
    fitness.fun = fitness.fun,
    lambda = lambda,
    population = initials,
    mutator = mutator,
    recombinator = crossover,
    generations = ceiling((maxeval - mu) / lambda)
  )

  if (PARALLELIZE) {
    parallelStop()
  }

  runtime = proc.time() - time

  return(list(result = result, task.test = task.test, task.train = task.train, runtime = runtime, ps = ps, filtermat = fima))
} 

