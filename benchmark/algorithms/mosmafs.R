mosmafs = function(data, job, instance, learner, maxeval, cv.iters, filter, initialization,
  lambda, mu, parent.sel, chw.bitflip, adaptive.filter.weights, filter.during.run, multi.objective, tune.hyperparams, tune.iters) {

  # mosmafs:  our method, multi-objective simulatenous hyperparameter optimization and feature selection 
  #           based on GAs

  # data, job, instance:      batchtools specific
  # learner:                  learner to be tuned
  # maxeval:                  maximum number of evals, one eval = one complete CV
  # cv.iters:                 iterations of inner cross-validation
  # filter:                   set of filters used for initialization and mutation 
  # initialization:           distribution w.r.t. which the feature bit vector is initialization 
  # lambda:                   lambda in (mu + lambda)-strategy
  # mu:                       mu in (mu + lambda)-strategy
  # parent.sel:               type of parent selection
  # chw.bitflip:              hamming-weight preserving bitflip mutation
  # adaptive.filter.weights:  tune over weights of filter combinations 
  # filter.during.run:        use filter during run
  # multi.objective:          if TRUE, tune over (perf, nfeat)
  # tune.hyperparams:         if TRUE, hyperparameters and feature are tuned jointly
  # tune.iters:               allow a warm start for tuning. Attention: filter configuration has to be stored before 

  # ---
  # 1. Define task, learner, paramset, and inner resampling
  # ---

  lrn = LEARNERS[[learner]] # learner 
  
  train.task = instance$train.task # training
  test.task = instance$test.task # for outer evaluation

  inner = makeResampleDesc("CV", iters = cv.iters, stratify = TRUE)

  ps = PAR.SETS[[learner]] # paramset
  num.discrete = sum(BBmisc::viapply(ps$pars[sapply(ps$pars, isDiscrete)], getParamLengths))
  num.numeric = sum(BBmisc::viapply(ps$pars[sapply(ps$pars, isNumeric)], getParamLengths))

  if (!tune.hyperparams) {
    params = instance$hyperparams[[learner]]
    params$kernel = as.character(params$kernel)
    params = params[getParamIds(ps)]
    params = trafoValue(ps, params)
    ps = pSS()
    lrn = setHyperPars2(lrn, params)
  }   

  # ---
  # 2. Mosmafs initialization 
  # --- 
  

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
    fima = makeFilterMat(train.task, filters = FILTER[[filter]])
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

  
  # --- strategy parameters for mutation ---
  ps = c(ps,  pSS(stratparm.numeric: numeric[, ], # no bounds here
                  stratparm.discrete: numeric[0, 1]))
  
  # --- create objective ---
  if (multi.objective) {
      fitness.fun = makeObjective(learner = lrn, task = train.task, ps = ps, resampling = inner, holdout.data = test.task)
  } else {
      fitness.fun = makeSingleObjectiveFeatsel(learner = lrn, task = train.task, ps = ps, resampling = inner, holdout.data = test.task)
  }
  
  ps = getParamSet(fitness.fun)
  
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
  
  # --- parent selector ---
  par.sel = PARENTSEL[[parent.sel]] 

  # --- survival selector ---
  sur.sel = setup(selGreedy)
  if (multi.objective)
    sur.sel = setup(selNondom)

  # ---
  # 3. population initialization
  # --- 

  initials = sampleValues(ps.init, mu, discrete.names = TRUE)

  if (initialization == "unif") 
    distribution = function() floor(runif(1, 0, length(initials[[1]]$selector.selection) + 1))
  else 
    distribution = function() rbinom(1, length(initials[[1]]$selector.selection), 0.5)

  if (filter == "none") 
    initials = initSelector(initials, distribution = distribution)
  else  
    initials = initSelector(initials, distribution = distribution, soften.op = ecr::setup(mutUniformMetaResetSHW, p = 1), soften.op.strategy = getFilterStrat(TRUE)) 


  # "soft start" - we tune hyperparameters, but they are initialized already
  if (tune.hyperparams & tune.iters > 0) {
    params = instance$hyperparams[[learner]]
    params$kernel = as.character(params$kernel)
    params = params[which(getParamIds(ps) %in% names(params))]
    initials = lapply(initials, function(x) {
      x[names(x) %in% names(params)] = params
      x
    })
  }

  
  # ---
  # 4. Run 
  # --- 
  
  time = proc.time()

  result = slickEcr(
    fitness.fun = fitness.fun,
    lambda = lambda,
    population = initials,
    parent.selector = par.sel,
    survival.selector = sur.sel, 
    mutator = mutator,
    recombinator = crossover,
    generations = ceiling((maxeval - mu) / lambda)
  )

  runtime = proc.time() - time

  return(list(result = result, test.task = test.task, train.task = train.task, runtime = runtime, ps = ps, filtermat = fima))
} 

