randomsearch = function(data, job, instance, learner, maxeval, filter, initialization, cv.iters) {

    PARALLELIZE = FALSE

    # ---
    # 0. Define task, learner, paramset, and inner resampling
    # ---

    lrn = LEARNERS[[learner]] # learner 

    train.task = instance$train.task # training
    test.task = instance$test.task # for outer evaluation

    inner = makeResampleDesc("CV", iters = cv.iters, stratify = TRUE)

    ps = PAR.SETS[[learner]] # paramset

    # ---
    # 1. eventually setup parallel environemnt
    # --- 
    
    if (PARALLELIZE)
      parallelMap::parallelStartMulticore(cpus = 15L)
 
    # ---
    # 2. population initialization
    # --- 

    # --- filter initialization
    filter.strat = NULL
    fima = NULL

    if (filter != "none") {
        fima = makeFilterMat(train.task, filters = FILTER[[filter]])
        ps = c(ps, pSS(filterweights: numeric[0, ~1]^length(FILTER[[filter]])))
        filter.strat = makeFilterStrategy(fima, "filterweights")
    }

    fitness.fun = makeObjective(learner = lrn, task = train.task, ps = ps, resampling = inner, holdout.data = test.task)
    
    ps = getParamSet(fitness.fun)
    
    initials = sampleValues(ps, maxeval, discrete.names = TRUE)

    # --- initialization according to a distribution
    if (initialization == "unif") 
      distribution = function() floor(runif(1, 0, length(initials[[1]]$selector.selection) + 1))
    else 
      distribution = function() rbinom(1, length(initials[[1]]$selector.selection), 0.5)

    if (filter == "none") 
      initials = initSelector(initials, distribution = distribution)
    else  
      initials = initSelector(initials, distribution = distribution, soften.op = ecr::setup(mutUniformMetaResetSHW, p = 1), soften.op.strategy = filter.strat) 
  
  time = proc.time()

  # --- fitness function --- 
  result = initEcr(
    fitness.fun = fitness.fun,
    population = initials
  )

  if (PARALLELIZE) {
    parallelStop()
  }

  runtime = proc.time() - time

  return(list(result = result, test.task = test.task, train.task = train.task, runtime = runtime, ps = ps, filtermat = fima))
} 
