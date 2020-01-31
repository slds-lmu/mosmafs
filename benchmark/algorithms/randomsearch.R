randomsearch = function(data, job, instance, learner, maxeval, filter, initialization, cv.iters) {

    # ---
    # 1. Define task, learner, paramset, and inner resampling
    # ---

    lrn = LEARNERS[[learner]] # learner 

    train.task = instance$train.task # training
    test.task = instance$test.task # for outer evaluation

    inner = makeResampleDesc("CV", iters = cv.iters, stratify = TRUE)

    ps = PAR.SETS[[learner]] # paramset
 
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
    p = NULL

    # --- initialization according to a distribution
    if (initialization == "unif") 
      distribution = function() floor(runif(1, 0, length(initials[[1]]$selector.selection) + 1))
    if (initialization == "none") 
      distribution = function() rbinom(1, length(initials[[1]]$selector.selection), 0.5)
    
    if (initialization == "geom") {
      # we empirically find a good value for p by fitting 100 trees 
      rpart = makeLearner("classif.rpart")

      rsmp = makeResampleDesc("RepCV")
      rinst = makeResampleInstance(rsmp, train.task)
      nacvars = rep(0, length(rinst$train.inds))

      for (i in 1:length(rinst$train.inds)) {
          tsk = subsetTask(train.task, subset = rinst$train.inds[[i]])
          mod = train(rpart, tsk)
      
          splits = mod$learner.model$splits
      
          acvars = unique(rownames(splits))
          nacvars[i] = length(acvars)
      }
      p = 1 / mean(nacvars) # heuristic for p
      distribution = function() {
        z = getTaskNFeats(train.task) + 10
        while(z > getTaskNFeats(train.task)) {
          z = rgeom(1, p)
        }
        z
      }   
    }



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


  runtime = proc.time() - time

  return(list(result = result, test.task = test.task, train.task = train.task, runtime = runtime, ps = ps, filtermat = fima, p_geom = p))
} 
