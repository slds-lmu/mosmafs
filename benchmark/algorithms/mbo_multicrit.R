mbo_multicrit = function(data, job, instance, learner, maxeval, maxtime, cv.iters, filter, 
  surrogate, infill, propose.points) {

    PARALLELIZE = FALSE

    # ---
    # 0. Define task, learner, paramset, and inner resampling
    # ---

    lrn = LEARNERS[[learner]] # learner 

    train.task = instance$train.task # training
    test.task = instance$test.task # for outer evaluation

    inner = makeResampleDesc("CV", iters = cv.iters, stratify = TRUE)

    ps = PAR.SETS[[learner]] # paramset
    
    filters = FILTER[[filter]]

    ps = c(ps, pSS(filter: discrete[filters], perc: numeric[0, 1]))
    
    filtermat = makeFilterMat(task = train.task, filters = filters)
    p = getTaskNFeats(train.task)

    # ---
    # 1. eventually setup parallel environemnt
    # --- 
    
    if (PARALLELIZE)
      parallelMap::parallelStartMulticore(cpus = 10L)
 
    # ---
    # 2. Initial design 
    # --- 
  
    # no specification --> default is maximin Latin Hypercube with 4 * nparams

    # ---
    # 3. Control 
    # --- 
    
    ctrl = makeMBOControl(n.objectives = 2L, propose.points = propose.points)
    ctrl = setMBOControlMultiObj(ctrl, method = "parego")
    ctrl = setMBOControlTermination(ctrl, max.evals = maxeval, exec.time.budget = maxtime)
    ctrl = setMBOControlInfill(ctrl, crit = INFILL[[infill]])

    # ---
    # 4. Objective 
    # --- 



    tuneobj = makeMultiObjectiveFunction(name = "tuning",
     fn = function(x) {
      
        perc = x$perc
        x$perc = NULL

        filter = x$filter
        x$filter = NULL
          
        ind.features = order(filtermat[,filter, drop = FALSE], decreasing = TRUE)[1:ceiling(perc*p)]
        filtered.train.task = subsetTask(train.task, features = ind.features)
        filtered.test.task = subsetTask(test.task, features = ind.features) 

        lrn2 = setHyperPars2(lrn, par.vals = x)
        
        model = train(lrn2, filtered.train.task)
        prd = predict(model, filtered.test.task)
        val = performance(prd, mmce, test.task, model)[1]
        
        res = resample(lrn2, train.task, inner, show.info = FALSE)$aggr
        res = c(res, propfeat = perc)
        attr(res, "extras") = list(fitness.holdout.perf = val, fitness.holdout.propfeat = perc)
        res
      },
      par.set = ps,
      has.simple.signature = FALSE,
      minimize = c(TRUE, TRUE), 
      n.objectives = 2L, ref.point = c(1, 1)
    )

  time = proc.time()

  if (PARALLELIZE) {
    parallelStartMulticore(cpus = 80L)
  }

  # ---
  # 5. Run experiment 
  # ---
  result = mbo(tuneobj, learner = SURROGATE[[surrogate]], control = ctrl)
  
  
  runtime = proc.time() - time


  if (PARALLELIZE) {
    parallelStop()
  }
    
  return(list(result = result, test.task = test.task, train.task = train.task, runtime = runtime, ps = ps))
} 

