no_feature_sel = function(data, job, instance, learner, maxeval, maxtime, cv.iters, filter, 
  filter.during.run) {

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
    
    if (filter.during.run) {
      ps = c(ps, pSS(
        filter: discrete[filters], 
        perc: numeric[0, 1]))
    }

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
    ctrl = makeMBOControl()
    ctrl = setMBOControlTermination(ctrl, max.evals = maxeval, exec.time.budget = maxtime)
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())
    
    # ---
    # 4. Objective 
    # --- 

    tuneobj = makeSingleObjectiveFunction(name = "svm.tuning",
     fn = function(x) {
        
        if (!is.null(x$filter)) {
          filter = x$filter
          x$filter = NULL
        }
        
        if (!is.null(x$perc)) {
          perc = x$perc
          x$perc = NULL
        }
        
        lrn2 = setHyperPars2(lrn, par.vals = x)
        
        if (!is.null(filter)) {
          filtered.train.task = filterFeatures(train.task, method = filter, perc = perc)
          filtered.test.task = filterFeatures(train.task, method = filter, perc = perc) 
        } else {
          filtered.train.task = train.task
          filtered.test.task = test.task
        }
        
        model = train(lrn, filtered.train.task)
        prd = predict(model, filtered.test.task)
        val = performance(prd, mmce, test.task, model)[1]
        
        res = resample(lrn, train.task, inner, show.info = FALSE)$aggr
        attr(res, "extras") = list(fitness.holdout.perf = val, fitness.holdout.propfeat = 1)
        res
      },
      par.set = ps,
      noisy = TRUE,
      has.simple.signature = FALSE,
      minimize = TRUE
    )

  time = proc.time()

  if (PARALLELIZE) {
    parallelStartMulticore(cpus = 80L)
  }

  # ---
  # 5. Run experiment 
  # ---
  result = mbo(tuneobj, control = ctrl)

  if (PARALLELIZE) {
    parallelStop()
  }
  
  # ---
  # 6. Receive a Pareto front by subsetting features using filter
  # ---
  seq.perc = seq(0.1, 0.9, 0.15)    
  path = trafoOptPath(result$opt.path)$env$path
  best = as.list(tail(path, 1)[, !names(path) %in% "y"])
  # If filter was already tuned, take tuned filter 
  if (!is.null(best$filter)) {
    filters = best$filter
  }
  
  result.pf = as.data.frame(matrix(NA, nrow = length(seq.perc), ncol = length(filters)))
  rownames(result.pf) = seq.perc
  colnames(result.pf) = filters
  
  for (fil in filters) {
    for (s.perc in seq.perc) {
      best$filter = fil
      best$perc = s.perc
      perf = tuneobj(best)
      result.pf[as.character(s.perc), fil] = attr(perf, "extras")$fitness.holdout.perf
    }
  }
  
  if (ncol(result.pf) > 1) {
    col.id = which.max(rowSums(apply(result.pf, 1, rank)))
  } else {
    col.id = 1
  }
  
  runtime = proc.time() - time
    
  return(list(result = result, result.pf = result.pf[, col.id, drop = FALSE], test.task = test.task, train.task = train.task, runtime = runtime, ps = ps))
} 

