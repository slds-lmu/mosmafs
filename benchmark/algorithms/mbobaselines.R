no_feature_sel = function(data, job, instance, learner, maxeval, maxtime, cv.iters, filter, 
  surrogate, infill, filter.during.run, propose.points) {

    PARALLELIZE = TRUE
    STEPS = 120L
    START = 80L

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
    
    filtermat = makeFilterMat(task = train.task, filters = filters)
    p = getTaskNFeats(train.task)

    # ---
    # 1. eventually setup parallel environemnt
    # --- 
    
    if (PARALLELIZE)
      parallelMap::parallelStartMulticore(cpus = 15L)
 
    # ---
    # 2. Initial design 
    # --- 
    # no specification --> default is maximin Latin Hypercube with 4 * nparams

    # ---
    # 3. Control 
    # --- 
    
    ctrl = makeMBOControl(propose.points = propose.points)
    ctrl = setMBOControlTermination(ctrl, max.evals = maxeval, exec.time.budget = maxtime)
    ctrl = setMBOControlInfill(ctrl, crit = INFILL[[infill]])

    # ---
    # 4. Objective 
    # --- 

    tuneobj = makeSingleObjectiveFunction(name = "tuning",
     fn = function(x) {
        
       if (!is.null(x$perc)) {
         perc = x$perc
         x$perc = NULL
       } else {
          perc = 1
       }
        
        if (!is.null(x$filter)) {
          filter = x$filter
          x$filter = NULL
          ind.features = order(filtermat[,filter, drop = FALSE], decreasing = TRUE)[1:ceiling(perc*p)]
          filtered.train.task = subsetTask(train.task, features = ind.features)
          filtered.test.task = subsetTask(test.task, features = ind.features) 
        } else {
          filtered.train.task = train.task
          filtered.test.task = test.task
        }
        
        lrn2 = setHyperPars2(lrn, par.vals = x)
        
        model = train(lrn2, filtered.train.task)
        prd = predict(model, filtered.test.task)
        val = performance(prd, mmce)[1]
        
        res = resample(lrn2, filtered.train.task, inner, show.info = FALSE)$aggr
        attr(res, "extras") = list(fitness.holdout.perf = val, fitness.holdout.propfeat = perc)
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
  result = mbo(tuneobj, learner = SURROGATE[[surrogate]], control = ctrl)
  
  
  runtime = proc.time() - time


  if (PARALLELIZE) {
    parallelStop()
  }
  
  # ---
  # 6. Receive a Pareto front by subsetting features using filter
  # ---

  time = proc.time()

  # if the number of features is too high, we just construct the paretofront with less features 
  pind = pmin(p, 20L)
  seq.perc = seq(1, p, length.out = pind) / p

  path = trafoOptPath(result$opt.path)$env$path
  n.steps = floor((maxeval - START) / STEPS)
  seq.path = c(START, START + 1:n.steps * STEPS)
  seq.path = sort(c(4 * sum(getParamLengths(ps)), seq.path)) # add first evaluation

  # If filter was already tuned, take tuned filter
  filters = filters[- which(filters == "DUMMY")]
  final = tail(path, 1)
  if (!is.null(final$filter)) {
    filters = final$filter
  }
  
  # Create lists of results: one for inner evaluation, one for outer evaluation on test set
  result.pf.list = list()
  result.pf.test.list = list()
  
  for (row.nr in seq.path) {
    best_id = which.min(path[1:row.nr,]$y)
    best = as.list(path[best_id,][, !names(path) %in% c("y")])
  
    # if (!is.null(path)) {
    #   filters = best$filter
    # }
  
    # temporary result dataframe: one for inner evaluation, one for outer evaluation on test set
    result.pf = as.data.frame(matrix(NA, nrow = length(seq.perc), ncol = length(filters)))
    rownames(result.pf) = seq.perc
    colnames(result.pf) = filters
    result.pf.test = result.pf
    
    for (fil in filters) {
      for (s.perc in seq.perc) {
        best$filter = fil
        best$perc = s.perc
        perf = tuneobj(best)
        result.pf[as.character(s.perc), fil] = perf
        result.pf.test[as.character(s.perc), fil] = attr(perf, "extras")$fitness.holdout.perf
      }
    }
  # if (ncol(result.pf) > 1) {
  #   col.id = which.max(rowSums(apply(result.pf, 1, rank)))
  # } else {
  #   col.id = 1
  # }
  # result.pf = setDT(result.pf[, col.id, drop = FALSE], keep.rownames = TRUE)[]
  # result.pf.test = setDT(result.pf.test[, col.id, drop = FALSE], keep.rownames = TRUE)[]
    
    result.pf = setDT(result.pf, keep.rownames = TRUE)[]
    result.pf.test = setDT(result.pf.test, keep.rownames = TRUE)[]
    result.pf.list[[as.character(row.nr)]] = result.pf
    result.pf.test.list[[as.character(row.nr)]] = result.pf.test

    print(paste("Reconstructing Pareto Front Iteration: ", row.nr, " / ", max(seq.path)))

  }
  pareto.time = proc.time() - time
    
  return(list(result = result, result.pf = result.pf.list, result.pf.test = result.pf.test.list, test.task = test.task, train.task = train.task, runtime = runtime, pareto.time = pareto.time, ps = ps))
} 

