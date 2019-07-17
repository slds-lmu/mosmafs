no_feature_sel = function(data, job, instance, learner, maxeval, maxtime, cv.iters, filter, 
  surrogate, infill, filter.during.run, propose.points) {

    PARALLELIZE = FALSE
    LENGTH.OUT = 10

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
      parallelMap::parallelStartMulticore(cpus = 10L)
 
    # ---
    # 2. Initial design 
    # --- 
  
    # no specification --> default is maximin Latin Hypercube with 4 * nparams

    # ---
    # 3. Control 
    # --- 
    
    ctrl = makeMBOControl(propose.points = 10L)
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
        val = performance(prd, mmce, test.task, model)[1]
        
        res = resample(lrn2, train.task, inner, show.info = FALSE)$aggr
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
  seq.perc = seq(1, p) / p

  path = trafoOptPath(result$opt.path)$env$path
  seq.path = round(seq(from = 1, to = nrow(path), length.out = LENGTH.OUT))
  
  # result dataframe : one for inner evaluation, one for outer evaluation on test set
  result.pf= data.table(matrix(NA, nrow = length(seq.perc), ncol = length(seq.path)))
  rownames(result.pf) = seq.perc
  colnames(result.pf) = as.character(seq.path)
  result.pf.test = result.pf
  
  
  for (row.nr in seq.path) {
    best = as.list(path[row.nr,][, !names(path) %in% c("y", "perc")])
  
  #best = as.list(tail(path, 1)[, !names(path) %in% "y"])

  # If filter was already tuned, take tuned filter 
    if (!is.null(best$filter)) {
      filters = best$filter
    }
  
    # temporary result dataframe: one for inner evaluation, one for outer evaluation on test set
    result.pf.temp = as.data.frame(matrix(NA, nrow = length(seq.perc), ncol = length(filters)))
    rownames(result.pf.temp) = seq.perc
    colnames(result.pf.temp) = filters
    result.pf.test.temp = result.pf.temp
    
    for (fil in filters) {
      for (s.perc in seq.perc) {
        best$filter = fil
        best$perc = s.perc
        perf = tuneobj(best)
        result.pf.temp[as.character(s.perc), fil] = perf
        result.pf.test.temp[as.character(s.perc), fil] = attr(perf, "extras")$fitness.holdout.perf
      }
    }
  # if (ncol(result.pf) > 1) {
  #   col.id = which.max(rowSums(apply(result.pf, 1, rank)))
  # } else {
  #   col.id = 1
  # }

  # result.pf = setDT(result.pf[, col.id, drop = FALSE], keep.rownames = TRUE)[]
  # result.pf.test = setDT(result.pf.test[, col.id, drop = FALSE], keep.rownames = TRUE)[]
    
    # result.pf.temp = setDT(result.pf.temp, keep.rownames = TRUE)[]
    # result.pf.test.temp = setDT(result.pf.test.temp, keep.rownames = TRUE)[]
    
    result.pf[, as.character(row.nr)] = result.pf.temp[, 1]
    result.pf.test[, as.character(row.nr)] = result.pf.test.temp[,1]
  }
  pareto.time = proc.time() - time
    
  return(list(result = result, result.pf = result.pf, result.pf.test = result.pf.test, test.task = test.task, train.task = train.task, runtime = runtime, pareto.time = pareto.time, ps = ps))

} 

