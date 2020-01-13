no_feature_sel = function(data, job, instance, learner, maxeval, maxtime, cv.iters, filter, 
  surrogate, infill, filter.during.run, propose.points, start.recon.iter, step.size) {

    # MBO baseline: single-objective tuning with "reconstruction" of a Pareto front afterwards

    # data, job, instance:      batchtools specific
    # learner:                  learner to be tuned
    # maxeval:                  maximum number of evals, one eval = one complete CV
    # maxtime:                  maximum time the algorithm should run 
    # cv.iters:                 iterations of inner cross-validation
    # filter:                   set of filters to be tuned over, specified in def.R; 
    # surrogate:                surrogate model for model-based optimization
    # infill:                   infill crit for model-based optimization
    # filter.during.run:        if TRUE, we tune over filter and perc in a single-objective way 
    # propose.points:           number of points proposed in one batch

    # ---
    # 1. Define task, learner, paramset, and inner resampling
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
    colnames(filtermat) = sapply(colnames(filtermat), function(x) {
      z = strsplit(x, "value.")[[1]]
      z[length(z)]
    })


    p = getTaskNFeats(train.task)

 
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


  # ---
  # 5. Run experiment 
  # ---

  time = proc.time()
  result = mbo(tuneobj, learner = SURROGATE[[surrogate]], control = ctrl)  
  runtime = proc.time() - time



  # ---
  # 6. Receive a Pareto front by subsetting features using filter
  # ---

  # Basic Idea: take 1:p features according to a filter, and compute performance
  # get the Pareto front and the dominated hypervolume from this 

  recon = reconstructParetoFront(tuneobj = tuneobj, start.iter = start.recon.iter, step.size = step.size, mbo.result = result, train.task = train.task, test.task = test.task, maxeval = maxeval, filters = filters, ps = ps)

  return(list(result = result, result.pf = recon$result.pf.list, result.pf.test = recon$result.pf.test.list, test.task = test.task, train.task = train.task, runtime = runtime, pareto.time = recon$pareto.time, ps = ps))
} 



