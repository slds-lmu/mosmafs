mbo_multicrit = function(data, job, instance, learner, maxeval, maxtime, cv.iters, filter, 
  surrogate, infill, propose.points, adaptive.filter.weights) {

    # Multiobjective model-based joint optimization 

    # data, job, instance:      batchtools specific
    # learner:                  learner to be tuned
    # maxeval:                  maximum number of evals, one eval = one complete CV
    # maxtime:                  maximum time the algorithm should run 
    # cv.iters:                 iterations of inner cross-validation
    # filter:                   set of filters to be tuned over, specified in def.R; 
    # surrogate:                surrogate model for model-based optimization
    # infill:                   infill crit for model-based optimization
    # propose.points:           number of points proposed in one batch
    # adaptive.filter.wegihts:  tuning over a weighted sum of filter values


    # ---
    # 0. Define task, learner, paramset, and inner resampling
    # ---

    lrn = LEARNERS[[learner]] # learner 

    train.task = instance$train.task # training
    test.task = instance$test.task # for outer evaluation
    p = getTaskNFeats(train.task)

    inner = makeResampleDesc("CV", iters = cv.iters, stratify = TRUE)

    ps = PAR.SETS[[learner]] # paramset
    
    filters = FILTER[[filter]]

    if (!adaptive.filter.weights) {
        ps = c(ps, pSS(filter: discrete[filters], perc: numeric[0, 1]))
    }
     
    # ---
    # 1. Initial design 
    # --- 
  
    # no specification --> default is maximin Latin Hypercube with 4 * nparams

    # ---
    # 2. Control 
    # --- 
    
    ctrl = makeMBOControl(n.objectives = 2L, propose.points = propose.points)
    ctrl = setMBOControlMultiObj(ctrl, method = "parego")
    ctrl = setMBOControlTermination(ctrl, max.evals = maxeval, exec.time.budget = maxtime)
    ctrl = setMBOControlInfill(ctrl, crit = INFILL[[infill]])

    # ---
    # 3. Objective 
    # --- 
    if (adaptive.filter.weights) {
      tuneobj = makeBaselineObjective(learner = lrn, task = train.task, filters = filters, ps = ps, resampling = inner, holdout.data = test.task) 
    } else {
      filtermat = makeFilterMat(task = train.task, filters = filters)
      colnames(filtermat) = sapply(colnames(filtermat), function(x) {
        z = strsplit(x, "value.")[[1]]
        z[length(z)]
      })
      
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
          val = performance(prd, mmce)[1]
          
          res = resample(lrn2, filtered.train.task, inner, show.info = FALSE)$aggr
          res = c(res, propfeat = perc)
          attr(res, "extras") = list(fitness.holdout.perf = val, fitness.holdout.propfeat = perc)
          res
        },
        par.set = ps,
        has.simple.signature = FALSE,
        minimize = c(TRUE, TRUE), 
        n.objectives = 2L, ref.point = c(1, 1)
      )
    }


  # ---
  # 4. Run experiment 
  # ---

  time = proc.time()
  result = mbo(tuneobj, learner = SURROGATE[[surrogate]], control = ctrl)  
  runtime = proc.time() - time

    
  return(list(result = result, test.task = test.task, train.task = train.task, runtime = runtime, ps = ps))
} 

