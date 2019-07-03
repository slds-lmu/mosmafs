no_feature_sel = function(data, job, instance, learner, maxeval, maxtime, cv.iters) {

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
        lrn2 = setHyperPars2(lrn, par.vals = x)

        model = train(lrn, train.task)
        prd = predict(model, test.task)
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

  # --- fitness function --- 
  result = mbo(tuneobj, control = ctrl)

  if (PARALLELIZE) {
    parallelStop()
  }

  runtime = proc.time() - time

  return(list(result = result, test.task = test.task, train.task = train.task, runtime = runtime, ps = ps))
} 

