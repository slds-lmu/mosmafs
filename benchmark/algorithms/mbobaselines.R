no_feature_sel = function(data, job, instance, learner, maxeval, maxtime, cv.iters, filter, 
  surrogate, infill, filter.during.run, propose.points, start.recon.iter, step.size, ensemble) {

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

        if (ensemble) {
          ps$pars$filter = NULL
          params = makeParamSet(params = lapply(seq_along(filters), function(idx) {
            # not using vector parameters here because mlrMBO probably
            # sucks at handling them.
            makeNumericParam(sprintf("mosmafs.select.weights.%s", idx),
              lower = 0, upper = 1 - .Machine$double.eps)
          }))
          ps = c(ps, params)
        }
    } 


    
    filtermat = makeFilterMat(task = train.task, filters = filters)
    colnames(filtermat) = sapply(colnames(filtermat), function(x) {
      z = strsplit(x, "value.")[[1]]
      z[length(z)]
    })

    if (ensemble) {
      params = lapply(seq_along(filters), function(idx) {
        # not using vector parameters here because mlrMBO probably
        # sucks at handling them.
        makeNumericParam(sprintf("mosmafs.select.weights.%s", idx),
          lower = 0, upper = 1 - .Machine$double.eps)
      })
      ps = c(ps, params)
    }


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

    if (!ensemble) {
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
    } 

    if (ensemble) {
        tuneobj = smoof::makeSingleObjectiveFunction(
          name = "tuning",
          has.simple.signature = FALSE, par.set = ps, noisy = TRUE,
          fn = function(x) {
            # mlrMBO is the platonic ideal of awful design.
            # The function parameter actually must be named 'x'.
            args <- x
            dif.names <- getParamIds(ps)[!getParamIds(ps) %in% names(args)]
            if (length(dif.names) > 0) {
              stop(sprintf("%s must be an element in list 'x'", dif.names))
            }

            # trafo not necessary in mlrMBO

            # set up `selector.selection` from nselect, iselect, select.weights and fmat
            nselect <- ceiling(p * args$perc) 

            if (length(filters) > 1) {
              select.weights <- unlist(args[sprintf("mosmafs.select.weights.%s",
                seq_along(filters))])
              select.weights <- pmin(select.weights, 1 - .Machine$double.eps)
              select.weights <- -log1p(-select.weights)
              select.weights <- select.weights / max(sum(select.weights), .001)
              fvals <- c(filtermat %*% select.weights)
            } else {
              fvals <- c(filtermat)
            }

            selections <- order(fvals, decreasing = TRUE)
            args$selector.selection <- rep(FALSE, p)
            args$selector.selection[selections[seq_len(nselect)]] <- TRUE            # filter out mosmafs.* parameters we don't need any more
            
            args_lrn <- args[intersect(names(args), getParamIds(getLearnerParamSet(lrn)))]
            lrn2 <- setHyperPars(lrn, par.vals = args_lrn)

            propfeat <- mean(args$selector.selection)

            filtered.train.task = subsetTask(train.task, features = args$selector.selection)
            filtered.test.task = subsetTask(test.task, features = args$selector.selection) 

            net.time <- system.time(
              val <- resample(lrn2, filtered.train.task, inner, show.info = FALSE)$aggr,
              gcFirst = FALSE)[3]
            if (is.na(val)) {
              val <- worst.measure
            }
            userextra <- list(net.time = net.time)

            model <- train(lrn2, filtered.train.task)
            prd <- predict(model, filtered.test.task)
            val.holdout <- performance(prd, mmce)[1]
            if (is.na(val.holdout)) {
              val.holdout <- worst.measure
            }
            userextra <- c(userextra, list(
              fitness.holdout.perf = val.holdout,
              fitness.holdout.propfeat = propfeat))
      
            result <- c(perf = val)
            attr(result, "extras") <- userextra
            result
          })
    }


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

  recon = reconstructParetoFront(tuneobj = tuneobj, start.iter = start.recon.iter, step.size = 240L, mbo.result = result, train.task = train.task, test.task = test.task, maxeval = maxeval, filters = filters, ps = ps)

  return(list(result = result, result.pf = recon$result.pf.list, result.pf.test = recon$result.pf.test.list, test.task = test.task, train.task = train.task, runtime = runtime, pareto.time = recon$pareto.time, ps = ps))
} 



