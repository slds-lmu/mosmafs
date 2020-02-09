# helpers

printState = function(tab, experiments, dataset = NULL, ids) {
  # prints the current state of experiments that are finished w.r.t. the specified datasets
  experiments = lapply(1:length(experiments), function(i) cbind(version = names(experiments[i]), experiments[[i]]))
  dfs = lapply(experiments, function(ex) ijoin(tab, ex, by = names(ex)[2:ncol(ex)]))
  df = do.call(rbind, dfs)
  df$isdone = df$job.id %in% ids$job.id 
  df = df[, sum(isdone), by = c("version", "problem", "learner")]
  
  if (!is.null(dataset)) {
    df = df[problem == dataset, ]
  }

  df = dcast(df, version ~ learner, fun.aggregate = sum)
  
  return(df)
}

fitnesses <- function(results) {
  pops <- getPopulations(results$log)
  do.call(rbind, lapply(seq_along(pops), function(idx) {
    pop <- pops[[idx]]
    df <- as.data.frame(t(pop$fitness))
    colnames(df) <- c("perf", "propfeat")
    df$iter <- idx
    df
  }))
}

getIndividualsChromosomes <- function(results) {
  pops = getPopulations(results$log)
  do.call(rbind, lapply(seq_along(pops), function(idx) {
    pop = pops[[idx]]
    df = lapply(pop$population, function(x) t(x$selector.selection))
    df = do.call("rbind", df)
    fitnesses = as.data.frame(t(pop$fitness))
    colnames(fitnesses) = c("perf", "propfeat")
    fitnesses$iter = idx
    df = cbind(fitnesses, df)
  }))
}


getAllIndividuals = function(ecr_res) {
	pops = lapply(getPopulations(ecr_res$log), function(x) do.call("rbind", lapply(x$population, unlist)))
	pops = lapply(1:length(pops), function(i) cbind(i, pops[[i]]))
	pops = do.call("rbind", pops)
	pops = pops[!duplicated(pops[, -1]), ]

}

makeSingleObjectiveFeatsel <- function(learner, task, ps, resampling, measure = NULL, holdout.data = NULL, worst.measure = NULL, cpo = NULLCPO) {
  if (is.null(measure)) {
    measure <- getDefaultMeasure(task)
  }
  assertClass(learner, "Learner")
  assertClass(cpo, "CPO")
  assertClass(task, "Task")
  assertClass(holdout.data, "Task", null.ok = TRUE)
  assertClass(ps, "ParamSet")
  assert(
      checkClass(resampling, "ResampleInstance"),
      checkClass(resampling, "ResampleDesc"),
      checkFunction(resampling, nargs = 1)
  )
  assertClass(measure, "Measure")
  if (is.null(worst.measure)) {
    worst.measure <- measure$worst
  }
  assertNumber(worst.measure, finite = TRUE)

  obj.factor <- if (measure$minimize) 1 else -1

  worst.measure <- worst.measure * obj.factor

  # error if selector.selection already in ps, will be automatically added
  if ("selector.selection" %in% ParamHelpers::getParamIds(ps)) {
    stop("selector.selection is not allowed to be part of 'ps' as it is automatically added")
  }
  ps = c(ps, pSS(selector.selection = NA: logical^getTaskNFeats(task)))
  
  learner <- cpoSelector() %>>% checkLearner(learner, type = getTaskType(task))
  learner %<<<% cpo

  argnames <- getParamIds(getParamSet(learner))
  smoof::makeSingleObjectiveFunction(
    sprintf("mosmafs_%s_%s", learner$id, task$task.desc$id),
    has.simple.signature = FALSE, par.set = ps, noisy = TRUE,
    fn = function(args, fidelity = NULL, holdout = FALSE) {
      if (holdout && is.null(holdout.data)) {
        return(c(perf = Inf, propfeat = Inf))
      }
      if (!missing(fidelity) && identical(fidelity, 0)) {
        return(c(perf = 0, propfeat = 0))
      }
      # filter out strategy parameters
      args <- args[intersect(names(args), argnames)]
      learner <- setHyperPars(learner, par.vals = args)
      if (holdout) {
        model <- train(learner, task)
        prd <- predict(model, holdout.data)
        val <- performance(prd, list(measure), task, model)[1]
      } else {
        if (is.function(resampling)) {
          assertNumber(fidelity)
          res <- resampling(fidelity)
        } else {
          res <- resampling
        }

        val <- resample(learner, task, res,
          list(measure), show.info = FALSE)$aggr
      }
      if (is.na(val)) {
        val <- worst.measure
      }
      propfeat <- mean(args$selector.selection)
      c(perf = unname(val * obj.factor))
  })
}



collectResult <- function(ecr.object, aggregate.perresult = list(domHV = function(x) computeHV(x, ref.point)), aggregate.perobjective = list("min", "mean", "max"), ref.point = c(1, 1), cor.fun = cor) {
  assertClass(ecr.object, "MosmafsResult")

  normalize.funlist <- function(fl) {
    assertList(fl, any.missing = FALSE, types = c("function", "character"))

    charentries <- vlapply(fl, is.character)
    names(fl)[charentries] <- ifelse(is.na(names2(fl)[charentries]),
      unlist(fl[charentries], recursive = FALSE),
      names2(fl)[charentries])
    fl[charentries] <- lapply(fl[charentries], get,
      envir = .GlobalEnv, mode = "function")
    assertList(fl, any.missing = FALSE, types = "function", names = "unique")
  }

  aggregate.perresult <- normalize.funlist(aggregate.perresult)
  aggregate.perobjective <- normalize.funlist(aggregate.perobjective)

  assertFunction(cor.fun)

  aggregate.fitness <- function(fitness) {
    resmat <- sapply(fitness, function(fit) {
      if (ecr.object$task$n.objectives == 1) {
        vnapply(aggregate.perobjective, function(f) f(fit))
      } else {
        if (is.null(rownames(fit))) {
          rownames(fit) <- paste0("obj.", seq_len(nrow(fit)))
        }
        c(
          unlist(apply(fit, 1, function(frow) {
            as.list(vnapply(aggregate.perobjective, function(f) f(frow)))
          })),
          vnapply(aggregate.perresult, function(f) f(fit))
        )
      }
    })
    as.data.frame(t(resmat))
  }

  fitnesses <- popAggregate(ecr.object$log, "fitness")
  if (ecr.object$task$n.objectives == 1) {
    propfeats <- lapply(ecr.object$log$env$pop, function(x) {
        y = unlist(lapply(x$population, function(x) sum(x$selector.selection) / length(x$selector.selection)))
      })
    propfeats = propfeats[1:length(fitnesses)]
    fitnesses = mapply(rbind, fitnesses, propfeats, SIMPLIFY = FALSE)
  }

  if (length(fitnesses) == 1L) # ugly fix for calculating traces for randomsearch
      fitnesses = lapply(c(seq(80, 2000, by = 15), 2000), function(i) fitnesses[[1]][, 1:i])

  stats <- getStatistics(ecr.object$log)
  stats.newinds <- getStatistics(ecr.object$log.newinds)

  no.fid <- is.null(stats.newinds$fidelity)
  if (no.fid) {
    stats.newinds$fidelity.sum <- 0
  } else {
    reevals <- stats.newinds$gen[stats.newinds$population == "fidelity.reeval"]
  }

  resdf <- with(stats.newinds, data.frame(
    gen,
    runtime = cumsum(runtime.sum),
    evals = cumsum(size),
    cum.fid = cumsum(fidelity.sum)))
  resdf <- resdf[!rev(duplicated(rev(resdf$gen))), ]
  assertTRUE(all.equal(resdf$gen, seq_len(nrow(resdf)) - 1))
  assertTRUE(all.equal(resdf$gen, stats$gen))
  if (no.fid) {
    resdf$cum.fid <- NULL
  } else {
    resdf$fid.reeval <- resdf$gen %in% reevals
  }
  resdf <- cbind(resdf,
    eval = aggregate.fitness(fitnesses))
  resdf$evals = 80 + resdf$gen * 15

  hofitnesses <- popAggregate(ecr.object$log, "fitness.holdout")
  if (length(hofitnesses) == 1L) # ugly fix for calculating traces for randomsearch
      hofitnesses = lapply(c(seq(80, 2000, by = 15), 2000), function(i) hofitnesses[[1]][, 1:i])

  if (ecr.object$task$n.objectives == 1) {
    hofitnesses = mapply(rbind, hofitnesses, propfeats, SIMPLIFY = FALSE)
  }

  if (any(vlapply(hofitnesses, function(x) any(is.finite(x))))) {

    corcols <- lapply(seq_len(ecr.object$task$n.objectives), function(idx) {
      mapply(function(eval.fit, hout.fit) {
        suppressWarnings(cor.fun(eval.fit[idx, ], hout.fit[idx, ]))
      }, fitnesses, hofitnesses)
    })
    names(corcols) <- rownames(fitnesses[[1]]) %??% paste0("obj.", 1:length(corcols))

        true.hout.domHV <- mapply(function(eval.fit, hout.fit) {
          unbiasedHoldoutDomHV(eval.fit, hout.fit, ref.point)
        }, fitnesses, hofitnesses)
    
        naive.hout.domHV <- mapply(function(eval.fit, hout.fit) {
          naiveHoldoutDomHV(eval.fit, hout.fit, ref.point)
        }, fitnesses, hofitnesses)
      resdf <- cbind(resdf, hout = aggregate.fitness(hofitnesses),
      true.hout.domHV, naive.hout.domHV,
      cor = corcols)
    } else {
      resdf <- cbind(resdf, hout = aggregate.fitness(hofitnesses), cor = corcols)
    }
  
  resdf$obj.1 = NULL

  resdf
}

collectResultMBO = function(object, aggregate.perresult = list(domHV = function(x) computeHV(x, ref.point)), aggregate.perobjective = list("min", "mean", "max"), ref.point = smoof::getRefPoint(ecr.object$control$task$fitness.fun), cor.fun = cor) {
 
  opt.path <- data.frame(object$result$opt.path)
  resdf <- data.table()
  task.p <- getTaskNFeats(object$train.task)

  resdf = as.data.table(opt.path)
  resdf$evals = seq_len(nrow(resdf))
  resdf$gen <- pmax(ceiling((resdf$evals - 80) / 15), 0)
  times = opt.path[, c("train.time", "propose.time", "exec.time")]
  times[is.na(times)] = 0
  times$runtime = cumsum(rowSums(times))
  resdf = cbind(resdf, times)

  if (object$result$control$n.objectives == 1) {
    resdf$perf = opt.path$y
    resdf$rn = opt.path$fitness.holdout.propfeat 
  } else {
    cols = which(names(resdf) %in% c("y_1", "y_2"))
    names(resdf)[cols] = c("perf", "rn")  
  }

  object$result.pf = lapply(unique(resdf$gen), function(x) {
    x_filt = resdf[gen <= x, ]
    # why ceiling here? 
    x_filt$rn <- ceiling(task.p * x_filt$rn) / task.p
    x_filt[, c("rn", "perf")]
  })

  object$result.pf.test = lapply(unique(resdf$gen), function(x) {
    x_filt = resdf[gen <= x, ]
    # cols = which(names(x_filt) %in% c("y_1", "y_2"))
    # names(x_filt)[cols] = c("perf", "rn")
    # why ceiling here? 
    x_filt$rn <- ceiling(task.p * x_filt$rn) / task.p
    x_filt[, c("rn", "fitness.holdout.perf")]
  })

  resdf = resdf[, .(runtime = max(runtime), evals = max(evals)), by = c("gen")]

  multi.fun <- function(x) {
    c(min = min(x), mean = mean(x, na.rm = TRUE), max = max(x))
  }
  
  receive.perf <- function(x, names) {
      agg <- as.vector(apply(x, 2, multi.fun))
      hv <- ecr::computeHV(t(as.matrix(x)), ref.point = c(1, 1))
      agg <- c(agg, hv)
      names(agg) = names
      return(agg)
  }
  
  perflist <- mapply(FUN = function(train, hold) {
    
    # Get summaries performance training data
    train$rn <- as.numeric(train$rn)
    train <- train[, c("perf", "rn")] # ATTENTION HERE !!!
    train.perf <- receive.perf(train, c("eval.perf.min", "eval.perf.mean", 
      "eval.perf.max", "eval.propfeat.min", 
      "eval.propfeat.mean", "eval.propfeat.max", "eval.domHV"))
    
    # Get summaries performance holdout data
    hold$rn <- as.numeric(hold$rn)
    hold <- hold[, c("fitness.holdout.perf", "rn")] 
    test.perf <- receive.perf(hold, c("hout.perf.min", 
      "hout.perf.mean", "hout.perf.max", "hout.propfeat.min", "hout.propfeat.mean", 
      "hout.propfeat.max",  "hout.domHV"))
    
    # Unbiased and naive domHV 
    true.hout.domHV <- unbiasedHoldoutDomHV(t(as.matrix(train)), t(as.matrix(hold)), refpoint = c(1,1))
    names(true.hout.domHV) <- "true.hout.domHV"
    naive.hout.domHV <- as.vector(naiveHoldoutDomHV(t(as.matrix(train)), t(as.matrix(hold)), refpoint = c(1,1)))
    names(naive.hout.domHV) <- "naive.hout.domHV"
    c(train.perf, test.perf, true.hout.domHV, naive.hout.domHV)
    
  }, object$result.pf, object$result.pf.test, SIMPLIFY = FALSE)
  
  perfdf = do.call(rbind, perflist)
  resdf = cbind(resdf, perfdf)
  
  # Cor not needed
  resdf$cor.perf = NA
  resdf$cor.propfeat = NA
  
  return(resdf)
}



summarizeResultMosmafs = function(x) {

    domhv = collectResult(x$result)
    
    # list of length with the number of generations 
    pops = x$result$log$env$pop
    pops[sapply(pops, is.null)] = NULL

    nfeats = getTaskNFeats(x$train.task)

    pops2 = lapply(1:length(pops), function(i) {
      gen = i - 1
      p = pops[[i]][[1]]
      res2 = lapply(p, function(q) {
        q$absfeat = sum(q$selector.selection)
        q$meanfeat = attr(q, "fitness")[2]
        q$perf = attr(q, "fitness")[1]
        q$perf.hout = attr(q, "fitness.holdout")[1]
        q$runtime = attr(q, "runtime")
        q$selector.selection = NULL
        unlist(q)
      })

      res2 = as.data.table(do.call(rbind, res2))

      if (!("meanfeat.propfeat" %in% names(res2)))
        res2$meanfeat.propfeat = as.numeric(res2$absfeat) / nfeats

      mat = apply(as.matrix(res2[, c("perf.perf", "meanfeat.propfeat")]), 2, as.numeric)
      res2$ranks = doNondominatedSorting(t(mat))$ranks

      cbind(gen = gen, res2)
    })

    list(obj.summary = domhv, population = pops2)
  }

summarizeResultMBO = function(x, nonjoint = FALSE) {
    p = getTaskNFeats(x$train.task)

    domhv = collectResultMBO(x)

    if (nonjoint) {
      newdomhv = as.data.table(getHyperVolumeOfTask(x))
      domhv$naive.hout.domHV = NULL
      domhv = ijoin(domhv, newdomhv, by = "evals")
    }

    # list of length with the number of generations 
    pops = setDT(data.frame(x$result$opt.path))
    pops$gen = pmax(floor((1:nrow(pops) - 80) / 15), 0)
    pops$abfeat = p * pops$fitness.holdout.propfeat
    pops$meanfeat.propfeat = pops$fitness.holdout.propfeat
    pops$perf.perf = pops$y
    if ("y_1" %in% names(pops))
      pops$perf.perf = pops$y_1

    pops$perf.hout.perf = pops$fitness.holdout.perf
    pops$fitness.holdout.propfeat = NULL
    pops$y = NULL
    pops$fitness.holdout.perf = NULL

    list(obj.summary = domhv, population = pops)
}

# CollectResults
collectBenchmarkResults = function(path, experiments, tab) {
  
  assert(!is.null(path))

  for (experiment in names(experiments)) {

    print(paste("Reducing ", experiment))

    for (prob in datasets) {

      savepath = file.path(path, prob, experiment, "result.rds")

      if (experiment == "BS2RF_ENS_NJ") {
          toreduce = ijoin(tab, experiments[["BS2RF_ENS"]], by = names(experiments[["BS2RF_ENS"]]))
          nonjoint = TRUE
      } else {
          toreduce = ijoin(tab, experiments[[experiment]], by = names(experiments[[experiment]]))
          nonjoint = FALSE
      }

      toreduce = ijoin(toreduce, findDone(), by = "job.id")
      toreduce = toreduce[problem == prob, ]

      print(paste("-- Reducing ", prob))

      print(nrow(toreduce))


      # if (file.exists(savepath) && nrow(readRDS(savepath)) == 30) {
      #   print(paste(experiment, "on", prob, "already completely reduced."))
      # } else {

      dir = as.numeric(sapply(list.files("registry/results/"), function(x) strsplit(x, ".rds")[[1]][1]))
      dir = data.frame(job.id = dir)
      toreduce = ijoin(toreduce, dir)

      if (experiments[[experiment]]$algorithm %in% c("no_feature_sel", "mbo_multicrit")) {
        res = reduceResultsDataTable(toreduce, function(x) summarizeResultMBO(x, nonjoint))   
      } else {
        res = reduceResultsDataTable(toreduce, function(x) summarizeResultMosmafs(x))
      }

      res = ijoin(tab, res, by = "job.id")
      res$variant = experiment

      # toreduce2 = ijoin(tab, experiments[["BS2RF_ENS"]], by = names(experiments[["BS2RF_ENS"]]))
      # toreduce2 = ijoin(toreduce2, findNotDone(), by = "job.id")
      # toreduce2 = toreduce2[problem == prob, ]
      # res_help = res[learner == "kknn", ]

      # res_help = do.call(rbind, lapply(1:4, function(x) res_help))
      # res = rbind(res, res_help)

      # res[23:30, ]$job.id = toreduce2$job.id
      # res[23:30, ]$learner = "kknn"

        # writing down metadata in the summary 

        nthr = nrow(res)

        if (!file.exists(file.path(path, "overview.rds"))) {
          overview = data.table(experiment = experiment, data.frame = prob, experiments_complete = nthr, population_reduced = TRUE, domHV_reduced = FALSE, front_reduced = FALSE)
        } else {
          overview = readRDS(file.path(path, "overview.rds"))
          
          if (nrow(ijoin(data.frame(experiment = experiment, data.frame = prob), overview)) == 0L) {
            overview = rbind(overview, data.table(experiment = experiment, data.frame = prob, experiments_complete = nthr, population_reduced = TRUE, domHV_reduced = FALSE, front_reduced = FALSE))
          } else {
            overview[experiment == experiment & data.frame == prob, ]$population_reduced = TRUE
            overview[experiment == experiment & data.frame == prob, ]$experiments_complete = nthr
          }
        }

        saveRDS(overview, file.path(path, "overview.rds"))

        if (nthr != 30)
          warning(paste("Experiments for ", prob, experiment, "not complete  (", nthr, " / 30 )"))

        dir.create(file.path(path, prob))
        dir.create(file.path(path, prob, experiment))

        if (nthr > 28) {
          saveRDS(res, file.path(path, prob, experiment, "result.rds"))
        # }
      }
    }
  }
} 



getHyperVolumeOfTask = function(x) {




  rr = sapply(1:length(x$result.pf), function(i) {
    result.pf = x$result.pf[[i]]
    result.pf.test = x$result.pf.test[[i]]

    idx = apply(result.pf[, 2:6], 1, which.min)

    perfhout = as.data.frame(result.pf.test)[-1][cbind(seq_len(nrow(result.pf)), idx)]
    ffrac = as.numeric(result.pf$rn)

    mat = t(matrix(c(perfhout, ffrac), ncol = 2))
    computeHV(mat, ref.point = c(1, 1))
    # sum(doNondominatedSorting(mat)$ranks==1)
  })

  data.frame(evals = as.numeric(names(x$result.pf)), naive.hout.domHV = rr)  
}




getPopulations = function(v, d) {
            f = file.path("raw", d, v, "result.rds")
            if (file.exists(f)) {
                resdf = readRDS(f)

                do.call(rbind, lapply(resdf$job.id, function(i) {
                    rr = resdf[job.id == i, ]$result[[1]]$population
                    if ("start.recon.iter" %in% colnames(rr))
                        rr$start.recon.iter = NULL
                    
                    if (is.data.table(rr)) {
                        rr$evals = 1:nrow(rr)
                        rr = list(rr)
                    } 

                    z = lapply(rr, function(df) {
                        if ("abfeat" %in% names(df))
                            names(df)[which(names(df) == "abfeat")] = "absfeat"
                        if (!("ranks" %in% names(df))) 
                            df$ranks = 1
                        if ("y_1" %in% names(df) & !("perf.perf" %in% names(df)))
                            df$perf.perf = df$y_1
                        if ("gen" %in% names(df)) {
                            df$evals = df$gen * 15L + 80L
                        }
                        res = df[, c("evals", "absfeat", "meanfeat.propfeat", "perf.perf", "perf.hout.perf", "ranks")]
                    })
                    z = do.call(rbind, z)
                    z = cbind(job.id = i, z)
                    z = ijoin(resdf[job.id == i, - c("result")], z, by = "job.id")
                    z

                }))
                }
              }                  
                     


getObjSummaries = function(datasets, experiments) {
    df = lapply(datasets, function(d) {
        resdflist = parallel::mclapply(experiments, function(v) {
            f = file.path(respath, "raw", d, v, "result.rds")
            if (file.exists(f)) {
                resdf = readRDS(f)
                do.call(rbind, lapply(resdf$job.id, function(i) {
                    rr = cbind(job.id = i, resdf[job.id == i, ]$result[[1]]$obj.summary)
                    rr = ijoin(resdf[job.id == i, - c("result")], rr, by = "job.id")
                    if ("start.recon.iter" %in% colnames(rr))
                        rr$start.recon.iter = NULL
                    if ("multi.objective" %in% colnames(rr))
                        rr$multi.objective = NULL
                    if ("multiobjective" %in% colnames(rr))
                        rr$multiobjective = NULL
                    if ("ensemble" %in% colnames(rr))
                        rr$ensemble = NULL

                    rr
                }))                   
            }            
        }, mc.cores = 6)
        do.call(rbind, resdflist)
    })
    do.call(rbind, df)
}


getHyperparamsPerProblem = function(x, evals = NULL) {
  
  path = as.data.frame(x$result$opt.path)
  if (!is.null(evals))
    path = path[1:evals, ]
  best_id = which.min(path$y)
  best = as.list(path[best_id,][, !names(path) %in% c("y")])

  return(best)
}


makeBaselineObjective <- function(learner, task, filters, ps, resampling, measure = NULL, num.explicit.featsel = 0, holdout.data = NULL, worst.measure = NULL, cpo = NULLCPO, numfeats = getTaskNFeats(task)) {
  if (is.null(measure)) {
    measure <- getDefaultMeasure(task)
  }
  assertClass(learner, "Learner")
  assertClass(cpo, "CPO")
  assertClass(task, "Task")
  assertClass(holdout.data, "Task", null.ok = TRUE)
  assertCharacter(filters, any.missing = FALSE, min.len = 1)
  # assertSubset(filters, names(get(".FilterRegister", envir = getNamespace("mlr"))))
  assertInt(num.explicit.featsel, lower =  0)
  assertClass(ps, "ParamSet")
  assert(
      checkClass(resampling, "ResampleInstance"),
      checkClass(resampling, "ResampleDesc")
  )
  assertClass(measure, "Measure")
  if (is.null(worst.measure)) {
    worst.measure <- measure$worst
  }
  assertNumber(worst.measure, finite = TRUE)
  assertInt(numfeats, lower = 1)

  obj.factor <- if (measure$minimize) 1 else -1

  worst.measure <- worst.measure * obj.factor

  learner <- cpoSelector() %>>% checkLearner(learner, type = getTaskType(task))
  learner %<<<% cpo

  argnames <- getParamIds(getParamSet(learner))

  assertSubset(getParamIds(ps), argnames)
  ps <- c(ps, pSS(mosmafs.nselect = NA: integer[0L, numfeats]),
    makeParamSet(params = lapply(seq_len(num.explicit.featsel), function(idx) {
      # not using vector parameters here because mlrMBO probably
      # sucks at handling them.
      makeIntegerParam(sprintf("mosmafs.iselect.%s", idx),
        lower = 1L, upper = numfeats)
    })),
    if (length(filters) > 1) {
      makeParamSet(params = lapply(seq_along(filters), function(idx) {
        # not using vector parameters here because mlrMBO probably
        # sucks at handling them.
        makeIntegerParam(sprintf("mosmafs.select.weights.%s", idx),
          lower = 1L, upper = numfeats)
      }))
    }
  )

  fmat <- makeFilterMat(task %>>% cpo, filters)
  assertMatrix(fmat, nrows = numfeats)

  smoof::makeMultiObjectiveFunction(
    sprintf("mosmafs_baseline_%s_%s", learner$id, task$task.desc$id),
    has.simple.signature = FALSE, par.set = ps, n.objectives = 2,
    ref.point = c(worst.measure, 1),
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
      nselect <- args$mosmafs.nselect
      iselect <- args[sprintf("mosmafs.iselect.%s", seq_len(num.explicit.featsel))]
      if (length(filters) > 1) {
        select.weights <- unlist(args[sprintf("mosmafs.select.weights.%s",
          seq_along(filters))])
        fvals <- c(fmat %*% select.weights)
      } else {
        fvals <- c(fmat)
      }
      selections <- order(fvals, decreasing = TRUE)
      selections <- selections[unique(c(unlist(iselect), seq_along(selections)))]
      args$selector.selection <- rep(FALSE, numfeats)
      args$selector.selection[selections[seq_len(nselect)]] <- TRUE

      # filter out mosmafs.* parameters we don't need any more
      args <- args[intersect(names(args), argnames)]
      learner <- setHyperPars(learner, par.vals = args)

      propfeat <- mean(args$selector.selection)

      net.time <- system.time(
        val <- resample(learner, task, resampling,
          list(measure), show.info = FALSE)$aggr,
        gcFirst = FALSE)[3]
      if (is.na(val)) {
        val <- worst.measure
      }
      userextra <- list(net.time = net.time)

      if (!is.null(holdout.data)) {
        model <- train(learner, task)
        prd <- predict(model, holdout.data)
        val.holdout <- performance(prd, list(measure), task, model)[1]
        if (is.na(val.holdout)) {
          val.holdout <- worst.measure
        }
        userextra <- c(userextra, list(
          fitness.holdout.perf = unname(val.holdout * obj.factor),
          fitness.holdout.propfeat = propfeat))
      }

      result <- c(perf = unname(val * obj.factor), propfeat = propfeat)
      attr(result, "extras") <- userextra
      result
    })
}


reconstructParetoFront = function(tuneobj, start.iter, step.size, mbo.result, train.task, test.task, maxeval, filters, ps) {

  time = proc.time()

  path = trafoOptPath(mbo.result$opt.path)$env$path
  p = getTaskNFeats(train.task)

  # if the number of features is too high, just take every 100th feature 
  pind = pmin(p, 10L) 
  seq.perc = seq(1, p, length.out = pind) / p

  n.steps = floor((maxeval - start.iter) / step.size)
  seq.path = c(start.iter, start.iter + 1:n.steps * step.size)
  seq.path = unique(sort(c(4 * sum(getParamLengths(ps)), seq.path))) # add first evaluation

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

  return(list(result.pf.list = result.pf.list, result.pf.test.list = result.pf.test.list, pareto.time = pareto.time))
}





