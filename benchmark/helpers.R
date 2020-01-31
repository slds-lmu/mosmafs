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














































collectResult <- function(ecr.object, aggregate.perresult = list(domHV = function(x) computeHV(x, ref.point)), aggregate.perobjective = list("min", "mean", "max"), ref.point = smoof::getRefPoint(ecr.object$control$task$fitness.fun), cor.fun = cor) {
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
  # if (ecr.object$task$n.objectives == 1) {
  #   propfeats <- lapply(getPopulations(ecr.object$log), function(x) {
  #       y = unlist(lapply(x$population, function(x) sum(x$selector.selection) / length(x$selector.selection)))
  #     })
  #   }
  #   fitnesses = mapply(rbind, fitnesses, propfeats, SIMPLIFY = FALSE)
  # }

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
      fitnesses = lapply(fitnesses, function(x) matrix(x, nrow = 1))
      hofitnesses = lapply(hofitnesses, function(x) matrix(x, nrow = 1))
  }

  if (any(vlapply(hofitnesses, function(x) any(is.finite(x))))) {

    corcols <- lapply(seq_len(ecr.object$task$n.objectives), function(idx) {
      mapply(function(eval.fit, hout.fit) {
        suppressWarnings(cor.fun(eval.fit[idx, ], hout.fit[idx, ]))
      }, fitnesses, hofitnesses)
    })
    names(corcols) <- rownames(fitnesses[[1]]) %??% paste0("obj.", 1:length(corcols))

    if (ecr.object$task$n.objectives > 1){
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
  }
  
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

# # CollectResults
# collectBenchmarkResults = function(path, experiments, tab, mbo = FALSE) {
  
#   assert(!is.null(path))

#   for (experiment in names(experiments)) {
#     toreduce = ijoin(tab, experiments[[experiment]], by = names(experiments[[experiment]]))
#     toreduce = ijoin(toreduce, findDone(), by = "job.id")

#     dir = as.numeric(sapply(list.files("registry/results/"), function(x) strsplit(x, ".rds")[[1]][1]))
#     dir = data.frame(job.id = dir)
#     toreduce = ijoin(toreduce, dir)

#     if (mbo) {
#       res = reduceResultsDataTable(toreduce, function(x) collectResultMBO(x))   
#     } else {
#       if (!is.null(experiments[[experiment]]$multi.objective) && !experiments[[experiment]]$multi.objective) {
#           res = reduceResultsDataTable(toreduce, function(x) collectResult(x$result, ref.point = c(1)))
#         } else {
#           res = reduceResultsDataTable(toreduce, function(x) collectResult(x$result))
#       }
#     }

#     res = ijoin(tab, res, by = "job.id")
#     res$variant = experiment

#     for (prob in unique(res$problem)) {
#       if (nrow(res[problem == prob, ]) < 30)
#         warning(paste("Experiments for ", prob, experiment, "not complete  (", nrow(res[problem == prob, ]), " / 30 )"))

#       dir.create(file.path(path, prob))
#       dir.create(file.path(path, prob, experiment))

#       saveRDS(res, file.path(path, prob, experiment, "domHV_over_evals.rds"))
#     }

#     dir.create(file.path(path, experiment))

#     saveRDS(res, file.path(path, experiment, "result.rds"))
#     # saveRDS(pops, file.path(path, experiment, "population.rds"))

#   }
# }

summarizeResultMosmafs = function(x) {
    domhv = collectResult(x)
    
    # list of length with the number of generations 
    pops = x$log$env$pop
    pops[sapply(pops, is.null)] = NULL

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

      mat = apply(as.matrix(res2[, c("perf.perf", "meanfeat.propfeat")]), 2, as.numeric)
      res2$ranks = doNondominatedSorting(t(mat))$ranks

      cbind(gen = gen, res2)
    })

    list(obj.summary = domhv, population = pops2)
  }

summarizeResultMBO = function(x) {
    p = getTaskNFeats(x$train.task)

    domhv = collectResultMBO(x)
    
    # list of length with the number of generations 
    pops = setDT(data.frame(x$result$opt.path))
    pops$gen = pmax(floor((1:nrow(pops) - 80) / 15), 0)
    pops$abfeat = p * pops$fitness.holdout.propfeat
    pops$meanfeat.propfeat = pops$fitness.holdout.propfeat
    pops$perf.perf = pops$y
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

    for (prob in datasets[1:10]) {

      print(paste("-- Reducing ", prob))

      toreduce = ijoin(tab, experiments[[experiment]], by = names(experiments[[experiment]]))
      toreduce = ijoin(toreduce, findDone(), by = "job.id")
      toreduce = toreduce[problem == prob, ]

      print(nrow(toreduce))

      dir = as.numeric(sapply(list.files("registry/results/"), function(x) strsplit(x, ".rds")[[1]][1]))
      dir = data.frame(job.id = dir)
      toreduce = ijoin(toreduce, dir)

      if (experiments[[experiment]]$algorithm %in% c("no_feature_sel", "mbo_multicrit")) {
        res = reduceResultsDataTable(toreduce, function(x) summarizeResultMBO(x))   
      } else {
        res = reduceResultsDataTable(toreduce, function(x) summarizeResultMosmafs(x$result))
      }

      res = ijoin(tab, res, by = "job.id")
      res$variant = experiment

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

      if (nthr == 30) {
        saveRDS(res, file.path(path, prob, experiment, "result.rds"))
      }
    }
  }
}



collectParetofront = function(path, experiments, tab, problems, learners = "xgboost") {    

    whichexp <- function(tabrow) {
      expid <- sapply(experiments, function(excond) {
        all(sapply(names(excond), function(n) excond[[n]] == tabrow[[n]]))
      })
      if (sum(expid) == 0) return(NA)
      stopifnot(sum(expid) == 1)
      names(experiments)[expid]
    }

    tab$maxeval <- NULL
    tab <- tab[is.na(mu) | (mu == 80 & lambda == 15)]
    tab$mu <- NULL
    tab$lambda <- NULL
    tab = ijoin(findDone(), tab)


    tab$expname <- sapply(mosmafs:::transpose.list(tab), whichexp)
    # tab <- tab[!is.na(expnames), ]

    tab$filter <- NULL
    tab$initialization <- NULL
    tab$chw.bitflip <- NULL
    tab$adaptive.filter.weights <- NULL
    tab$filter.during.run <- NULL
    tab$.count <- NULL

    makeparetodata <- function(...) {
      args <- list(...)
      refpt <- c(1, 1)

      stopifnot(!is.null(names(args)) && !"" %in% names(args))
      stopifnot(all(names(args) %in% colnames(tab)))

      checkline <- function(line) {
        all(sapply(names(args), function(n) line[[n]] %in% args[[n]]))
      }

      subtab <- tab[sapply(mosmafs:::transpose.list(tab), checkline)]

      fitn <- reduceResultsList(subtab, function(x) t(x$result$pareto.front))

      restbl <- rbindlist(lapply(seq_along(fitn), function(fidx) {
          fi <- fitn[[fidx]]
          if (dim(fi)[2] == 1)
            fi = cbind(fi, fi)
          dfe <- as.data.table(paretoEdges(t(fi), refpt))
          colnames(dfe)[1:2] <- c("mmce", "featfrac")
          dfe$isref <- FALSE
          dfe$instance <- fidx
          cbind(dfe, subtab[fidx, ])
      }))
      restbl$job.id <- NULL

      reftbl <- unique(restbl[, list(instance, algorithm, problem, learner, expname)])

      rbind(
          restbl,
          cbind(data.table(mmce = refpt[1], featfrac = refpt[2], point = FALSE, isref = TRUE), reftbl), fill = TRUE)
    }

    parfrnt = makeparetodata(learner = learners, expname = names(experiments), problem = problems)
    saveRDS(parfrnt, paste(path, "/pareto_examples/paretofront.rds", sep = ""))
}


extractFromSummary = function(res, toextract) {
  cols = ncol(res)
  hypervol = lapply(1:nrow(res), function(i) cbind(res[i, ]$job.id, setDT(res[i, ]$result[[1]])[, ..toextract]))
  hypervol = as.data.table(do.call("rbind", hypervol))
  names(hypervol) = c("job.id", toextract)
  df = ijoin(res[, names(res) != "result", with = FALSE], hypervol, by = "job.id")
  return(df)
}



plotMosmafsOptResult = function(res, plotspath, experiments, 
  logscale = FALSE, metric = "naive.hout.domHV", 
  plot.sd = FALSE, prompt, limits = c(0.5, 1), 
  height = 10, width = 7, plotRanks = TRUE) {
    
    # --- 0. PREPARATION 
    # prepare the dataset for plotting 
    df = res[variant %in% experiments$variant, ]

    df = extractFromSummary(df, c("evals", "runtime", metric))
    df$gen = (df$evals - 80) / 15
    df = df[gen >= 0, ]
    df = df[evals <= 4000, ] # budget is limited by 4000

    # indicate the replication 
    df = df[, replication := 1:length(job.id), by = c("learner", "variant", "problem", "gen")]
    assert(all(df$replication <= 10 & df$replication > 0)) # we made always 10 replications
    # just do some renaming for better plotting
    df = renameAndRevalue(df)
    names(df)[which(names(df) == metric)] = "metric"

    # --- 1. AGGREGATION 
    # per learner, problem, replication and evaluation: calculate Rank
    dfr = df[, `:=` (rank_variant = rank(- metric)), by = c("learner", "problem", "evals", "replication")]
    assert(all(dfr$rank_variant <= length(experiments) & dfr$rank_variant >= 1)) # we made always 10 replications

    # aggregate over all replications 
    dfr = dfr[, .(mean(metric), mean(rank_variant)), by = c("algorithm", "learner", "problem", "evals", "variant")]

    # --- 2. CREATE PLOTS
    # --- a) Overall results (all experiments compared)
    res_ovr = dfr[, .(mean.domHV = mean(V1), mean.rank = mean(V2), sd.domHV = sd(V1) / sqrt(length(V1))), by = c("evals", "variant", "algorithm")]
    if (plotRanks) {
      outpath = file.path(plotspath, prompt, gsub("\\.", "", metric), paste("ranks.pdf", sep = ""))
      plotRanks(data = res_ovr, outpath = outpath, metric = metric, limits = limits)
    }
    outpath = file.path(plotspath, prompt, gsub("\\.", "", metric), paste("performance_", limits[1], "_", limits[2], ".pdf", sep = ""))
    plotAbsPerformance(data = res_ovr, outpath = outpath, metric = metric, plot.sd = plot.sd, limits = limits)

    # --- b) Overall results (all experiments compared)
    res_ovr = dfr[, .(mean.domHV = mean(V1), sd.domHV = sd(V1) / sqrt(length(V1)), mean.rank = mean(V2)), by = c("evals", "variant", "algorithm", "learner")]
    for (lrn in unique(res_ovr$learner)) {
        if (plotRanks) {        
          outpath = file.path(plotspath, prompt, gsub("\\.", "", metric), paste("ranks_", lrn, "_", limits[1], "_", limits[2], ".pdf", sep = ""))
          plotRanks(data = res_ovr[learner == lrn, ], outpath = outpath, metric = metric, limits = limits)
        }        
        outpath = file.path(plotspath, prompt, gsub("\\.", "", metric), paste("performance_", lrn, "_", limits[1], "_", limits[2], ".pdf", sep = ""))
        plotAbsPerformance(data = res_ovr[learner == lrn, ], outpath = outpath, metric = metric, plot.sd = plot.sd, limits = limits)
    }

    # Plot ranks per problem
    res_ovr = dfr[, .(mean.domHV = mean(V1), mean.rank = mean(V2), sd.domHV = sd(V1) / sqrt(length(V1))), by = c("evals", "variant", "algorithm", "learner", "problem")]
    res_ovr_perprob = dfr[, .(mean.domHV = mean(V1), mean.rank = mean(V2), sd.domHV = sd(V1) / sqrt(length(V1))), by = c("evals", "variant", "algorithm", "problem")]

    for (prob in unique(res_ovr$problem)) {
        outpath = file.path(plotspath, prompt, gsub("\\.", "", metric), prob)
        dir.create(outpath)
        for (lrn in unique(res_ovr$learner)) {
            if (plotRanks) {
            plotRanks(data = res_ovr[learner == lrn & problem == prob, ], outpath = file.path(outpath, paste("ranks_", lrn, ".pdf", sep = "")), metric = metric, limits = limits)
            }
            plotAbsPerformance(data = res_ovr[learner == lrn & problem == prob, ], outpath = file.path(outpath, paste("performance_", lrn, "_", limits[1], "_", limits[2], ".pdf", sep = "")), metric = metric, plot.sd = plot.sd, limits = limits)
            plotPerformanceVsRuntime(data = df[learner == lrn & problem == prob, ], outpath = file.path(outpath, paste("performance_vs_runtime_", lrn, "_", limits[1], "_", limits[2], ".pdf", sep = "")), metric_name = metric, limits = limits)
        }
        if (plotRanks) {
          plotRanks(data = res_ovr_perprob[problem == prob, ], outpath = file.path(outpath, paste("ranks.pdf", sep = "")), metric = metric, limits = limits)   
        }
        plotAbsPerformance(data = res_ovr_perprob[problem == prob, ], outpath = file.path(outpath, paste("performance_", limits[1], "_", limits[2], ".pdf", sep = "")), metric = metric, plot.sd = plot.sd, limits = limits)
    }
}


plotAbsPerformance = function(data, outpath, metric, plot.sd, limits) {
    # rankdata: gives for each version a rank
    ylab_names = data.table(name = c("DomHV[test]", "DomHV[valid]", "trueDomHV[test]"), 
                          mymetric = c("naive.hout.domHV", "eval.domHV", "true.hout.domHV"))
    myname = ylab_names[mymetric == metric, ]$name

    # --- plot stuff over all learners first
    p1 = ggplot()
    p1 = p1 + geom_line(data = data, aes(x = evals, y = mean.domHV, lty = algorithm, colour = variant), size = 0.6)
    p1 = p1 + scale_colour_Publication() + theme_Publication() + scale_fill_Publication()
    p1 = p1 + ylab("Value") + labs(colour = "Variant", lty = "Algorithm") 
    p1 = p1 + theme(legend.direction = "horizontal", legend.position = "top", legend.box = "vertical", legend.box.just = "left")
    p1 = p1 + guides(lty = guide_legend(order = 1), colour = guide_legend(order = 2))
    p1 = p1 + xlab("Evaluations") + ylim(limits) + ylab(myname)
    if (plot.sd) {
      p1 = p1 + geom_ribbon(data = data, aes(x = evals, ymin = mean.domHV - sd.domHV, ymax = mean.domHV + sd.domHV, lty = algorithm, fill = variant), alpha = 0.2)
    }


    ggsave(outpath, p1, width = 12, height = 6, device = "pdf")
}

plotPerformanceVsRuntime = function(data, outpath, metric_name, limits) {
    # rankdata: gives for each version a rank
    ylab_names = data.table(name = c("DomHV[test]", "DomHV[valid]", "trueDomHV[test]"), 
                          mymetric = c("naive.hout.domHV", "eval.domHV", "true.hout.domHV"))
    myname = ylab_names[mymetric == metric, ]$name
    data$id = paste(data$algorithm, data$variant, data$replication, sep = "_")
    data$runtime = log(data$runtime)
    # --- plot stuff over all learners first
    p1 = ggplot()
    p1 = p1 + geom_line(data = data, aes(x = runtime, y = metric, lty = algorithm, colour = variant, group = id), size = 0.6)
    p1 = p1 + scale_colour_Publication() + theme_Publication() + scale_fill_Publication()
    p1 = p1 + ylab("Value") + labs(colour = "Variant", lty = "Algorithm") 
    p1 = p1 + theme(legend.direction = "horizontal", legend.position = "top", legend.box = "vertical", legend.box.just = "left")
    p1 = p1 + guides(lty = guide_legend(order = 1), colour = guide_legend(order = 2))
    p1 = p1 + xlab("Evaluations") + ylim(limits) + ylab(myname)

    ggsave(outpath, p1, width = 9, height = 6, device = "pdf")
}

plotRanks = function(data, outpath, metric, limits) {
    # rankdata: gives for each version a rank
    ylab_names = data.table(name = c("DomHV[test]", "DomHV[valid]", "trueDomHV[test]"), 
                          mymetric = c("naive.hout.domHV", "eval.domHV", "true.hout.domHV"))
    myname = ylab_names[mymetric == metric, ]$name

    # --- plot stuff over all learners first
    p1 = ggplot()
    p1 = p1 + geom_line(data = data, aes(x = evals, y = mean.rank, lty = algorithm, colour = variant), size = 0.6)
    p1 = p1 + scale_colour_Publication() + theme_Publication() + scale_fill_Publication()
    p1 = p1 + ylab("Value") + labs(colour = "Variant", lty = "Algorithm") 
    p1 = p1 + theme(legend.direction = "horizontal", legend.position = "top", legend.box = "vertical", legend.box.just = "left")
    p1 = p1 + guides(lty = guide_legend(order = 1), colour = guide_legend(order = 2))
    p1 = p1 + xlab("Evaluations") + ylab(myname)
    
    ggsave(outpath, p1, width = 12, height = 6, device = "pdf")
}


plotSOperformance = function(res, plotspath, experiments, logscale = FALSE, prompt, limits = c(0.5, 1), height = 10, width = 7) {
    # PUT GENERATIONS HERE 
    # 1) Pepare the dataset for plotting 
    metrics = paste("eval.perf", c(".min", ".mean", ".max"), sep = "")
    metrics = c(metrics, paste("hout.perf", c(".min", ".mean", ".max"), sep = ""))

    df = res[variant %in% experiments$variant, ]
    df = extractFromSummary(df, c("evals", "runtime", metrics))

    df = df[evals <= 4000, ]
    df = df[, replication := 1:length(job.id), by = c("learner", "variant", "problem", "evals")]

    df = renameAndRevalue(df)

    # --- calculate ranks within learner, problem and replication ---
    choseby = c("evals", "variant", "algorithm", "problem", metrics)
    df = df[,  ..choseby]
    res_ovr = df[, lapply(.SD, mean), by = c("evals", "variant", "algorithm", "problem")]

    # --- average across problems

    # ylab_names = data.table(name = c(expression(DomHV[test]), expression(DomHV[valid])), 
    #                       mymetric = c("naive.hout.domHV", "eval.domHV"))
    # mynames = ylab_names[metric %in% mymetric, ]

    # --- plot stuff over all learners first
    names(res_ovr)[5:10] = c("min", "mean", "max", "min", "mean", "max")
    res_ovr = rbind(cbind(type = "eval", res_ovr[, 1:7]), cbind(type = "hout", res_ovr[, c(1:4, 8, 9, 10)]))

    dir.create(file.path(plotspath, prompt))
    dir.create(file.path(plotspath, prompt, gsub("\\.", "", metric)))

    # --- plot stuff over all learners first
    for (prob in unique(res_ovr$problem)) {

        res_prob = res_ovr[problem == prob, ]

        p1 = ggplot()
        p1 = p1 + geom_line(data = res_prob[type == "eval", ], aes(x = evals, y = mean, lty = algorithm, colour = variant), size = 0.6)
        p1 = p1 + scale_colour_Publication() + theme_Publication() 
        p1 = p1 + ylab("Value") + labs(colour = "Variant", lty = "Algorithm") 
        p1 = p1 + theme(legend.direction = "horizontal", legend.position = "top", legend.box = "vertical", legend.box.just = "left")
        p1 = p1 + guides(lty = guide_legend(order = 1), colour = guide_legend(order = 2))
        p1 = p1 + xlab("Evaluations") + ylab(expression(mmce[valid]))

        p2 = ggplot()
        p2 = p2 + geom_line(data = res_prob[type == "hout", ], aes(x = evals, y = mean, lty = algorithm, colour = variant), size = 0.6)
        p2 = p2 + scale_colour_Publication() + theme_Publication() 
        p2 = p2 + ylab("Value") + labs(colour = "Variant", lty = "Algorithm") 
        p2 = p2 + theme(legend.direction = "horizontal", legend.position = "top", legend.box = "vertical", legend.box.just = "left")
        p2 = p2 + guides(lty = guide_legend(order = 1), colour = guide_legend(order = 2))
        p2 = p2 + xlab("Evaluations") + ylab(expression(mmce[test]))
      p = ggarrange(p1, p2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "top")


      }

    ggsave(file.path(plotspath, prompt, gsub("\\.", "", metric), paste("ranksPerLearner_", limits[1], "_", limits[2], ".pdf", sep = "")), p, width = 9, height = 11, device = "pdf")
}

# --- this is not a general function
# --- should be rewritten
renameAndRevalue = function(df) {
    
    # --- renaming ---
    df[algorithm == "randomsearch" & variant == "RS", ]$variant = "O"
    df[algorithm == "randomsearch" & variant == "RSI", ]$variant = "OI"
    df[algorithm == "randomsearch" & variant == "RSIF", ]$variant = "OIFi"
    
    # --- reordering of factors for plots
    library(forcats)
    ord_ages_class = c("O", "OI", "OIFi", "OIFiFm", "OIFiFmS", "OIH", "OIHFiFmS", 
      "BS1RF", "BS2RF", "BSMO", "OIHFiFmS_no_hyperpars", "OIHFiFmS_no_hyperpars500",
      "BSMOF", "OIHFiFmS_preset500")
    df$variant = factor(df$variant, levels = ord_ages_class)
    df$variant = revalue(df$variant, 
      c("O" = "base version", "OI" = "+UI", "OIFi" = "+UI+FI", "OIFiFm" = "+UI+FI+FM", 
        "OIFiFmS" = "+UI+FI+FM (s.a.)", "OIH" = "+UI+HP", "OIHFiFmS" = "+UI+FI+HP+FM (s.a.)", "BS1RF" = "BS1RF", "BS2RF" = "BS2RF", "BSMO" = "BSMO",
        "OIHFiFmS_no_hyperpars" = "+UI+FI+HP+FM (s.a.) / no tune",
        "OIHFiFmS_no_hyperpars500" = "+UI+FI+HP+FM (s.a.) / no tune (500)", 
        "BSMOF" = "BSMO_ensemble", "OIHFiFmS_preset500" = "+UI+FI+HP+FM (s.a.) / tune 500"))
    df$algorithm = revalue(df$algorithm, c("mosmafs" = "NSGA-II", "randomsearch" = "Random Search", "no_feature_sel" = "MBO", "mbo_multicrit" = "MBO"))

    return(df)
}


calculateSummaryOfMethods = function(path, res, maxevals = 4000L) {

    # structure of the table
    # problem | RS (double budget) | RSI (double budget) | RSIF (double budget) | NSGA-II | MOSMAFS
    # jeweils pro problem
    # jeweils für hout and eval 

    # extract performance for half budget
    df = extractFromSummary(res, c("evals", "naive.hout.domHV"))
    df$gen = (df$evals - 80) / 15
    df = df[, replication := 1:length(job.id), by = c("learner", "variant", "problem", "gen")]
    df = df[evals < maxevals, ]

    dfs = df[, list(mean.domHV = mean(naive.hout.domHV, na.rm = TRUE), 
            sd.domHV = sd(naive.hout.domHV, na.rm = T) / sqrt(10)), 
            by = .(variant, problem, learner)]
    # dfs$evaldomHV = paste(round(dfs$mean.domHV, 2), " (", round(dfs$sd.domHV, 2), ")", sep = "")
    dfs$evaldomHV = round(dfs$mean.domHV, 2)

    for (lrn in unique(dfs$learner)) {

        dfs$variant = revalue(dfs$variant, 
          c("O" = "NSGA-II", "OI" = "+UI", "OIFi" = "+UI+FI", "OIFiFm" = "+UI+FI+FM", 
              "OIFiFmS" = "+UI+FI+FM (s.a.)", "OIH" = "+UI+HP", "OIHFiFmS" = "+UI+FI+HP+FM (s.a.)"))


        dfr = dfs[learner == lrn , c("variant", "problem", "evaldomHV")]
        dfc = dcast(dfr, problem ~ variant, value.var = "evaldomHV")
        dfc = ijoin(problems, dfc, by = "problem")
        dfc = dfc[order(dfc$p), ]
        dfc$dummycol = NA
        dfcc = dfc[, c("problem", "NSGA-II", "+UI", "+UI+FI", "+UI+FI+FM", "+UI+FI+FM (s.a.)", "+UI+HP", "+UI+FI+HP+FM (s.a.)", "dummycol", "RS", "RSI", "RSIF")]
        print(xtable::xtable(dfcc, type = "latex", include.rownames=FALSE), file = paste("latex_temp/houtdomHV/", lrn, "complete", "_", maxevals, ".tex", sep = ""))

        dfcc = dfc[, c("problem", "n", "p", "NSGA-II", "+UI+FI+HP+FM (s.a.)", "RS", "RSI", "RSIF")]

        print(xtable::xtable(dfcc, type = "latex", include.rownames=FALSE), file = paste(path, "/houtdomHV/", lrn, "_", maxevals, ".tex", sep = ""))



        # dfr = obj[learner == lrn & variant %in% c("O", "RS", "RSI", "RSIF", "OIHFiFmS"), c("variant", "problem", "houtdomHV")]
        # dfc = dcast(dfr, problem ~ variant, value.var = "houtdomHV")

        # dfc = dfc[, c("problem", "RS", "RSI", "RSIF", "O", "OIHFiFmS")]
        # names(dfc) = c("dataset", "RS", "RSU", "RSUF", "NSGA-II", "MOSMAFS")
        # rownames(dfc) = NULL

        # print(xtable(dfc, type = "latex", include.rownames=FALSE), file = paste("latex_temp/houtdomHV", "_", method, "/", lrn, ".tex", sep = ""))
    }
} 

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

calculateEvalsToRandomsearch = function(res, path, runtime = FALSE) {

    # naive.hout.domHV
    df = extractFromSummary(res, c("evals", "naive.hout.domHV", "runtime"))
    df$gen = (df$evals - 80) / 15
    df$runtime.gen = (df$runtime/df$maxeval)*df$evals # SD
    
    df = df[, replication := 1:length(job.id), by = c("learner", "variant", "problem", "gen")]
    df = df[evals < 4000, ]

    dfr = df[variant %in% c("RS", "RSI", "RSIF"), ]
    dfm = df[algorithm == "mosmafs", ]
    
    res2 = res
    res2 = res2[algorithm == "mosmafs", ]
    res2 = res2[, replication := 1:length(job.id), by = c("learner", "variant", "problem")]

    res2$RS.beat = 0
    res2$RSI.beat = 0
    res2$RSIF.beat = 0
    
    res2$RS.beat.time = 0
    res2$RSI.beat.time = 0
    res2$RSIF.beat.time = 0

    for (repl in 1:10) {
      for (prob in unique(dfm$problem)) {
        for (lrn in unique(dfm$learner)) {
          for (myvariant in unique(dfm$variant)) {
            vals = dfm[replication == repl & problem == prob & learner == lrn & variant == myvariant , ]

            RS = mean(dfr[replication == repl & problem == prob & learner == lrn & variant == "RS" & evals == 3995, ]$naive.hout.domHV, na.rm = TRUE)
            RSI = mean(dfr[replication == repl & problem == prob & learner == lrn & variant == "RSI" & evals == 3995, ]$naive.hout.domHV, na.rm = TRUE)
            RSIF = mean(dfr[replication == repl & problem == prob & learner == lrn & variant == "RSIF" & evals == 3995, ]$naive.hout.domHV, na.rm = TRUE)
     
            res2[replication == repl & replication == repl & replication == repl & learner == lrn & problem == prob & variant == myvariant, ]$RS.beat = vals[naive.hout.domHV >= RS, ][1, ]$evals
            res2[replication == repl & replication == repl & replication == repl & learner == lrn & problem == prob & variant == myvariant, ]$RS.beat.time = vals[naive.hout.domHV >= RS, ][1, ]$runtime.gen #SD
            res2[replication == repl & replication == repl & learner == lrn & problem == prob & variant == myvariant, ]$RSI.beat = vals[naive.hout.domHV >= RSI, ][1, ]$evals
            res2[replication == repl & replication == repl & learner == lrn & problem == prob & variant == myvariant, ]$RSI.beat.time = vals[naive.hout.domHV >= RSI, ][1, ]$runtime.gen # SD
            res2[replication == repl & learner == lrn & problem == prob & variant == myvariant, ]$RSIF.beat = vals[naive.hout.domHV >= RSIF, ][1, ]$evals
            res2[replication == repl & learner == lrn & problem == prob & variant == myvariant, ]$RSIF.beat.time = vals[naive.hout.domHV >= RSIF, ][1, ]$runtime.gen # SD
          }
        }
      }      
    }
    
    saveRDS(res2, file.path(path, "beat_randomsearch_complete.rds"))
  
    # --- imputation evals
    res3 = res2[, .(RS.beat = mean(RS.beat, na.rm = TRUE), 
                    RS.sd = sd(RS.beat, na.rm = TRUE) / sqrt(360), 
                    RS.nas = mean(is.na(RS.beat)) * 100,
                    RSI.beat = mean(RSI.beat, na.rm = TRUE), 
                    RSI.sd = sd(RSI.beat, na.rm = TRUE) / sqrt(360),                     
                    RSI.nas = mean(is.na(RSI.beat)) * 100,
                    RSIF.beat = mean(RSIF.beat, na.rm = TRUE),
                    RSIF.sd = sd(RSIF.beat, na.rm = TRUE) / sqrt(360),
                    RSIF.nas = mean(is.na(RSIF.beat)) * 100,
                    test = length(RS.beat)), by = c("variant")]
    
    res3$RS.beat = paste(round(res3$RS.beat), " (", round(res3$RS.sd), ")", sep = "")
    res3$RSI.beat = paste(round(res3$RSI.beat), " (", round(res3$RSI.sd), ")", sep = "")
    res3$RSIF.beat = paste(round(res3$RSIF.beat), " (", round(res3$RSIF.sd), ")", sep = "")
    res3$RS.nas = round(res3$RS.nas, digits = 1)
    res3$RSI.nas = round(res3$RSI.nas, digits = 1)
    res3$RSIF.nas = round(res3$RSIF.nas, digits = 1)
    
    res3$variant = revalue(res3$variant, 
      c("O" = "NSGA-II", "OI" = "+UI", "OIFi" = "+UI+FI", "OIFiFm" = "+UI+FI+FM", 
        "OIFiFmS" = "+UI+FI+FM (s.a.)", "OIH" = "+UI+HP", "OIHFiFmS" = "+UI+FI+HP+FM (s.a.)"))

    names(res3) = c(" ", "RS", "RS.sd", "NC.1", "RS+UI", "RSI.sd", "NC.2", "RS+UI+IF", "RSUIIF.sd", "NC.3", "test")

    print(xtable::xtable(res3[, c(" ", "RS", "NC.1", "RS+UI", "NC.2", "RS+UI+IF", "NC.3")], 
      type = "latex", include.rownames=FALSE), file = paste("latex_temp/beatRS_evals_with_nas_average_after.tex", sep = ""))
    
    
    # --- imputation runtime
    res4 = res2[, .(RS.beat= mean(RS.beat.time, na.rm = TRUE), 
      RS.sd = sd(RS.beat.time, na.rm = TRUE) / sqrt(360), 
      RS.nas = mean(is.na(RS.beat.time)) * 100,
      RSI.beat = mean(RSI.beat.time, na.rm = TRUE), 
      RSI.sd = sd(RSI.beat.time, na.rm = TRUE) / sqrt(360),                     
      RSI.nas = mean(is.na(RSI.beat.time)) * 100,
      RSIF.beat = mean(RSIF.beat.time, na.rm = TRUE),
      RSIF.sd = sd(RSIF.beat.time, na.rm = TRUE) / sqrt(360),
      RSIF.nas = mean(is.na(RSIF.beat.time)) * 100,
      test = length(RS.beat.time)), by = c("variant")]
    
    res4$RS.beat = paste(round(res4$RS.beat), " (", round(res4$RS.sd), ")", sep = "")
    res4$RSI.beat = paste(round(res4$RSI.beat), " (", round(res4$RSI.sd), ")", sep = "")
    res4$RSIF.beat = paste(round(res4$RSIF.beat), " (", round(res4$RSIF.sd), ")", sep = "")
    res4$RS.nas = round(res4$RS.nas, digits = 1)
    res4$RSI.nas = round(res4$RSI.nas, digits = 1)
    res4$RSIF.nas = round(res4$RSIF.nas, digits = 1)
    
    res4$variant = revalue(res4$variant, 
      c("O" = "NSGA-II", "OI" = "+UI", "OIFi" = "+UI+FI", "OIFiFm" = "+UI+FI+FM", 
        "OIFiFmS" = "+UI+FI+FM (s.a.)", "OIH" = "+UI+HP", "OIHFiFmS" = "+UI+FI+HP+FM (s.a.)"))
    
    names(res4) = c(" ", "RS", "RS.sd", "NC.1", "RS+UI", "RSI.sd", "NC.2", "RS+UI+IF", "RSUIIF.sd", "NC.3", "test")
    
    print(xtable::xtable(res4[, c(" ", "RS", "NC.1", "RS+UI", "NC.2", "RS+UI+IF", "NC.3")], 
      type = "latex", include.rownames=FALSE), file = paste("latex_temp/beatRS_runtime_with_nas_average_after.tex", sep = ""))
    
}


plotFrontPerProblem = function(path, parfront) {

  for (prob in problems$problem) {

  allparetos = parfrnt[problem == prob, ]
  allparetos = allparetos[expname %in% c("O", "OIHFiFmS", "RS", "RSI", "RSIF"), ]

  allparetos$expname = revalue(allparetos$expname, 
      c("O" = "NSGAII", "OI" = "NSGAII+UI", "OIFi" = "NSGAII+UI+FI", "OIFiFm" = "NSGAII+UI+FI+FM", 
          "OIFiFmS" = "NSGAII+UI+FI+FM(s.a.)", "OIH" = "NSGAII+UI+HP", "OIHFiFmS" = "NSGAII+UI+FI+HP+FM(s.a.)", 
          "RS" = "RS", "RSI" = "RS+UI", "RSIF" = "RS+UI+IF"))
  allparetos$expname = factor(allparetos$expname, levels = c("RS", "RS+UI", "RS+UI+IF", "NSGAII", "NSGAII+UI+FI+HP+FM(s.a.)"))
  allparetos$learner = factor(allparetos$learner, levels = c("SVM", "kknn", "xgboost"))
  
  p = ggplot(allparetos, aes(x = mmce, y = featfrac, group = instance)) 
  p = p + geom_polygon(data = allparetos, fill = "grey", alpha = 0.05) 
  p = p + geom_line(data = allparetos, colour = "grey", alpha = 0.6) 
  p = p + geom_point(data = allparetos[point == TRUE], color = "#386cb0", alpha = 0.4)
  p = p + scale_colour_Publication() + theme_Publication() + scale_fill_Publication()
  # p = p + ylab("Value") + labs(colour = "Variant", lty = "Algorithm") 
  p = p + labs(colour = "", fill = "")
  p = p + facet_grid(expname ~ learner) 
  p = p + xlim(c(0, 1)) + ylim(c(0, 1)) + coord_fixed()
  p = p + xlab(expression(mmce[outer])) + ylab("Fraction of features selected")
  p = p + theme(legend.position = "none")
  # p = p + theme(
  #   strip.background = element_blank(),
  #   strip.text.x = element_blank(),
  #   strip.text.y = element_blank(),
  #   legend.position = "right", legend.direction = "vertical", legend.box = "vertical")
  # p = p + guides(colour = guide_legend(override.aes = list(size = 4, alpha = 0.6)))

  ggsave(file.path(path, "front", paste("all_variants", "_", prob, ".pdf", sep = "")), p, width = 9, height = 12, device = "pdf")
}

}

getHyperparamsPerProblem = function(x, evals = NULL) {
  
  path = as.data.frame(x$result$opt.path)
  if (!is.null(evals))
    path = path[1:evals, ]
  best_id = which.min(path$y)
  best = as.list(path[best_id,][, !names(path) %in% c("y")])

  return(best)
}


# --- FOR LATER


    # if (!x_runtime) {
    # } else {
    #     df$runtime = df$runtime / 60 # minutes
    #     df$runtime_binned = floor(df$runtime / 5) # FLOOR OR ROUND?
    #     dfr = df[, max(metric), by = c("algorithm", "variant", "learner", "problem", "replication", "runtime_binned")]
    # }


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

  return(list(result.pf.list, result.pf.test.list, pareto.time))
}





