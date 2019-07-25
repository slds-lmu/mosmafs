# helpers

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

saveOpenMLTask = function(id, path) {
  task = convertOMLTaskToMlr(getOMLTask(id))$mlr.task
  task.id = task$task.desc$id
  dir.create(file.path(path, task.id))
  saveRDS(task, paste(path, task.id, "task.rds", sep = "/"))
}

fromDataToTask = function(path, id, target) {
  f = list.files(file.path(path, id), full.names = TRUE)
  data = do.call("rbind", lapply(f,  RWeka::read.arff))
  task = makeClassifTask(id = id, data = data, target = target)
  saveRDS(task, paste(path, id, "task.rds", sep = "/"))
}

saveHypersphereTask = function(path = "data", p.inf = 10, p.noise, n.train = 200, n.test = 10000, r = 1.8) {
  id = paste("hypersphere", n.train, p.inf + p.noise, sep = ".")
  dir.create(file.path(path, id))
  task = create.hypersphere.data(p.inf, n.train + n.test, radius = r) %>% create.classif.task(id = id) %>% task.add.random.cols(num = p.noise)   
  data = getTaskData(task)
  names(data)[length(data)] = "class"
  write.arff(data[1:200, ], file = file.path(path, id, "train.arff"))
  write.arff(data[201:nrow(data), ], file = file.path(path, id, "test.arff"))
}

calculateRanks = function(dfr, nevals) {
  dfr = dfr[evals <= nevals, ]
  dfr = dfr[, .SD[which.max(generation)], by = job.id]
  dfr = dfr[, replication := 1:length(job.id), by = c("problem", "mu", "lambda", "learner")]
  dfr = dfr[, ranks_hypervol := rank(1 - hypervol), by = c("problem", "learner", "replication")]    
  dfr = dfr[, mean(ranks_hypervol), by = c("mu", "lambda")]              
  return(dfr)
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



# datapath = "data"
# dirs = list.dirs(datapath)
# for (i in 1:length(dirs)) {
#   file = dirs[i]
#   if (file.exists(file.path(file, "task.rds"))) {
#       task = readRDS(file.path(file, "task.rds"))
#       data = getTaskData(task)
#       data = data[data$class %in% c(6, 9), ]
#       data$class = as.factor(as.character(data$class))
#       task = makeClassifTask(data = data, target = "class")
#       outer.res.inst = makeResampleInstance(makeResampleDesc("Holdout", stratify = TRUE, split = 0.7), task)
#       train.data = data[outer.res.inst$train.inds[[1]], ]
#       names(train.data)[length(names(train.data))] = "class"
#       test.data = data[outer.res.inst$test.inds[[1]], ]
#       names(test.data)[length(names(test.data))] = "class"
#       saveRDS(train.data, file = file.path(file, "train.arff.rds"))
#       saveRDS(test.data, file = file.path(file, "test.arff.rds"))
#   }
#   if (file.exists(file.path(file, "train.arff"))) {
#       train.data = read.arff(file.path(file, "train.arff"))
#       names(train.data)[length(names(train.data))] = "class"
#       test.data = read.arff(file.path(file, "test.arff"))
#       names(test.data)[length(names(test.data))] = "class"
#       saveRDS(train.data, file = file.path(file, "train.arff.rds"))
#       saveRDS(test.data, file = file.path(file, "test.arff.rds"))
#   }
# }

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

  assertNumeric(ref.point, any.missing = FALSE, finite = TRUE,
    len = ecr.object$task$n.objectives)
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
  if (length(fitnesses) == 1L) # ugly fix for calculating traces for randomsearch
      fitnesses = lapply(c(seq(80, 4000, by = 15), 4000), function(i) fitnesses[[1]][, 1:i])


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
  resdf$evals = c(seq(80, 4000, by = 15), 4000)

  hofitnesses <- popAggregate(ecr.object$log, "fitness.holdout")
  if (length(hofitnesses) == 1L) # ugly fix for calculating traces for randomsearch
      hofitnesses = lapply(c(seq(80, 4000, by = 15), 4000), function(i) hofitnesses[[1]][, 1:i])

  if (any(vlapply(hofitnesses, function(x) any(is.finite(x))))) {

    corcols <- lapply(seq_len(ecr.object$task$n.objectives), function(idx) {
      mapply(function(eval.fit, hout.fit) {
        suppressWarnings(cor.fun(eval.fit[idx, ], hout.fit[idx, ]))
      }, fitnesses, hofitnesses)
    })
    names(corcols) <- rownames(fitnesses[[1]]) %??% paste0("obj.", seq_len(corcols))


    true.hout.domHV <- mapply(function(eval.fit, hout.fit) {
      unbiasedHoldoutDomHV(eval.fit, hout.fit, ref.point)
    }, fitnesses, hofitnesses)

    naive.hout.domHV <- mapply(function(eval.fit, hout.fit) {
      naiveHoldoutDomHV(eval.fit, hout.fit, ref.point)
    }, fitnesses, hofitnesses)

    resdf <- cbind(resdf, hout = aggregate.fitness(hofitnesses),
      true.hout.domHV, naive.hout.domHV,
      cor = corcols)
  }
  resdf
}

collectResultMBO = function(object, ecr.object, aggregate.perresult = list(domHV = function(x) computeHV(x, ref.point)), aggregate.perobjective = list("min", "mean", "max"), ref.point = smoof::getRefPoint(ecr.object$control$task$fitness.fun), cor.fun = cor) {
  opt.path <- data.frame(object$result$opt.path)
  resdf <- data.table()
  resdf$gen <- 0:(length(object$result.pf)-1)
  evals <- as.integer(names(object$result.pf))
  resdf$runtime <- cumsum(rowSums(opt.path[evals, c("train.time",  "propose.time", "exec.time")], na.rm = TRUE))
  resdf$evals <- evals
  resdf
  
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
    train <- train[, c(2, 1)]
    train.perf <- receive.perf(train, c("eval.perf.min", "eval.perf.mean", 
      "eval.perf.max", "eval.propfeat.min", 
      "eval.propfeat.mean", "eval.propfeat.max", "eval.domHV"))
    
    # Get summaries performance holdout data
    hold$rn <- as.numeric(hold$rn)
    hold <- hold[, c(2, 1)]
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

# CollectResults
collectBenchmarkResults = function(path, experiments, tab) {
  
  for (experiment in names(experiments)) {
    toreduce = ijoin(tab, experiments[[experiment]], by = names(experiments[[experiment]]))
    toreduce = ijoin(toreduce, findDone(), by = "job.id")

    dir = as.numeric(sapply(list.files("registry/results/"), function(x) strsplit(x, ".rds")[[1]][1]))
    dir = data.frame(job.id = dir)
    toreduce = ijoin(toreduce, dir)

    res = reduceResultsDataTable(toreduce, function(x) collectResult(x$result))
    res = ijoin(tab, res, by = "job.id")
    res$variant = experiment

    # pops = reduceResultsDataTable(toreduce, function(x) lapply(getPopulations(x$result$log), function(x) x$fitness))
    # pops = ijoin(tab, pops, by = "job.id")
    # pops$variant = experiment

    dir.create(file.path(path, experiment))

    saveRDS(res, file.path(path, experiment, "result.rds"))
    # saveRDS(pops, file.path(path, experiment, "population.rds"))

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


theme_Publication <- function(base_size=14, base_family="helvetica") {
      library(grid)
      library(ggthemes)
      (theme_foundation(base_size=base_size)
       + theme(plot.title = element_text(face = "bold",
                                         size = rel(1.2), hjust = 0.5),
               text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(face = "bold",size = rel(1)),
               axis.title.y = element_text(angle=90,vjust =2),
               axis.title.x = element_text(vjust = -0.2),
               axis.text = element_text(), 
               axis.line = element_line(colour="black"),
               axis.ticks = element_line(),
               panel.grid.major = element_line(colour="#f0f0f0"),
               panel.grid.minor = element_blank(),
               legend.key = element_rect(colour = NA),
               legend.position = "bottom",
               legend.direction = "horizontal",
               legend.key.size= unit(0.5, "cm"),
               legend.margin = unit(0, "cm"),
               legend.title = element_text(face="italic"),
               plot.margin=unit(c(10,5,5,5),"mm"),
               strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
               strip.text = element_text(face="bold")
          ))
      
}

scale_fill_Publication <- function(...){
      library(scales)
      discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#00856E","#000000")), ...)

}

scale_colour_Publication <- function(...){
      library(scales)
      discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#00856E","#000000")), ...)

}

scale_colour_spec <- function(...){
      library(scales)
      discrete_scale("colour","Publication",manual_pal(values = c("#000000", "#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
}


plotOptPathHypervol = function(res, plotspath) {
  df = extractFromSummary(res, c("evals", "eval.domHV"))
  df = df[- which(problem %in% c("hypersphere.200.50", "hypersphere.200.200")), ] 
  df = df[, replication := 1:length(job.id), by = c("learner", "variant", "problem", "evals")]
 
  status_finished = df[, max(replication), by = c("variant", "problem", "learner")]
  status_finished = status_finished[, sum(V1), by = c("problem", "learner")]
  status_finished = status_finished[V1 == 100, ]
  df = ijoin(df, status_finished, by = c("problem", "learner")) 

  dfp = df[, .(mean.domHV = mean(eval.domHV)), by = c("algorithm", "evals", "problem", "learner", "variant")]
  dfp = dfp[evals != 8000, ]
  dfp = dfp[variant %in% c("RS", "RSI", "RSIF", "O", "OIHFiFmS"), ]
  dfp = dfp[evals <= 4000, ]
  dfp$variant = revalue(dfp$variant, c("RS" = "RS", "RSI" = "RSU", "RSIF" = "RSUF", "O" = "NSGA-II", "OIHFiFmS" = "MOSMAFS"))

  for (lrn in unique(status_finished$learner)) {
    plist = list()
    for (prob in unique(dfp$problem)) {
      plist[[prob]] = ggplot()
      plist[[prob]] = plist[[prob]] + geom_line(data = dfp[algorithm == "mosmafs" & learner == lrn & problem == prob, ], aes(x = evals, y = mean.domHV, colour = variant))
      plist[[prob]] = plist[[prob]] + geom_hline(data = dfp[algorithm == "randomsearch"  & learner == lrn & problem == prob, ], aes(yintercept = mean.domHV, lty = variant))
      plist[[prob]] = plist[[prob]] + theme_bw() + labs(colour = "") + labs(lty = "") + ggtitle(prob)
      plist[[prob]] = plist[[prob]] + xlab("evaluations") + ylab("domHV (inner)")
      plist[[prob]] = plist[[prob]] + scale_colour_Publication() + theme_Publication()
      mylegend = g_legend(plist[[prob]])
      plist[[prob]] = plist[[prob]] + theme(legend.position = "none")
    }
    p3 = do.call("grid.arrange", c(plist, nrow = 2))
    p3 = grid.arrange(arrangeGrob(p3),
             mylegend, nrow = 2, heights =c(10, 1))
  ggsave(file.path(plotspath, "eval.domHV", paste(lrn, "all.png", sep = "_")), p3, width = 8, height = 6)
  }

  ggsave(file.path(plotspath, "eval.domHV", "all.png"), width = 10, height = 10)

  for (prob in unique(dfp$problem)) {
    for (lrn in unique(dfp$learner)) {
      p = ggplot()
      p = p + geom_line(data = dfp[algorithm == "mosmafs" & learner == lrn & problem == prob, ], aes(x = evals, y = mean.domHV, colour = variant))
      p = p + geom_hline(data = dfp[algorithm == "randomsearch" & learner == lrn & problem == prob, ], aes(yintercept = mean.domHV, lty = variant))
      p = p + theme_Publication()
      p = p + scale_colour_Publication()
      ggsave(file.path(plotspath, "eval.domHV", paste(lrn, prob, "png", sep = ".")))
    }
  }

}


plotPerformanceEval = function(res, plotspath) {
  df = extractFromSummary(res, c("evals", "eval.perf.min", "eval.perf.mean", "eval.perf.max"))
  df = df[- which(problem %in% c("hypersphere.200.50", "hypersphere.200.200")), ] 
  df = df[, replication := 1:length(job.id), by = c("learner", "variant", "problem", "evals")]
 
  dfp = df[, .(min.perf = mean(eval.perf.min), mean.perf = mean(eval.perf.mean), max.perf = mean(eval.perf.max)), by = c("algorithm", "evals", "problem", "learner", "variant")]
  dfp = dfp[evals != 8000, ]

  p = ggplot()
  p = p + geom_line(data = dfp[algorithm == "mosmafs", ], aes(x = evals, y = mean.domHV, colour = variant))
  p = p + geom_hline(data = dfp[algorithm == "randomsearch", ], aes(yintercept = mean.domHV, colour = variant))
  p = p + facet_grid(learner ~ problem) + theme_bw()
  p = p + scale_colour_Publication() + theme_Publication()

  ggsave(file.path(plotspath, "eval.domHV", "all.png"), width = 10, height = 10)

  for (prob in unique(dfp$problem)) {
    for (lrn in unique(dfp$learner)) {
      p = ggplot()
      p = p + geom_line(data = dfp[algorithm == "mosmafs" & learner == lrn & problem == prob, ], aes(x = evals, y = min.perf, colour = variant))
      p = p + geom_hline(data = dfp[algorithm == "randomsearch" & learner == lrn & problem == prob, ], aes(yintercept = min.perf, colour = variant))
      p = p + theme_Publication()
      p = p + scale_colour_Publication()
      ggsave(file.path(plotspath, "performance", paste(lrn, prob, "png", sep = ".")), width = 5, height = 5)
    }
  }

}


plotPerformanceHout = function(res, plotspath) {
  df = extractFromSummary(res, c("evals", "hout.perf.min", "hout.perf.mean", "hout.perf.max"))
  df = df[- which(problem %in% c("hypersphere.200.50", "hypersphere.200.200")), ] 
  dfp = df[, .(min.perf = mean(hout.perf.min), mean.perf = mean(hout.perf.mean), max.perf = mean(max)), by = c("algorithm", "evals", "problem", "learner", "variant")]
  dfp = dfp[evals != 8000, ]

  for (prob in unique(dfp$problem)) {
    for (lrn in unique(dfp$learner)) {
      p = ggplot()
      p = p + geom_line(data = dfp[algorithm == "mosmafs" & learner == lrn & problem == prob, ], aes(x = evals, y = min.perf, colour = variant))
      p = p + geom_hline(data = dfp[algorithm == "randomsearch" & learner == lrn & problem == prob, ], aes(yintercept = min.perf, colour = variant))
      p = p + theme_Publication()
      p = p + scale_colour_Publication()
      ggsave(file.path(plotspath, "performance_hout", paste(lrn, prob, "png", sep = ".")), width = 5, height = 5)
    }
  }

}


# plotHvOverRuntime = function(res, plotspath, metric = "naive.hout.domHV", heights = 7, width = 7) {
#   df = extractFromSummary(res, c("evals", "runtime", metric))
#   df = df[evals < 4000, ]
#   df$gen = (df$evals - 80) / 15
#   df$runtime.gen = (df$runtime/df$maxeval)*df$evals
#   df = df[, replication := 1:length(job.id), by = c("learner", "variant", "problem", "gen")]
#   df = renameAndRevalue(df)
#   names(df)[17] = "metric"
#   
#   for (lrn in unique(df$learner)) {
#     for (prob in unique(df$problem)) {
#       df.sub = df[problem == prob & learner == lrn, ]
#       df.sub$time.interval = cut(df.sub$runtime.gen, breaks = 10)
#       dfm = df.sub[, meanhv := mean(metric), by = c("time.interval")]
#     }
#   }
#   
#   dfr = df[, mean_hv := mean(metric), by = c("learner", "problem", "evals", "replication")]
#   
# }

plotRanks = function(res, plotspath, logscale = FALSE, metric = "naive.hout.domHV", limits = c(0.37, 1), height = 10, width = 7) {
    
    # --- naive.hout.domHV
    df = extractFromSummary(res, c("evals", metric))
    df = df[evals < 4000, ]
    df$gen = (df$evals - 80) / 15
    df = df[, replication := 1:length(job.id), by = c("learner", "variant", "problem", "gen")]
    df = renameAndRevalue(df)
    names(df)[17] = "metric"

    # --- calculate ranks within learner, problem and replication ---
    dfr = df[, rank_variant := rank(- metric), by = c("learner", "problem", "evals", "replication")]
    
    # --- average domHV and ranks across replications
    dfr = dfr[, .(mean(metric), mean(rank_variant)), by = c("algorithm", "learner", "problem", "evals", "variant")]
    dfr$V2 = (dfr$V2 / 10)

    # --- average across problems
    res_ovr = dfr[, .(mean.domHV = mean(V1), mean.rank = mean(V2)), by = c("evals", "variant", "algorithm")]
    res_ovr = melt(res_ovr, id.vars = c("evals", "variant", "algorithm"))    
    res_ovr$variable = revalue(res_ovr$variable, c("mean.domHV" = "Mean DomHV", "mean.rank" = "Mean Ranks"))

    res_ovr_pl = dfr[, .(mean.domHV = mean(V1), mean.rank = mean(V2)), by = c("evals", "variant", "algorithm", "learner")]
    res_ovr_pl = melt(res_ovr_pl, id.vars = c("evals", "variant", "algorithm", "learner"))
    res_ovr_pl$variable = revalue(res_ovr_pl$variable, c("mean.domHV" = "Mean DomHV", "mean.rank" = "Mean Ranks"))

    # --- plot stuff over all learners first
    p = ggplot()
    p = p + geom_line(data = res_ovr, aes(x = evals, y = value, lty = algorithm, colour = variant), size = 0.6)
    p = p + scale_colour_Publication() + theme_Publication() 
    p = p + facet_grid(. ~ variable, scales = 'free') + labs(y="value_label_1") 
    p = p + ylab("Value") + labs(colour = "Variant", lty = "Algorithm") 
    p = p + scale_y_continuous(limits = limits, sec.axis = sec_axis(~.*10))
    p = p + theme(legend.direction = "horizontal", legend.position = "top", legend.box = "vertical", legend.box.just = "left")
    p = p + guides(lty = guide_legend(order = 1), colour = guide_legend(order = 2))
    p = p + xlab("Evaluations")
   
    ggsave(file.path(plotspath, paste(gsub("\\.", "", metric), "ranks.pdf", sep = "_")), p, width = 9, height = 6, device = "pdf")

    p = ggplot()
    p = p + geom_line(data = res_ovr_pl, aes(x = evals, y = value, lty = algorithm, colour = variant), size = 0.6)
    p = p + scale_colour_Publication() + theme_Publication() 
    p = p + facet_grid(learner ~ variable) 
    p = p + ylab("Value") + labs(colour = "Variant", lty = "Algorithm") 
    p = p + scale_y_continuous(limits = limits, sec.axis = sec_axis(~.*10))
    p = p + theme(legend.direction = "horizontal", legend.position = "top", legend.box = "vertical", legend.box.just = "left")
    p = p + guides(lty = guide_legend(order = 1), colour = guide_legend(order = 2))
    p = p + xlab("Evaluations")
      
    ggsave(file.path(plotspath, paste(gsub("\\.", "", metric), "ranks_perLearner.pdf", sep = "_")), p, width = 7, height = 10)
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
    ord_ages_class = c("O", "OI", "OIFi", "OIFiFm", "OIFiFmS", "OIH", "OIHFiFmS")
    df$variant = factor(df$variant, levels = ord_ages_class)
    df$variant = revalue(df$variant, 
      c("O" = "base version", "OI" = "+UI", "OIFi" = "+UI+FI", "OIFiFm" = "+UI+FI+FM", 
        "OIFiFmS" = "+UI+FI+FM (s.a.)", "OIH" = "+UI+HP", "OIHFiFmS" = "+UI+FI+HP+FM (s.a.)"))
    df$algorithm = revalue(df$algorithm, c("mosmafs" = "NSGA-II", "randomsearch" = "Random Search"))

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