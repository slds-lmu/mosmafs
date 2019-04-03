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
      (theme_foundation(base_size=base_size, base_family=base_family)
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
      discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}

scale_colour_Publication <- function(...){
      library(scales)
      discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

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
 
  status_finished = df %>% group_by(variant, problem, learner) %>% summarize(num.repls = max(replication)) %>% filter(num.repls == 10)
  df = ijoin(df, status_finished, by = c("problem", "learner", "variant")) 

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


plotRanks = function(res, plotspath, steps) {
    # naive.hout.domHV
    df = extractFromSummary(res, c("evals", "naive.hout.domHV"))
    df = df[- which(problem %in% c("hypersphere.200.50", "hypersphere.200.200", "philippine", "dilbert", "AP_Lung_Uterus", "eating")), ] 
    df = df[evals != 8000, ]
    df$gen = (df$evals - 80) / 15
    df = df[, replication := 1:length(job.id), by = c("learner", "variant", "problem", "gen")]
    df = df[evals < 4000, ]

    # remove the ones where we have not enough replications
    finished_exps = df[, .(num.repls = max(replication)), by = c("variant", "problem", "learner")]
    dfs = ijoin(df, finished_exps[num.repls == 10, ], by = c("problem", "learner", "variant")) 
    dfs = dfs[problem %in% c("USPS", "madeline", "madelon", "lsvt", "isolet", "cnae-9"), ]

    # --- calculate ranks within learner, problem and replication ---
    dfr = dfs[, rank_variant := rank(- naive.hout.domHV), by = c("learner", "problem", "evals", "replication")]
    # average across replications
    dfr = dfr[, mean(rank_variant), by = c("learner", "problem", "evals", "variant")]
    # average across problems
    ranks_overall = dfr[, mean(V1), by = c("evals", "variant")]
    ranks_overall = ijoin(ranks_overall, unique(res[, c("variant", "algorithm")]), by = "variant")
    ranks_overall[algorithm == "randomsearch" & variant == "RS", ]$variant = "O"
    ranks_overall[algorithm == "randomsearch" & variant == "RSI", ]$variant = "OI"
    ranks_overall[algorithm == "randomsearch" & variant == "RSIF", ]$variant = "OIFi"

    library(forcats)
    ord_ages_class = c("O", "OI", "OIFi", "OIFiFm", "OIFiFmS", "OIH", "OIHFiFmS")
    ranks_overall$variant = factor(ranks_overall$variant, levels = ord_ages_class)

    names(ranks_overall) = c("Evaluations", "Variant", "MeanRank", "Algorithm")

    ranks_overall$Variant = revalue(ranks_overall$Variant, 
      c("O" = "base version", "OI" = "+UI", "OIFi" = "+UI+FI", "OIFiFm" = "+UI+FI+FM", 
        "OIFiFmS" = "+UI+FI+FM (s.a.)", "OIH" = "+UI+HP", "OIHFiFmS" = "+UI+FI+HP+FM (s.a.)"))
    ranks_overall$Algorithm = revalue(ranks_overall$Algorithm, c("mosmafs" = "NSGA-II", "randomsearch" = "Random Search"))

    scale_colour_spec <- function(...){
          library(scales)
          discrete_scale("colour","Publication",manual_pal(values = c("#000000", "#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
    }

    p = ggplot()
    p = p + geom_line(data = ranks_overall, aes(x = Evaluations, y = MeanRank, lty = Algorithm, colour = Variant), size = 1)
    p = p + theme_bw()
    p = p + scale_colour_Publication() + theme_Publication()
    p = p + ylab("Mean DomHV (outer mmce)")
    p = p + theme(legend.title = element_blank(), legend.direction = "vertical", legend.position = "bottom", legend.box = "horizontal")
    # p = p + guides(color = guide_colorbar(order = 1), lty = guide_legend(order = 0))  
    ggsave(file.path(plotspath, "ranks.domHV", "ranks_hout_ge100.png"), p, width = 7, height = 6)

    for (lrn in unique(dfr$learner)) {
      dfr_lrn = dfr[learner == lrn, ]
      ranks_overall = dfr_lrn[, mean(V1), by = c("evals", "variant")]
      names(ranks_overall) = c("Generation", "Variant", "MeanRank")

      p = ggplot()
      p = p + geom_line(data = ranks_overall, aes(x = Generation, y = MeanRank, colour = Variant))
      p = p + theme_bw()
      p = p + scale_colour_Publication() + theme_Publication()
      ggsave(file.path(plotspath, "ranks.domHV", "perLearner", paste(lrn, "ranks.png", sep = ".")), p)
    }


    for (prob in unique(dfr$problem)) {
      dfr_prob = dfr[problem == prob, ]
      ranks_overall = dfr_prob[, mean(V1), by = c("evals", "variant")]
        names(ranks_overall) = c("Generation", "Variant", "MeanRank")

        p = ggplot()
        p = p + geom_line(data = ranks_overall, aes(x = Generation, y = MeanRank, colour = Variant))
      p = p + theme_bw()
      p = p + scale_colour_Publication() + theme_Publication()
      ggsave(file.path(plotspath, "ranks.domHV", "perProblem", paste(prob, "ranks.png", sep = ".")), p)
    }

  for (lrn in unique(dfr$learner)) {
      for (prob in unique(dfr$problem)) {
      dfr_prob = dfr[problem == prob & learner == lrn, ]
      ranks_overall = dfr_prob[, mean(V1), by = c("evals", "variant")]
        names(ranks_overall) = c("Generation", "Variant", "MeanRank")

        p = ggplot()
        p = p + geom_line(data = ranks_overall, aes(x = Generation, y = MeanRank, colour = Variant))
      p = p + theme_bw()
      p = p + scale_colour_Publication() + theme_Publication()
      ggsave(file.path(plotspath, "ranks.domHV", "perLearnerperProblem", paste(lrn, prob, "ranks.png", sep = ".")), p)
      }
  }
}


plotFeatures = function(res, plotspath) {

}


plotHeatmap = function(populations, plotspath) {

  for (prob in unique(populations$problem)) {

    for (lrn in unique(populations$learner)) {

    plist = list()

      for (vari in unique(populations$variant)) {
          pops = populations[problem == prob & learner == lrn & variant == vari, ]
          popls = list()

          for (i in 1:nrow(pops)) {
            popres = pops[i, ]$result[[1]]
            popres = lapply(1:length(popres), function(i) popres[[i]][, which(doNondominatedSorting(popres[[i]])$ranks == 1)])
            popres = lapply(1:length(popres), function(x) cbind(gen = x, as.data.frame(t(popres[[x]]))))
            popres = do.call("rbind", popres)
            popres = cbind(pops[i, ], popres)
            names(popres)[19:20] = c("mmce", "nfeat")
            popls[[i]] = popres
          }

          popls = do.call("rbind", popls)
          popls = popls[popls$gen %% 50 == 1, ]
          
          p = ggplot()

          for (job in unique(popls$job.id)) {
            p = p + geom_point(data = popls[job.id == job, ], aes(x = mmce, y = nfeat, colour = gen), alpha = 0.4, size = 1)
            p = p + theme_bw()
            p = p + theme_Publication()
            p = p + theme(legend.position = "none")
          }
          p = p + ggtitle(vari)
          plist[[vari]] = p
        } 
        g = do.call("grid.arrange", plist)
        savepath = file.path(plotspath, "front", lrn)
        dir.create(savepath)
        ggsave(file.path(savepath, paste(prob, "front.png", sep = ".")), g, width = 15, height = 10)
    }
  }
}



calculateSummaryOfMethods = function(res) {

    # structure of the table
    # problem | RS (double budget) | RSI (double budget) | RSIF (double budget) | NSGA-II | MOSMAFS
    # jeweils pro problem
    # jeweils für hout and eval 

    # extract performance for half budget
    df = extractFromSummary(res, c("evals", "naive.hout.domHV"))
    df = df[- which(problem %in% c("hypersphere.200.50", "hypersphere.200.200", "philippine", "dilbert", "AP_Lung_Uterus", "eating", "gina_agnostic")), ] 
    df = df[evals != 8000, ]
    df$gen = (df$evals - 80) / 15
    df = df[, replication := 1:length(job.id), by = c("learner", "variant", "problem", "gen")]
    df = df[evals < 4000, ]

    # --- double budget consideration
    dfs = df[, list(mean.domHV = mean(naive.hout.domHV, na.rm = TRUE), 
            sd.domHV = sd(naive.hout.domHV, na.rm = T) / sqrt(10)), 
            by = .(variant, problem, learner)]
    dfs$evaldomHV = paste(round(dfs$mean.domHV, 2), " (", round(dfs$sd.domHV, 2), ")", sep = "")
    
    for (lrn in dfs$learner) {
        dfr = dfs[learner == lrn , c("variant", "problem", "evaldomHV")]
        dfr = dfr[variant %in% c("O", "OIHFiFmS", "RS", "RSI", "RSIF")]
        dfc = dcast(dfr, problem ~ variant, value.var = "evaldomHV")

        print(xtable(dfc, type = "latex", include.rownames=FALSE), file = paste("latex_temp/houtdomHV", "_singlebudget/", lrn, ".tex", sep = ""))

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

calculateEvalsToRandomsearch = function(res) {

    # naive.hout.domHV
    df = extractFromSummary(res, c("evals", "naive.hout.domHV"))
    df = df[- which(problem %in% c("hypersphere.200.50", "hypersphere.200.200", "philippine", "dilbert", "AP_Lung_Uterus", "eating")), ] 
    df = df[evals != 8000, ]
    df$gen = (df$evals - 80) / 15
    df = df[, replication := 1:length(job.id), by = c("learner", "variant", "problem", "gen")]
    df = df[evals < 4000, ]

    # remove the ones where we have not enough replications
    finished_exps = df[, .(num.repls = max(replication)), by = c("variant", "problem", "learner")]
    dfs = ijoin(df, finished_exps[num.repls == 10, ], by = c("problem", "learner", "variant")) 

    dfr = dfs[algorithm == "randomsearch", ]
    dfm = dfs[algorithm == "mosmafs", ]
    
    res2 = res
    res2$RS.beat = 0
    res2$RSI.beat = 0
    res2$RSIF.beat = 0
    res2 = res2[algorithm == "mosmafs", ]
    res2 = res2[- which(problem %in% c("hypersphere.200.50", "hypersphere.200.200", "philippine", "dilbert", "AP_Lung_Uterus", "eating", "gina_agnostic")), ] 

    for (repl in 1:10) {
      for (prob in unique(dfm$problem)) {
        for (lrn in unique(dfm$learner)) {
          for (myvariant in unique(dfm$variant)) {
            vals = dfs[replication == repl & problem == prob & learner == lrn & variant == myvariant , ]
            vals = vals[, mean(naive.hout.domHV), by = c("evals")]

            RS = mean(dfr[replication == repl & problem == prob & learner == lrn & variant == "RS" & evals == 3095, ]$naive.hout.domHV, na.rm = TRUE)
            RSI = mean(dfr[replication == repl & problem == prob & learner == lrn & variant == "RSI" & evals == 3095, ]$naive.hout.domHV, na.rm = TRUE)
            RSIF = mean(dfr[replication == repl & problem == prob & learner == lrn & variant == "RSIF" & evals == 3095, ]$naive.hout.domHV, na.rm = TRUE)
     
            res2[replication == repl & replication == repl & replication == repl & learner == lrn & problem == prob & variant == myvariant, ]$RS.beat = vals[V1 >= RS, ][1, ]$evals
            res2[replication == repl & replication == repl & learner == lrn & problem == prob & variant == myvariant, ]$RSI.beat = vals[V1 >= RSI, ][1, ]$evals
            res2[replication == repl & learner == lrn & problem == prob & variant == myvariant, ]$RSIF.beat = vals[V1 >= RSIF, ][1, ]$evals
          }
        }
      }      
    }

    saveRDS(res2, file.path(path, "beat_randomsearch_complete.rds"))
  
    
    res3 = res2[, .(RS.beat = mean(RS.beat, na.rm = TRUE),
                    RS.sd = sd(RS.beat, na.rm = TRUE) / sqrt(length(RS.beat)), 
                    RSI.beat = mean(RSI.beat, na.rm = TRUE), 
                    RSI.sd = sd(RSI.beat, na.rm = TRUE) / sqrt(length(RS.beat)),                     
                    RSIF.beat = mean(RSIF.beat, na.rm = TRUE),
                    RSIF.sd = sd(RSIF.beat, na.rm = TRUE)  / sqrt(length(RS.beat)),
                    test = length(RS.beat)), by = c("variant")]
    
    res3$RS.beat = paste(round(res3$RS.beat, 2), " (", round(res3$RS.sd, 2), ")", sep = "")
    res3$RSI.beat = paste(round(res3$RSI.beat, 2), " (", round(res3$RSI.sd, 2), ")", sep = "")
    res3$RSIF.beat = paste(round(res3$RSIF.beat, 2), " (", round(res3$RSIF.sd, 2), ")", sep = "")

    print(xtable(res3[, c("variant", "RS.beat", "RSI.beat", "RSIF.beat")], type = "latex", include.rownames=FALSE), file = paste("latex_temp/beatRS.tex", sep = ""))
}