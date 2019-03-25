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
#       data = data[data$class %in% c("Apple", "Banana"), ]
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



# CollectResults
collectBenchmarkResults = function(path, experiments) {
  
  tab = summarizeExperiments(by = c("job.id", "algorithm", 
    "problem", "learner", "maxeval", "filter", "initialization", 
    "lambda", "mu", "parent.sel", "chw.bitflip", "adaptive.filter.weights",
    "filter.during.run", "surrogate"))

  for (experiment in names(experiments)) {
    toreduce = ijoin(tab, experiments[[experiment]], by = names(experiments[[experiment]]))
    toreduce = ijoin(toreduce, findDone(), by = "job.id")

    dir = as.numeric(sapply(list.files("registry/results/"), function(x) strsplit(x, ".rds")[[1]][1]))
    dir = data.frame(job.id = dir)
    toreduce = ijoin(toreduce, dir)

    # res = reduceResultsDataTable(toreduce, function(x) collectResult(x$result))
    # res = ijoin(tab, res, by = "job.id")
    # res$variant = experiment

    pops = reduceResultsDataTable(toreduce, function(x) lapply(getPopulations(x$result$log), function(x) x$fitness))
    pops = ijoin(tab, pops, by = "job.id")
    pops$variant = experiment

    dir.create(file.path(path, experiment))

    # saveRDS(res, file.path(path, experiment, "result.rds"))
    saveRDS(pops, file.path(path, experiment, "population.rds"))

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
 
  status_finished = df %>% group_by(variant, problem, learner) %>% summarize(num.repls = max(replication)) %>% filter(num.repls == 10)
  df = ijoin(df, status_finished, by = c("problem", "learner", "variant")) 

  dfp = df[, .(mean.domHV = mean(eval.domHV)), by = c("algorithm", "evals", "problem", "learner", "variant")]
  dfp = dfp[evals != 8000, ]

  p = ggplot()
  p = p + geom_line(data = dfp[algorithm == "mosmafs", ], aes(x = evals, y = mean.domHV, colour = variant))
  p = p + geom_hline(data = dfp[algorithm == "randomsearch", ], aes(yintercept = mean.domHV, lty = variant))
  p = p + facet_grid(learner ~ problem) + theme_bw()
  p = p + scale_colour_Publication() + theme_Publication()

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
  df = extractFromSummary(res, c("evals", "eval.domHV"))
    df = df[- which(problem %in% c("hypersphere.200.50", "hypersphere.200.200")), ] 
    df = df[evals != 8000, ]
    df$gen = (df$evals - df$mu) / df$lambda
    df = df[, replication := 1:length(job.id), by = c("learner", "variant", "problem", "gen")]
    df = df[- which(algorithm == "randomsearch"), ]

    # remove the ones where we have not enough replications
    status_finished = df %>% group_by(variant, problem, learner) %>% summarize(num.repls = max(replication)) %>% filter(num.repls == 10)
    status_finished = status_finished %>% group_by(problem, learner) %>% summarize(nvariant = length(variant)) %>% filter(nvariant == 7)
    dfs = ijoin(df, status_finished, by = c("problem", "learner")) 


    # --- calculate ranks within learner, problem and replication ---
    dfr = dfs[, rank_variant := rank(- eval.domHV), by = c("learner", "problem", "gen", "replication")]
    dfr = dfr[, mean(rank_variant), by = c("learner", "problem", "gen", "variant")]

    ranks_overall = dfr[, mean(V1), by = c("gen", "variant")]
    names(ranks_overall) = c("Generation", "Variant", "MeanRank")

    p = ggplot()
    p = p + geom_line(data = ranks_overall, aes(x = Generation, y = MeanRank, colour = Variant))
  p = p + theme_bw()
  p = p + scale_colour_Publication() + theme_Publication()
  ggsave(file.path(plotspath, "ranks.domHV", "ranks.png"), p)

  for (lrn in unique(dfr$learner)) {
    dfr_lrn = dfr[learner == lrn, ]
    ranks_overall = dfr_lrn[, mean(V1), by = c("gen", "variant")]
      names(ranks_overall) = c("Generation", "Variant", "MeanRank")

      p = ggplot()
      p = p + geom_line(data = ranks_overall, aes(x = Generation, y = MeanRank, colour = Variant))
    p = p + theme_bw()
    p = p + scale_colour_Publication() + theme_Publication()
    ggsave(file.path(plotspath, "ranks.domHV", "perLearner", paste(lrn, "ranks.png", sep = ".")), p)
    }


    for (prob in unique(dfr$problem)) {
    dfr_prob = dfr[problem == prob, ]
    ranks_overall = dfr_prob[, mean(V1), by = c("gen", "variant")]
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
      ranks_overall = dfr_prob[, mean(V1), by = c("gen", "variant")]
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
