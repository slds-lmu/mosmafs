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

    res = reduceResultsDataTable(toreduce, function(x) collectResult(x$result))
    res = ijoin(tab, res, by = "job.id")
    res$variant = experiment

    dir.create(file.path(path, experiment))

    saveRDS(res, file.path(path, experiment, "result.rds"))
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
               legend.key.size= unit(0.2, "cm"),
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

plotOptPathFacetGrid = function(res, plotspath) {
  df = extractFromSummary(res, c("evals", "eval.domHV"))
  df = df[- which(problem %in% c("hypersphere.200.50", "hypersphere.200.200")), ] 
  dfp = df[, .(mean.domHV = mean(eval.domHV)), by = c("algorithm", "evals", "problem", "learner", "variant")]
  dfp = dfp[evals != 8000, ]
    p = ggplot()
    p = p + geom_line(data = dfp[algorithm == "mosmafs", ], aes(x = evals, y = mean.domHV, colour = variant))
  p = p + geom_hline(data = dfp[algorithm == "randomsearch", ], aes(yintercept = mean.domHV, colour = variant))
  p = p + facet_grid(learner ~ problem) + theme_bw()
  p = p + scale_colour_Publication() + theme_Publication()

  ggsave(file.path(plotspath, "eval.domHV", "all.png"))

  for (prob in unique(dfp$problem)) {
    for (lrn in unique(dfp$learner)) {
      p = ggplot()
        p = p + geom_line(data = dfp[algorithm == "mosmafs" & learner == lrn & problem == prob, ], aes(x = evals, y = mean.domHV, colour = variant))
      p = p + geom_hline(data = dfp[algorithm == "randomsearch" & learner == lrn & problem == prob, ], aes(yintercept = mean.domHV, colour = variant))
      p = p + theme_Publication()
      p = p + scale_colour_Publication()
      ggsave(file.path(plotspath, "eval.domHV", paste(lrn, prob, "png", sep = ".")))
    }
  }

}