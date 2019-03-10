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


# datapath = "../data"
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
#       write.arff(train.data, file = file.path(file, "train.arff"))
#       write.arff(test.data, file = file.path(file, "test.arff"))
#   }
#   if (file.exists(file.path(file, "train.arff"))) {
#       bla = read.arff(file.path(file, "train.arff"))
#       names(bla)
#   }
# }
