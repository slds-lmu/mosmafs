#!/usr/bin/env -S Rscript --max-ppsize=100000
args = commandArgs(trailingOnly=TRUE)
if (length(args) < 3) {
  stop("Usage: randomsearch.R <data.rds> <number evals> <outfile>")
}
suppressMessages({
  library("mlr")
  library("ecr")
  library("mlrCPO")
  library("magrittr")
  library("BBmisc")
  library("checkmate")
  library("mosmafs")
})

lrn <- makeLearner("classif.ksvm", predict.type = "prob")
lrn.ps <- pSS(
  C: numeric[10^(-3), 10^3], # according to Fröhlich et al.
  sigma: numeric[10^(-3), 10^3]
)

# tasks <- dir("/home/user/development/R/mosmafs/data", "*.rds", full.names = TRUE)

# chosen <- grep("SPLIT_ionosphere", tasks, value = TRUE)

chosen <- args[1]

outfile <- args[3]
assertDirectory(dirname(outfile))
assertTRUE(dirname(outfile) != outfile)
stopifnot(!file.exists(outfile))

numrounds <- as.integer(args[2])
assertInt(numrounds, lower = 1)

data <- readRDS(chosen)
assertClass(data$task, "Task")
assertClass(data$hout, "Task")

task <- data$task

ps <- c(lrn.ps,
  pSS(selector.selection: logical^getTaskNFeats(task)))


objective <- makeObjective(lrn, task, ps, cv10, holdout.data = data$hout)

initials <- sampleValues(ps, numrounds, discrete.names = TRUE) %>%
  initSelector(function() (getTaskNFeats(task) - 1))



results <- lapply(initials, function(indiv) {
  list(
    individual = indiv,
    eval.time = system.time(result <- objective(indiv), gcFirst = FALSE),
    result = result,
    holdout = objective(indiv, holdout = TRUE))
})

saveRDS(results, outfile)

