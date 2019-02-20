packages = c("BBmisc", "magrittr", "batchtools", "mlr", "data.table")

sapply(packages, require, character.only = TRUE)

source("../datagen.R")

unlink("runtime-experiment", recursive = TRUE)
reg = makeExperimentRegistry(file.dir = "runtime-experiment", seed = 123L,
      packages = packages, source = "../datagen.R")

# Evaluation of running time

problems = CJ(n = c(100, 200, 500, 1000, 2000, 5000, 10000),
            p = c(10, 20, 50, 100, 200, 500, 1000, 5000))

lrns = makeLearners(c("classif.kknn", "classif.ksvm"))
tsks = lapply(1:nrow(problems), function(i) create.hypersphere.data(dim = 2, n = problems[i, ]$n) %>% create.classif.task(id = paste("hypersphere.", i, sep = ""))  %>% task.add.random.cols(num = problems[i, ]$p - 2))   
tsk.ids = sapply(tsks, function(x) x$task.desc$id)
problems = cbind(problems, tsk.ids)
rdesc = makeResampleDesc("CV", iters = 10L)# list(makeResampleDesc("CV", iters = 5L), makeResampleDesc("CV", iters = 10L))
meas = list(timetrain)
batchmark(learners = lrns, tasks = tsks, resamplings = rdesc, measures = meas, models = FALSE, reg = reg)

tab = summarizeExperiments()
submitJobs(findNotDone()$job.id, resources = list(
  walltime = 3600 * 5L, memory.limit = 1024L * 2L)
)

ids = findDone()$job.id
tab = batchtools::getJobPars(ids, reg = reg)[, c("job.id", "problem", "algorithm")]
setkeyv(tab, cols = c("problem", "algorithm"), physical = FALSE)
result = namedList(tab[, unique(problem)])

for (prob in names(result)) {
    algos = unique(tab[problem == prob], by = "algorithm")
    data = batchtools::makeJob(id = algos$job.id[1L], reg = reg)$problem$data
    result[[prob]] = namedList(algos$algorithm)

  for (algo in names(result[[prob]])) {
    res = batchtools::reduceResultsList(tab[problem == prob & algorithm == algo], reg = reg)
    models = !is.null(res[[1L]]$model)
    lrn = data$learner[[algo]]
    extract.this = mlr:::getExtractor(lrn)
    rs = mlr:::mergeResampleResult(learner.id = algo, task = data$task, iter.results = res, measures = data$measures,
      rin = data$rin, keep.pred = FALSE, models = models, show.info = TRUE, runtime = NA, extract = extract.this)
    rs$learner = lrn
    result[[prob]][[algo]] = addClasses(rs, "ResampleResult")
  }
}

res = sapply(result, function(x) data.frame(kknn = as.numeric(x$classif.kknn$aggr), ksvm = ifelse(is.null(x$classif.ksvm), NA, as.numeric(x$classif.ksvm$aggr))))
res = as.data.frame(t(res))
res$tsk.ids = rownames(res)

res = ijoin(res, problems, by = "tsk.ids")

saveRDS(res, "timetrain.rds")


# some vizualisation

library(ggplot2)
library(reshape2)

timetrain = readRDS("test-benchmarks/timetrain.rds")
timetrain$kknn = unlist(timetrain$kknn)
timetrain$ksvm = unlist(timetrain$ksvm)
df = melt(timetrain, id = c("n", "p", "tsk.ids"))
p = ggplot(data = df, aes(x = n, y = value, lty = as.factor(p), colour = variable))
p = p + geom_line() + geom_point()
p = p + ylim(c(0, 50))

# ionosphere: n = 351, p = 34
# training time per fold: 0.04 secs
# training time for 10-folds: 0.4 secs
# allow 1000 experiments: 6 minutes
# allow 2000 experiments: 12 minutes per experiment
