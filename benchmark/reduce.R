library(batchtools)

source("def.R")

# load registry
reg = loadRegistry("registry")
tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "initialization", "mu", "lambda", "maxeval", "filter.method", "learner", "parent.sel"))

# Extract hypervolume
res = reduceResultsDataTable(findDone(), function(x) unlist(x$domhypervol))
res = ijoin(tab, res, by = "job.id")
saveRDS(res, "results/reduced_results/hypervol_parentsel.rds")

# reduce pareto front test set
res = reduceResultsDataTable(findDone(), function(x) do.call("rbind", x$pareto.front.test))
res = ijoin(tab, res, by = "job.id")
saveRDS(res, "results/reduced_results/paretotest_parentsel.rds")

# reduce pareto front validation set
res = reduceResultsDataTable(findDone(), function(x) do.call("rbind", lapply(1:length(x$domhypervol), function(i) cbind(i, t(getPopulations(x$results$log)[i][[1]]$fitness[, x$paretofront[[i]]])))))
res = ijoin(tab, res, by = "job.id")
saveRDS(res, "results/reduced_results/pareto_all_parentsel.rds")


runtime = reduceResultsDataTable(findDone(), function(x) x$runtime[[3]])
runtime = ijoin(tab, runtime, by = "job.id")
saveRDS(runtime, "results/runtime_parentsel_10000.rds")



res = reduceResultsDataTable(findDone(), function(x) getIndividualsChromosomes(x$results))
res = ijoin(tab, res, by = "job.id")
res.fitnesses = reduceResultsDataTable(findDone(), function(x) fitnesses(x$results))
res.fitnesses = ijoin(tab, res.fitnesses, by = "job.id")

saveRDS(res, "res_test.rds")
saveRDS(res.fitnesses, "res_fitnesses.rds")

