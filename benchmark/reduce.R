library(batchtools)
library(dplyr)

# load registry
reg = loadRegistry("registry")
tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "initialization", "mu", "lambda", "maxeval", "filter", "learner", "parent.sel", "propose.points"))

toreduce = ijoin(tab, findDone())[algorithm == "randomsearch" & filter %in% c("none", "custom") & maxeval == 4000L, ]
toreduce %>% group_by(learner, problem) %>% summarize(length(algorithm))


# reduce pareto front test set
# res = reduceResultsDataTable(toreduce, function(x) do.call("rbind", x$pareto.front.test))
# res = ijoin(tab, res, by = "job.id")
# saveRDS(res, "results/reduced_results/paretotest_init.rds")

# reduce pareto front validation set
res = reduceResultsDataTable(toreduce, function(x) do.call("rbind", lapply(1:length(x$domhypervol), function(i) cbind(i, t(getPopulations(x$results$log)[i][[1]]$fitness[, x$paretofront[[i]]])))))
res = ijoin(tab, res, by = "job.id")
saveRDS(res, "results/reduced_results/pareto_all_init.rds")


# CollectResults

collectBenchmarkResults = function(savepath, algo) {
	# analyze
	tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "initialization", "mu", "lambda", "maxeval", "filter", "learner", "parent.sel", "propose.points"))
	toreduce = ijoin(tab, findDone())[algorithm == algo & filter %in% c("none", "custom") & maxeval == 4000L, ]
	toreduce = toreduce[1:10, ]
	toreduce %>% group_by(learner, problem) %>% summarize(length(algorithm))

	# runtime
	runtime = reduceResultsDataTable(toreduce, function(x) x$runtime[[3]])
	res = ijoin(tab, runtime, by = "job.id")
	res$result = unlist(res$result)
	res$runtime.min = res$result / 60

	# Extract hypervolume
	hypervol = reduceResultsDataTable(toreduce, function(x) unlist(x$domhypervol))
	names(hypervol)[2] = "domhypervol"
	res = ijoin(res, hypervol, by = "job.id")

	# Extract results on pareto front
	paretofront = reduceResultsDataTable(toreduce, function(x) x$paretofront)
	names(paretofront)[2] = "pareto.train"
	res = ijoin(res, paretofront, by = "job.id")

	fitnesses = reduceResultsDataTable(toreduce, function(x) fitnesses(x$result))
	res = ijoin(res, fitnesses, by = "job.id")


}

