library(batchtools)
library(dplyr)

# load registry
reg = loadRegistry("registry")
tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "initialization", "maxeval", "parent.sel", "feature.mut", "filter", "learner", "surrogate", "lambda", "mu", "propose.points"))

path = "baseline-results"
algo = "randomsearch"

savepath = paste(path, algo, sep = "/")
dir.create(savepath)

collectBenchmarkResults(savepath, algo)


# CollectResults
collectBenchmarkResults = function(savepath, algo, maxevals = 4000L) {
	
	# analyze status
	tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "initialization", "mu", "lambda", "maxeval", "filter", "learner", "parent.sel", "propose.points"))
	toreduce = ijoin(tab, findDone())[algorithm == algo & filter %in% c("none", "custom") & maxeval == maxevals, ]
	status = toreduce %>% group_by(learner, problem, filter, initialization) %>% summarize(length(algorithm))

	# runtime
	runtime = reduceResultsDataTable(toreduce, function(x) x$runtime[[3]])
	res = ijoin(tab, runtime, by = "job.id")
	res$result = unlist(res$result)
	res$runtime.min = res$result / 60

	# fitnesses front 
	front = reduceResultsDataTable(toreduce, function(x) do.call("rbind", lapply(1:length(x$domhypervol), function(i) cbind(i, t(getPopulations(x$result$log)[i][[1]]$fitness[, x$paretofront[[i]]])))))
	names(front)[2] = "front.train"
	res = ijoin(res, front, by = "job.id")

	# fitnesses hout
	front.hout = reduceResultsDataTable(toreduce, function(x) popAggregate(x$result$log, "fitness.holdout"))
	names(front.hout)[2] = "front.hout"
	res = ijoin(res, front.hout, by = "job.id")

	summary = reduceResultsDataTable(toreduce, function(x) collectResult(x$result))
	names(summary)[2] = "summary"
	res = ijoin(res, summary, by = "job.id")
	
	saveRDS(res, file.path(savepath, "result.rds"))
	write.csv(status, file.path(savepath, "status.csv"))
	return(status)
}

