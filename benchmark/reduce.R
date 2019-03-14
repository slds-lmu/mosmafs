library(batchtools)
library(dplyr)

# load registry
reg = loadRegistry("registry")
tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "initialization", "maxeval", "parent.sel", "feature.mut", 
	"filter", "learner", "surrogate", "lambda", "mu", "propose.points"))

path = "results_raw"
algo = "mosmafs"

savepath = paste(path, algo, sep = "/")
dir.create(savepath)

collectBenchmarkResults(savepath, algo)

toextract = c("eval.domHV", "evals")

experiments = list(O = list(algo = "mosmafs", initialization = "none", filter = "none", feature.mut = "mutBitflip"),
	OI = list(algo = "mosmafs", initialization = "unif", filter = "none", feature.mut = "mutBitflip"),
	OIF = list(algo = "mosmafs", initialization = "unif", filter = "custom", feature.mut = "mutBitflip"),
	MBObaseline = list(algo = "MBObaseline", initialization = NA, filter = "custom", feature.mut = NA))

createReport(path, experiments, toextract = toextract, plot.by.colour = "initialization", plot.by.lty = "filter")

experiments = list(OIF = list(algo = "mosmafs", initialization = "unif", filter = "custom", feature.mut = "mutBitflip"),
	OIFH = list(algo = "mosmafs", initialization = "unif", filter = "custom", feature.mut = "mutBitflipCHW"))

createReport(path, experiments, toextract = toextract, plot.by.colour = "feature.mut")




# CollectResults
collectBenchmarkResults = function(savepath, algo, maxevals = 4000L) {
	
	# analyze status
	tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "initialization", "maxeval", "parent.sel", "feature.mut", 
		"filter", "learner", "surrogate", "lambda", "mu", "propose.points"))
	toreduce = ijoin(tab, findDone())[algorithm == algo & filter %in% c("none", "custom") & maxeval == maxevals, ]
	status = toreduce %>% group_by(learner, problem, filter, initialization) %>% summarize(length(algorithm))

	if (algo == "MBObaseline")
		summary = reduceResultsDataTable(toreduce, function(x) collectResultMBO(x))
	else 
		summary = reduceResultsDataTable(toreduce, function(x) collectResult(x$result))
	
	names(summary)[2] = "summary"
	res = ijoin(tab, summary, by = "job.id")
	
	saveRDS(res, file.path(savepath, "result.rds"))
	write.csv(status, file.path(savepath, "status.csv"))
	return(status)
}


collectResultMBO = function(x) {
  mbo.object = x$result
  fitnesses = as.data.frame(mbo.object$opt.path)
  fitnesses$gen = fitnesses$dob
  fitnesses = setDT(fitnesses)
  fitnesses$y_2 = fitnesses$mosmafs.nselect
  fitnesses$runtime = cumsum(fitnesses$exec.time)
  fitnesses$evals = 1:nrow(fitnesses)
  fitnesses$eval.domHV = unlist(x$domhypervol)
  fitnesses$hout.domHV = sapply(1:nrow(fitnesses), function(i) computeHV(t(as.matrix(fitnesses[1:i, c( "fitness.holdout.perf", "fitness.holdout.propfeat")])), ref.point = c(1, 1)))

  stats = fitnesses[, .(runtime = max(runtime), evals = max(evals), eval.perf.min = min(y_1), eval.perf.mean = mean(y_1), eval.perf.max = max(y_1),
  	eval.propfeat.min = min(y_2), eval.propfeat.mean = mean(y_2), eval.propfeat.max = max(y_2),
  	hout.perf.min = min(fitness.holdout.perf), hout.perf.mean = mean(fitness.holdout.perf), hout.perf.max = max(fitness.holdout.perf),
  	hout.propfeat.min = min(fitness.holdout.propfeat), hout.propfeat.mean = mean(fitness.holdout.propfeat), hout.propfeat.max = max(fitness.holdout.propfeat),
  	eval.domHV = max(eval.domHV), hout.domHV = max(hout.domHV)), by = .(gen)]

  stats$runtime.min = stats$runtime / 60

  return(stats)
  
}


createReport = function(path, experiments, toextract, plot.by.colour, plot.by.lty = NULL) {
	dir.create(paste("results_plots/", toextract[1], sep = ""))

	dflist = lapply(experiments, function(x) readRDS(file.path(path, x$algo, "result.rds"))[initialization == x$initialization & filter == x$filter & feature.mut == x$feature.mut, ])
	if ("MBObaseline" %in% names(dflist)) {
		dflist$MBObaseline = readRDS(file.path(path, "MBObaseline", "result.rds"))
	}

	dflist = lapply(dflist, function(x) extractFromSummary(x, toextract))
	df = do.call("rbind", dflist)

	# vizualise hypervolume per evaluation	
	df = df[, .(mean.domHV = mean(domHV)), by = c("algorithm", "evals", "problem", "initialization", "filter", "learner", "parent.sel", "feature.mut")]

	p = ggplot(data = df, aes_string(x = "evals", y = "mean.domHV", colour = plot.by.colour, lty = plot.by.lty))
	p = p + geom_line()
	p = p + facet_grid(learner ~ problem)
	ggsave(paste("results_plots/", toextract[1], "/", paste(names(experiments), collapse = "_"), ".png", sep = ""), p, width = 10)

	# viz final pareto front on training


	# viz final pareto front on test set


	# create boxplot of final results 	


	# lineplots 


	# create ranks

}

extractFromSummary = function(res, toextract) {
	cols = ncol(res)
	hypervol = lapply(1:nrow(res), function(i) cbind(res[i, ]$job.id, setDT(res[i, ]$summary[[1]])[, ..toextract]))
	hypervol = as.data.table(do.call("rbind", hypervol))
	names(hypervol) = c("job.id", "domHV", "evals")
	df = ijoin(res[, 1:(cols - 1)], hypervol, by = "job.id")
	return(df)
}