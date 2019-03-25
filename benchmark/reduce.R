library(batchtools)
library(dplyr)

# load registry
reg = loadRegistry("registry")
tab = summarizeExperiments(by = c("job.id", "algorithm", 
	"problem", "learner", "maxeval", "filter", "initialization", 
	"lambda", "mu", "parent.sel", "chw.bitflip", "adaptive.filter.weights",
	"filter.during.run", "surrogate", "MBMOmethod", "propose.points"))

path = "results_raw"

experiments = list(
	O = data.table(algorithm = "mosmafs", filter = "none", initialization = "none", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OI = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OIFi = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OIFiFm = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = TRUE),
	OIFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = TRUE, filter.during.run = TRUE),
	OIH = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OIHFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE),
	RS = data.table(algorithm = "randomsearch", initialization = "none", filter = "none"),
	RSI = data.table(algorithm = "randomsearch", initialization = "unif", filter = "none"),
	RSIF = data.table(algorithm = "randomsearch", initialization = "unif", filter = "custom")
	)

collectBenchmarkResults(path, experiments)






# collectResultMBO = function(x) {
#   mbo.object = x$result
#   fitnesses = as.data.frame(mbo.object$opt.path)
#   fitnesses$gen = fitnesses$dob
#   fitnesses = setDT(fitnesses)
#   fitnesses$y_2 = fitnesses$fitness.holdout.propfeat
#   fitnesses$runtime = cumsum(fitnesses$exec.time)
#   fitnesses$evals = 1:nrow(fitnesses)
#   fitnesses$eval.domHV = sapply(1:nrow(fitnesses), function(i) computeHV(t(as.matrix(fitnesses[1:i, c( "y_1", "y_2")])), ref.point = c(1, 1)))
#   fitnesses$hout.domHV = sapply(1:nrow(fitnesses), function(i) computeHV(t(as.matrix(fitnesses[1:i, c( "fitness.holdout.perf", "fitness.holdout.propfeat")])), ref.point = c(1, 1)))

#   stats = fitnesses[, .(runtime = max(runtime), evals = max(evals), eval.perf.min = min(y_1), eval.perf.mean = mean(y_1), eval.perf.max = max(y_1),
#   	eval.propfeat.min = min(y_2), eval.propfeat.mean = mean(y_2), eval.propfeat.max = max(y_2),
#   	hout.perf.min = min(fitness.holdout.perf), hout.perf.mean = mean(fitness.holdout.perf), hout.perf.max = max(fitness.holdout.perf),
#   	hout.propfeat.min = min(fitness.holdout.propfeat), hout.propfeat.mean = mean(fitness.holdout.propfeat), hout.propfeat.max = max(fitness.holdout.propfeat),
#   	eval.domHV = max(eval.domHV), hout.domHV = max(hout.domHV)), by = .(gen)]

#   stats$runtime.min = stats$runtime / 60

#   return(stats)
  
# }


createReport = function(path, experiments, toextract, plot.by.colour, plot.by.lty = NULL) {
	dir.create(paste("results_plots/", toextract[1], sep = ""))

	dflist = lapply(experiments, function(x) readRDS(file.path(path, x$algo, "result.rds"))[initialization == x$initialization & filter == x$filter & chw.bitflip == x$chw.bitflip, ])
	if ("MBObaseline" %in% names(dflist)) {
		dflist$MBObaseline = readRDS(file.path(path, "MBObaseline", "result.rds"))
	}

	dflist = lapply(dflist, function(x) extractFromSummary(x, toextract))
	df = do.call("rbind", dflist)
	df = df[- which(problem %in% c("hypersphere.200.50", "hypersphere.200.200")), ]

	# vizualise hypervolume per evaluation	
	dfp = dfp[, .(mean.domHV = mean(eval.domHV)), by = c("algorithm", "evals", "problem", "initialization", "filter", "learner", "parent.sel", "chw.bitflip")]
	p = ggplot()
	p = p + geom_line(data = dfp[algorithm == "mosmafs", ], aes_string(x = "evals", y = "mean.domHV", colour = plot.by.colour, lty = plot.by.lty))
	p = p + geom_line(data = dfp[algorithm == "MBObaseline", ], aes_string(x = "evals", y = "mean.domHV"), colour = "black")
	p = p + facet_grid(learner ~ problem) + theme_bw()
	ggsave(paste("results_plots/", toextract[1], "/", paste(names(experiments), collapse = "_"), ".png", sep = ""), p, width = 15)

	for (lrn in unique(df$learner)) {
		for (prob in unique(df$problem)[c(1:6, 8:10)]) {
				dir.create(paste("results_plots/", toextract[1], "/", learner, "_", prob, sep = ""))
				dfp = df[problem == prob & learner == lrn, ]
				dfp = dfp[- which(initialization == "none"), ]
				dfp = dfp[- which(!chw.bitflip), ]
				dfp$job.id = as.factor(dfp$job.id)

				p = ggplot()
				p = p + geom_line(data = dfp, aes_string(x = "evals", y = "eval.domHV", colour = "job.id"))
				p = p + facet_grid( ~ algorithm) + theme_bw()
				ggsave(paste("results_plots/", toextract[1], "/", learner, "_", prob, "/", "plot.png", sep = ""), p, width = 15)

		}
	}

	dflist = lapply(experiments, function(x) readRDS(file.path(path, x$algo, "result.rds"))[initialization == x$initialization & filter == x$filter & chw.bitflip == x$chw.bitflip, ])
	if ("MBObaseline" %in% names(dflist)) {
		dflist$MBObaseline = readRDS(file.path(path, "MBObaseline", "result.rds"))
	}

	dflist = lapply(dflist, function(x) extractFromSummary(x, c("evals", "eval.perf.min", "eval.perf.mean", "eval.perf.max")))
	df = do.call("rbind", dflist)
	df = df[- which(problem %in% c("hypersphere.200.50", "hypersphere.200.200")), ]


	# viz final pareto front on training
	for (lrn in unique(df$learner)) {
		for (prob in unique(df$problem)[c(1:6, 8:10)]) {
				dir.create(paste("results_plots/", "front", "/", learner, "_", prob, sep = ""))
				dfp = df[problem == prob & learner == lrn, ]
				dfp = dfp[- which(initialization == "none"), ]
				dfp = dfp[- which(!chw.bitflip), ]
				dfp$job.id = as.factor(dfp$job.id)
				dfp = dfp[, .(eval.perf.min = mean(eval.perf.min), eval.perf.mean = mean(eval.perf.mean), eval.perf.max = mean(eval.perf.max)), by = c("algorithm", "evals", "problem", "initialization", "filter", "learner", "parent.sel", "chw.bitflip")]

				p = ggplot()
				p = p + geom_line(data = dfp, aes_string(x = "evals", y = "eval.domHV", colour = "job.id"))
				p = p + facet_grid( ~ algorithm) + theme_bw()
				ggsave(paste("results_plots/", toextract[1], "/", learner, "_", prob, "/", "plot.png", sep = ""), p, width = 15)

		}
	}	

	# viz final pareto front on test set


	# create boxplot of final results 	


	# lineplots 


	# create ranks

}

