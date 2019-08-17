library(batchtools)
library(dplyr)
library(mlr)
library(mlrCPO)

source("helpers.R")
source("probdesign.R")

# --- 1. Load Registry and Metadata
reg = loadRegistry("registry", writeable = TRUE)
tab = summarizeExperiments(
	by = c("job.id", "algorithm", "problem", "learner", "maxeval", "cv.iters", "filter", "initialization", 
	"lambda", "mu", "parent.sel", "chw.bitflip", "adaptive.filter.weights", "filter.during.run", "surrogate", "infill",
	"propose.points", "multi.objective", "tune.hyperparams", "tune.iters"))

# path to store 
path = "results_raw"
dir.create(path)


# --- 2. Experiments to be reduced 

# a) Algorithm versions
experiments = list(
	# O = data.table(algorithm = "mosmafs", filter = "none", initialization = "none", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, multi.objective = NA, parent.sel = "selTournamentMO", tune.hyperparams = NA),
	# OI = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, multi.objective = NA, parent.sel = "selTournamentMO", tune.hyperparams = NA),
	# OIFi = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, multi.objective = NA, parent.sel = "selTournamentMO", tune.hyperparams = NA),
	# OIFiFm = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = TRUE, multi.objective = NA, parent.sel = "selTournamentMO", tune.hyperparams = NA),
	# OIFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = NA, parent.sel = "selTournamentMO", tune.hyperparams = NA),
	# OIH = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, multi.objective = NA, parent.sel = "selTournamentMO", tune.hyperparams = NA),
	# OIHFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = NA, parent.sel = "selTournamentMO", tune.hyperparams = NA),
	# RS = data.table(algorithm = "randomsearch", initialization = "none", filter = "none", chw.bitflip = NA, adaptive.filter.weights = NA, filter.during.run = NA),
	# RSI = data.table(algorithm = "randomsearch", initialization = "unif", filter = "none", chw.bitflip = NA, adaptive.filter.weights = NA, filter.during.run = NA),
	# RSIF = data.table(algorithm = "randomsearch", initialization = "unif", filter = "custom", chw.bitflip = NA, adaptive.filter.weights = NA, filter.during.run = NA),
	BS1RF = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = FALSE, surrogate = "randomForest", infill = "cb", propose.points = 15L),
	BS2RF = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = TRUE, surrogate = "randomForest", infill = "cb", propose.points = 15L),
	BS5SO = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = FALSE, parent.sel = "selTournament"),
	BSMO = data.table(algorithm = "mbo_multicrit", filter = "custom", surrogate = "randomForest", infill = "cb", propose.points = 15L),
	OIHFiFmS_no_hyperpars = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = FALSE, tune.iters = NA),
	OIHFiFmS_no_hyperpars500 = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = FALSE, tune.iters = 500)
	)

# b) problems to reduce 
problems = c("wdbc", "ionosphere", "sonar", "hill-valley", "clean1", 
	"tecator", "semeion", "lsvt", "isolet", "cnae-9")

collectBenchmarkResults(path, experiments, tab)
collectParetofront(path, experiments = experiments[c("O", "OIHFiFmS", "RS", "RSI", "RSIF")], tab, problems, learners = c("xgboost"))

# Collect MBO Baselines BSMO, BS1RF, BS2RF
collectBenchmarkResults(path, experiments, tab, mbo = TRUE)

# Just get the hyperparameters of MBO 
toreduce = ijoin(tab, findDone())
toreduce = ijoin(toreduce, experiments[["OIHFiFmS_no_hyperpars"]])
res = reduceResultsDataTable(toreduce, function(x) getHyperparamsPerProblem(x, 500) )
res = ijoin(tab, res)

for (prob in problems) {
	res_reduced = res[problem == prob, ]
	res_reduced = res_reduced[, replication := 1:length(job.id), by = c("learner")]
	hyperparams = lapply(1:10, function(x) {
		a = res_reduced[replication == x, ]
		if (nrow(a) == 2)
			a = rbind(a, res_reduced[replication == x - 5 & learner == "xgboost", ])

		z = lapply(a$learner, function(x) {
			a[learner == x, ]$result[[1]]
		})
		names(z) = a$learner
		z
	})
	saveRDS(hyperparams, file.path("data", prob, "hyperparams_500.rds"))
}



# Reduce single results for MBO and for O
toreduce = tab[problem == "lsvt" & learner == "kknn", ]
toreduce = toreduce[algorithm %in% c("mbo_multicrit", "mosmafs"), ]
toreduce = toreduce[algorithm %in% "mbo_multicrit" 
| (is.na(tune.hyperparams) & filter == "custom" & adaptive.filter.weights & filter.during.run & chw.bitflip & initialization == "unif" & parent.sel == "selTournamentMO") 
| (!tune.hyperparams & is.na(tune.iters)), ]
toreduce = ijoin(toreduce, findDone())
toreduce = toreduce[c(1:3, 11:13, 21:23), ]

res = reduceResultsDataTable(toreduce)
saveRDS(res, file.path(path, "single_experiments_lsvt_kknn.rds"))





 fitnesses <- lapply(
 	mosmafs::getPopulations(res_mosmafs[[1]]$result$log),
 	function(x) x$fitness)

min(which(sapply(fitnesses, function(x) sum(nondominated(x))) > 65)) * 15 + 80

 for (gen in seq_along(fitnesses)[-1]) {
 	first <- fitnesses[[gen - 1]]
 	second <- fitnesses[[gen]]
 	print(min(which(nondominated(cbind(first, second - 1e-7)))))
 }

 mosmafs_coll <- collectResult(res_mosmafs[[1]]$result)

 head(mosmafs_coll)


 mbo_coll <- collectResultMBO(res_mbo[[1]])

 nrow(mbo_coll)




p <- environment(res_mbo[[1]]$result$final.opt.state$opt.problem$fun)$p
filtermat <- environment(
  res_mbo[[1]]$result$final.opt.state$opt.problem$fun)$filtermat


rx2 <- sapply(1:4010, function(cline) {

rdline <- as.list(resdf[cline, ])

arglist <- as.list(rdline[c("k", "distance", "kernel")])
arglist$kernel <- as.character(arglist$kernel)

sel.sel <- 1:p %in% order(filtermat[, as.character(rdline$filter),
	drop = FALSE], decreasing = TRUE)[1:ceiling(rdline$perc * p)]

arglist$selector.selection = sel.sel

res_mosmafs[[1]]$result$task$fitness.fun(arglist, holdout = FALSE)[1]
  rdline$y_1


	res_mosmafs[[1]]$result$task$fitness.fun(arglist, holdout = TRUE)[1]
	rdline$fitness.holdout.perf

}
)

rx

rx2