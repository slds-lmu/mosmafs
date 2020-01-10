library(batchtools)
library(dplyr)
library(mlr)
library(mlrCPO)

source("helpers.R")
source("probdesign.R")

# --- 1. Load Registry and Metadata
reg = loadRegistry("registry", writeable = FALSE)
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
	# BS1RF = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = FALSE, surrogate = "randomForest", infill = "cb", propose.points = 15L),
	# BS2RF = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = TRUE, surrogate = "randomForest", infill = "cb", propose.points = 15L),
	# BS5SO = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = FALSE, parent.sel = "selTournament"),
	# BSMO = data.table(algorithm = "mbo_multicrit", filter = "custom", surrogate = "randomForest", infill = "cb", propose.points = 15L, adaptive.filter.weights = NA)#,
	# OIHFiFmS_no_hyperpars = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = FALSE, tune.iters = NA),
	# OIHFiFmS_no_hyperpars500 = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = FALSE, tune.iters = 500),
	BSMOF_old = data.table(algorithm = "mbo_multicrit", filter = "custom", surrogate = "randomForest", infill = "cb", propose.points = 15L, adaptive.filter.weights = TRUE, multi.objective = NA),
	BSMOF_fixed = data.table(algorithm = "mbo_multicrit", filter = "custom", surrogate = "randomForest", infill = "cb", propose.points = 15L, adaptive.filter.weights = TRUE, multi.objective = TRUE),
	# OIHFiFmS_preset500 = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 500)
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
