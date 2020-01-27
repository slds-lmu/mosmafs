# ---
# SUBMISSION SCRIPT 
# ---

library(batchtools)
library(stringi)
library(dplyr)

# --- 1. Load Registry and Metadata
reg = loadRegistry("registry", writeable = TRUE)
reg$work.dir = getwd()

tab = summarizeExperiments(
	by = c("job.id", "algorithm", "problem", "learner", "maxeval", "cv.iters", "filter", "initialization", 
	"lambda", "mu", "parent.sel", "chw.bitflip", "adaptive.filter.weights", "filter.during.run", "surrogate", 
	"infill", "propose.points", "tune.hyperparams", "tune.iters", "multi.objective", "start.recon.iter")
	)
tab = tab[maxeval == 2000L, ]

resources.serial = list(
	walltime = 3600L * 96L, memory = 1024L * 4L,
	clusters = "serial", max.concurrent.jobs = 1000L # get name from lrz homepage)
)

resources.serial.doublemem = list(
	walltime = 3600L * 60L, memory = 1024L * 10L,
	clusters = "serial", max.concurrent.jobs = 250L # get name from lrz homepage)
)

resources.mpp2 = list(ncpus = 15L,
	walltime = 3600L * 48L, memory = 1024L * 4L,
	clusters = "mpp2") # get name from lrz homepage))


# --- 2. Experiments that will be run 

# a) Algorithm versions
experiments = list(
	O = data.table(algorithm = "mosmafs", filter = "none", initialization = "none", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	OG = data.table(algorithm = "mosmafs", filter = "none", initialization = "geom", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	OI = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	OIFi = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	OIFiFm = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	OIFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	OIH = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	OIHFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	OGHFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "geom", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	RS = data.table(algorithm = "randomsearch", initialization = "none", filter = "none"),
	RSI = data.table(algorithm = "randomsearch", initialization = "unif", filter = "none"),
	RSIF = data.table(algorithm = "randomsearch", initialization = "unif", filter = "custom"),
	BS1RF = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = FALSE, surrogate = "randomForest", infill = "cb", propose.points = 15L, start.recon.iter = 80L),
	BS2RF = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = TRUE, surrogate = "randomForest", infill = "cb", propose.points = 15L, start.recon.iter = 80L),
	BS5SO = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = FALSE, parent.sel = "selTournament", tune.hyperparams = TRUE, tune.iters = 0),
	BSMO = data.table(algorithm = "mbo_multicrit", filter = "custom", surrogate = "randomForest", infill = "cb", propose.points = 15L, adaptive.filter.weights = FALSE),
	OIHFiFmS_no_hyperpars = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = FALSE, tune.iters = 0L),
	BSMOF = data.table(algorithm = "mbo_multicrit", filter = "custom", surrogate = "randomForest", infill = "cb", propose.points = 15L, adaptive.filter.weights = TRUE)
	# OIHFiFmS_no_hyperpars500 = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = FALSE, tune.iters = 500),
	# OIHFiFmS_preset500 = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 500)
	)

# b) Datasets

# ru59sol2
problems.serial = c("AP_Breast_Colon")
problems.serial = c("arcene")
problems.serial = c("madeline")
problems.serial = c("AP_Colon_Kidney")
problems.serial = c("madelon")

printState(tab[problem == problems.serial, ], experiments, ids = findDone())
printState(tab[problem == problems.serial, ], experiments, ids = findQueued())
printState(tab[problem == problems.serial, ], experiments, ids = findExpired())
printState(tab[problem == problems.serial, ], experiments, ids = findRunning())


# NOT FULLY SUBMITTED 
for (exp in names(experiments)) {
	tosubmit = ijoin(tab, experiments[[exp]], by = names(experiments[[exp]]))
	tosubmit = ijoin(tosubmit, findNotDone())
	tosubmit = tosubmit[problem %in% problems.serial, ]
	if (exp %in% c("BSMO", "BSMOF")) {
		tosubmit = tosubmit[job.id > 100000, ]
	}

	if (nrow(tosubmit) > 0 && nrow(tosubmit) <= 30L) {
		submitJobs(tosubmit, resources = resources.serial)
	}
}
