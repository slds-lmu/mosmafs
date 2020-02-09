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
	"infill", "propose.points", "tune.hyperparams", "tune.iters", "multi.objective", "start.recon.iter", "ensemble", "multiobjective")
	)

resources.serial = list(
	walltime = 3600L * 96L, memory = 1024L * 4L,
	clusters = "serial", max.concurrent.jobs = 1000L # get name from lrz homepage)
)

resources.serial.doublemem = list(
	walltime = 3600L * 96L, memory = 1024L * 8L,
	clusters = "serial", max.concurrent.jobs = 250L # get name from lrz homepage)
)

resources.mpp2 = list(ncpus = 15L,
	walltime = 3600L * 48L, memory = 1024L * 4L,
	clusters = "mpp2") # get name from lrz homepage))



# --- 2. Algorithms that will be run 

experiments = list(
	# RQ 1: Proposed variants - GA vs BO
	GAMOFE = data.table(algorithm = "mosmafs", filter = "custom", initialization = "geom", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	BOMOFE = data.table(algorithm = "mbo_multicrit", filter = "custom", surrogate = "randomForest", infill = "cb", propose.points = 15L, adaptive.filter.weights = TRUE),
	# RQ 2: Proposed variants without filter ensemble
	BOMO = data.table(algorithm = "mbo_multicrit", filter = "custom", surrogate = "randomForest", infill = "cb", propose.points = 15L, adaptive.filter.weights = FALSE),
	GAMO = data.table(algorithm = "mosmafs", filter = "custom", initialization = "geom", chw.bitflip = TRUE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	# RQ 3: proposed variants as single objective
	BOSO = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = TRUE, ensemble = FALSE, surrogate = "randomForest", infill = "cb", propose.points = 15L),# start.recon.iter = 80L),
	BOSOFE = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = TRUE, ensemble = TRUE, surrogate = "randomForest", infill = "cb", propose.points = 15L, start.recon.iter = 80L),	
	GASOFE = data.table(algorithm = "mosmafs", filter = "custom", initialization = "geom", chw.bitflip = TRUE, adaptive.filter.weights = FALSE, filter.during.run = TRUE, multi.objective = FALSE, parent.sel = "selTournament", tune.hyperparams = TRUE, tune.iters = 0L),
	# RQ 4: proposed variants vs. non-joint correspondents 
	BOSOFENJ = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = TRUE, ensemble = TRUE, surrogate = "randomForest", infill = "cb", propose.points = 15L, start.recon.iter = 80L),
	BOSOFJ = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = TRUE, ensemble = FALSE, surrogate = "randomForest", infill = "cb", propose.points = 15L),# start.recon.iter = 80L),
	GASOFENJ = data.table(algorithm = "mosmafs", filter = "custom", initialization = "geom", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = FALSE, tune.iters = 500)
	)


ablation_experiments = list(
	NSGA2 = data.table(algorithm = "mosmafs", filter = "none", initialization = "none", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	NSGA2_unif = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	NSGA2_geom = data.table(algorithm = "mosmafs", filter = "none", initialization = "geom", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	NSGA2_geom_HWP = data.table(algorithm = "mosmafs", filter = "none", initialization = "geom", chw.bitflip = TRUE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	GAMO = data.table(algorithm = "mosmafs", filter = "custom", initialization = "geom", chw.bitflip = TRUE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	GAMOFE = data.table(algorithm = "mosmafs", filter = "custom", initialization = "geom", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE)
	)


randomsearch_experiments = list(
	RS = data.table(algorithm = "randomsearch", initialization = "none", filter = "none"),
	RSI = data.table(algorithm = "randomsearch", initialization = "unif", filter = "none"),
	RSG = data.table(algorithm = "randomsearch", initialization = "geom", filter = "none"),
	RSIF = data.table(algorithm = "randomsearch", initialization = "unif", filter = "custom"),
	RSGF = data.table(algorithm = "randomsearch", initialization = "geom", filter = "custom")
	)



# --- 3. Datasets that will be run 

problems.serial = datasets
toprint = tab[problem %in% problems.serial, ]

printState(toprint, experiments, ids = findDone())
printState(toprint, experiments, ids = findQueued())
printState(toprint, experiments, ids = findExpired())
printState(toprint, experiments, ids = findRunning())
printState(toprint, experiments, ids = findErrors())


# --- 4. Datasets that will be run 

for (exp in names(experiments)) {
	tosubmit = ijoin(tab, experiments[[exp]], by = names(experiments[[exp]]))
	tosubmit = ijoin(tosubmit, findNotDone())
	tosubmit = tosubmit[problem %in% problems.serial, ]
	submitJobs(tosubmit, resources = resources.serial)
}
