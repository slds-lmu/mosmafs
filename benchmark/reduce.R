library(batchtools)
library(dplyr)
library(mlr)
library(mlrCPO)

source("helpers.R")
source("probdesign.R")

# --- 1. Load Registry and Metadata
reg = loadRegistry("registry", writeable = FALSE)
# reg$work.dir = getwd()
tab = summarizeExperiments(
	by = c("job.id", "algorithm", "problem", "learner", "maxeval", "cv.iters", "filter", "initialization", 
	"lambda", "mu", "parent.sel", "chw.bitflip", "adaptive.filter.weights", "filter.during.run", "surrogate", 
	"infill", "propose.points", "tune.hyperparams", "tune.iters", "multiobjective", "multi.objective","start.recon.iter")
	)
# tab = tab[maxeval == 2000L, ]

# path to store 
path = "results_reduced"
# path = "../../../mosmafs/benchmark/results_reduced"
dir.create(path)

# --- 2. Experiments to be reduced 

# a) Algorithm versions

experiments_old = list(
	O = data.table(algorithm = "mosmafs", filter = "none", initialization = "none", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, tune.hyperparams = NA, multi.objective = NA),
	OI = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, tune.hyperparams = NA, multi.objective = NA),
	OIFi = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, tune.hyperparams = NA, multi.objective = NA),
	OIFiFm = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = TRUE, tune.hyperparams = NA, multi.objective = NA),
	OIFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, tune.hyperparams = NA, multi.objective = NA),
	OIH = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, tune.hyperparams = NA, multi.objective = NA),
	OIHFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, tune.hyperparams = NA, multi.objective = NA),
	OG = data.table(algorithm = "mosmafs", filter = "none", initialization = "geom", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),		
	OGHFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "geom", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	RS = data.table(algorithm = "randomsearch", initialization = "none", filter = "none"),
	RSI = data.table(algorithm = "randomsearch", initialization = "unif", filter = "none"),
	RSIF = data.table(algorithm = "randomsearch", initialization = "unif", filter = "custom"),
	BS1RF = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = FALSE, surrogate = "randomForest", infill = "cb", propose.points = 15L),
	BS2RF = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = TRUE, surrogate = "randomForest", infill = "cb", propose.points = 15L),
	BS5SO = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = FALSE, parent.sel = "selTournament"),
	BSMO = data.table(algorithm = "mbo_multicrit", filter = "custom", surrogate = "randomForest", infill = "cb", propose.points = 15L, adaptive.filter.weights = NA),
	# OIHFiFmS_no_hyperpars = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = FALSE, tune.iters = 0L),
	BSMOF = data.table(algorithm = "mbo_multicrit", filter = "custom", surrogate = "randomForest", infill = "cb", propose.points = 15L, adaptive.filter.weights = TRUE, multiobjective = TRUE)
	# OIHFiFmS_no_hyperpars500 = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = FALSE, tune.iters = 500),
	# OIHFiFmS_preset500 = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 500)
	)

experiments = list(
	O = data.table(algorithm = "mosmafs", filter = "none", initialization = "none", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	OI = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	OIFi = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	OIFiFm = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	OIFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	OIH = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	OIHFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	OG = data.table(algorithm = "mosmafs", filter = "none", initialization = "geom", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	OGH = data.table(algorithm = "mosmafs", filter = "none", initialization = "geom", chw.bitflip = TRUE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	OGHFi = data.table(algorithm = "mosmafs", filter = "custom", initialization = "geom", chw.bitflip = TRUE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	OGHFiFm = data.table(algorithm = "mosmafs", filter = "custom", initialization = "geom", chw.bitflip = TRUE, adaptive.filter.weights = FALSE, filter.during.run = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	OGHFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "geom", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 0, multi.objective = TRUE),
	RS = data.table(algorithm = "randomsearch", initialization = "none", filter = "none"),
	RSI = data.table(algorithm = "randomsearch", initialization = "unif", filter = "none"),
	# RSG = data.table(algorithm = "randomsearch", initialization = "geom", filter = "none"),
	RSIF = data.table(algorithm = "randomsearch", initialization = "unif", filter = "custom"),
	# RSGF = data.table(algorithm = "randomsearch", initialization = "geom", filter = "custom"),
	BS1RF = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = FALSE, surrogate = "randomForest", infill = "cb", propose.points = 15L),#, start.recon.iter = 80L),
	BS2RF = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = TRUE, surrogate = "randomForest", infill = "cb", propose.points = 15L, start.recon.iter = 80L),
	BS5SO = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = FALSE, parent.sel = "selTournament", tune.hyperparams = TRUE, tune.iters = 0),
	BSMO = data.table(algorithm = "mbo_multicrit", filter = "custom", surrogate = "randomForest", infill = "cb", propose.points = 15L, adaptive.filter.weights = FALSE),
	# OIHFiFmS_no_hyperpars = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = FALSE, tune.iters = 0L),
	BSMOF = data.table(algorithm = "mbo_multicrit", filter = "custom", surrogate = "randomForest", infill = "cb", propose.points = 15L, adaptive.filter.weights = TRUE)# ,
	# OIHFiFmS_no_hyperpars500 = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = FALSE, tune.iters = 500),
	# OIHFiFmS_preset500 = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 500),
	# OIGFiFmS_no_hyperpars500 = data.table(algorithm = "mosmafs", filter = "custom", initialization = "geom", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = FALSE, tune.iters = 500),
	# OIGFiFmS_preset500 = data.table(algorithm = "mosmafs", filter = "custom", initialization = "geom", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = TRUE, tune.iters = 500)
	)

# b) Problems to reduce 

# on the old registry perform

## Everything reduced except for 
# - BS1RF AP BREAST COLON 
# - RS Madelon 
# - DO NOT USE RSI 

if (grepl("repos", getwd())) {
	datasets = c("sonar", "ionosphere", 
		"hill-valley", "wdbc", "tecator", "lsvt", "isolet", "cnae-9", 
		"clean1", "semeion")
	lapply(datasets, function(x) dir.create(file.path(path, x)))
	collectBenchmarkResults(path, experiments_old, tab)
} else {
	datasets = c("sonar", "ionosphere", 
		"hill-valley", "wdbc", "tecator", "lsvt", "isolet", "cnae-9", 
		"clean1", "semeion", "AP_Breast_Colon", "arcene", "AP_Colon_Kidney", 
		"madelon", "madeline")
	lapply(datasets, function(x) dir.create(file.path(path, x)))
	collectBenchmarkResults(path, experiments, tab)	
}
# on the new one, perform 



# --- 3. Reduce 

# Get all populations in appropriate format 

# HELPER 


tmp = makeRegistry(file.dir = file.path(getwd(), "populations_temp"), make.default = TRUE, 
	conf.file = NA, packages = c("data.table"))
tmp = loadRegistry("populations_temp", conf.file = NA, writeable = TRUE)
tmp$cluster.functions = makeClusterFunctionsMulticore(ncpus = 4L)

args = readRDS("populations/args.rds")
nargs = nrow(args)
nd = length(datasets)
newid = (nargs + 1):(nargs + nd)
args = rbind(args, args = cbind(data.table::CJ(v = c("BS2RF_ENS"), d = datasets), job.id = newid))
saveRDS(args, "populations/args.rds")

args$job.id = NULL

ids = batchMap(getPopulations, args = args, reg = tmp)

submitJobs(newid)



# Get the optimal hyperparamters after 500 runs (single-objective) for intialization

toreduce = ijoin(tab, findDone())
toreduce = ijoin(toreduce, experiments[["BS1RF"]])

nevals = 500L
res = reduceResultsDataTable(toreduce, function(x) getHyperparamsPerProblem(x, nevals))
res = ijoin(tab, res, by = "job.id")

for (prob in unique(res$problem)) {
	res_reduced = res[problem == prob, ]

	if(nrow(res_reduced) == 30) {
		res_reduced = res_reduced[, replication := 1:length(job.id), by = c("learner")]	
		hyperparams = lapply(1:10, function(x) {
			a = res_reduced[replication == x, ]
			z = lapply(a$learner, function(x) {
				a[learner == x, ]$result[[1]]
			})
			names(z) = a$learner
			z
		})
		saveRDS(hyperparams, file.path("data", prob, paste("hyperparams_", nevals, ".rds", sep = "")))			
	} else {
		warning(paste(prob, "not completed yet"))
	}
}

