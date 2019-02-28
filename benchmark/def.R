packages = c("batchtools", "ecr", "magrittr", "mosmafs", "ParamHelpers", "mlr", "mlrCPO")

source("../initialization.R")

# do not overwrite registry
OVERWRITE = FALSE

# --- problem design ---
datafolder = "data"
datasets = list.dirs(path = datafolder, recursive = FALSE, full.names = FALSE)
datasets = datasets[-1]

# --- Specify algorithm design ---

# Machine learning algorithms to be benchmarked
LEARNERS = list("SVM" = makeLearner("classif.ksvm", kernel = "rbfdot"),
	"kknn" = makeLearner("classif.kknn"))

# Tuning parameter sets to be benchmarked
PAR.SETS = list(
	SVM = pSS(	  
		C: numeric[10^(-3), 10^3], # according to Fröhlich et al. 
		sigma: numeric[10^(-3), 10^3]
	),
	kknn = pSS(
		k: integer[1, 50],
		distance: numeric[1, 100])#,
		#kernel: discrete[rectangular, optimal, triangular, triweight, biweight, cos, inv, gaussian])
)

# Maximum number of evaluations allowed
MAXEVAL = 10000L

# feature initialization of initial population
INITIALIZATION = list("none" = NULL, "unif" = list(dist = runif), "rgeom0.3" = list(dist = rgeom, prob = 0.3))

# Filtering and Initialization hyperparameters
FILTER_METHOD = list("none" = "none", "auc" = "auc")

PARENTSEL = list("selSimple" = setup(selSimple), "selDomHV" = setup(selDomHV, ref.point = c(1, 1)), "selNondom" = setup(selNondom))

ades = CJ(learner = c("SVM"), 
	mu = c(15L, 80L, 160L), 
	lambda = c(15L),
	maxeval = MAXEVAL, 
	filter.method = c("none"),
	initialization = c("none", "unif"), 
	parent.sel = c("selNondom", "selDomHV"),
	sorted = FALSE)

# add baseline with random sampling
# baseline = CJ(learner = unique(ades$learner), 
# 	mu = MAXEVAL, lambda = 1L,
# 	maxeval = 1L, filter.method = "none", 
# 	resampling = c("10CV"), initialization = c("none"), 
# 	parent.sel = c("selSimple"),
# 	sorted = FALSE)

# add baseline
# ades = rbind(ades, baseline)

REPLICATIONS = 5L
