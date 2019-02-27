source("../R/initialization.R")

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
MAXEVAL = 50L

# feature initialization of initial population
INITIALIZATION = list("none" = NULL, "unif" = list(dist = runif), "rgeom0.3" = list(dist = rgeom, prob = 0.3))

# Filtering and Initialization hyperparameters
FILTER_METHOD = list("none" = "none", "auc" = "auc")

RESAMPLING = list("10CV" = makeResampleDesc("CV", iters = 10, stratify = TRUE))

PARENTSEL = list("selSimple" = setup(selSimple), "selTournament" = setup(selTournament, k = 2L))

ades = CJ(learner = c("SVM", "kknn"), 
	mu = c(15L, 80L, 160L), 
	lambda = c(30L),
	maxeval = MAXEVAL, 
	filter.method = c("none"),
	initialization = c("none"), 
	parent.sel = c("selSimple", "selTournament"),
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
