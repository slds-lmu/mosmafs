packages = c("batchtools", "ecr", "magrittr", "mosmafs", "ParamHelpers", "mlr", "mlrCPO", "parallelMap")

source("../initialization.R")

# do not overwrite registry
OVERWRITE = FALSE

# --- problem design ---
datafolder = "data"
datasets = list.dirs(path = datafolder, recursive = FALSE, full.names = FALSE)
datasets = datasets[- which(datasets == "gisette")]

# --- Specify algorithm design ---

# Machine learning algorithms to be benchmarked
LEARNERS = list("SVM" = makeLearner("classif.ksvm", kernel = "rbfdot"),
	"kknn" = makeLearner("classif.kknn"),
	"SVM.MOFS" = makeLearner("classif.ksvm", kernel = "rbfdot"))

# Tuning parameter sets to be benchmarked
PAR.SETS = list(
	SVM = pSS(	  
		C: numeric[10^(-3), 10^3], # according to Fröhlich et al. 
		sigma: numeric[10^(-3), 10^3]
	),
	kknn = pSS(
		k: integer[1, 50],
		distance: numeric[1, 100]),
		#kernel: discrete[rectangular, optimal, triangular, triweight, biweight, cos, inv, gaussian])
	SVM.MOFS = pSS()
)

# Maximum number of evaluations allowed
MAXEVAL = 10000L

# feature initialization of initial population
INITIALIZATION = list("none" = NULL, "unif" = list(dist = runif), "rgeom0.3" = list(dist = rgeom, prob = 0.3))

# Filtering and Initialization hyperparameters
# According to Guyon we take a information theoretic, a single classifier based and a correlation based filter value
FILTER_METHOD = list("none" = "none", "JMI_auc_var" = c("praznik_JMI", "auc", "variance"))

PARENTSEL = list("selSimple" = setup(selSimple), "selDomHV" = setup(selDomHV, ref.point = c(1, 1)), "selNondom" = setup(selNondom))

FEATURE_MUT = list("mutBitflipCHW" = setup(mutBitflipCHW), "mutBitflip" = mutBitflip)

ades = CJ(learner = c("SVM", "kknn"), 
	mu = c(80L), 
	lambda = c(15L),
	maxeval = MAXEVAL, 
	filter.method = c("none", "JMI_auc_var"),
	initialization = c("none", "unif"), 
	parent.sel = c("selDomHV"),
	feature.mut = c("mutBitflipCHW", "mutBitflip"),
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

REPLICATIONS = 10L

# mutation strategy according to MIES (R. Li et al. )
makeMutationStrategyNumeric <- function(param.name, output.name, lr, lower, upper) {
  function(ind) {
    param <- ind[[param.name]]
    # assertNumeric(param, lower = 0, upper = 1 - .Machine$double.eps, any.missing = FALSE)
    res = param * exp(lr * rnorm(0, 1))
    res = min(max(res, lower), upper)
    namedList(output.name, res)
  }
}