packages = c("mlr", "ecr", "OpenML", "magrittr", "mlrCPO", "data.table", "farff")

sapply(packages, require, character.only = TRUE)

source("../datagen.R")
source("../ecrshims.R")
source("../selectorcpo.R")
source("../customnsga2.R")
source("../operators.R")
source("../initialization.R")


# do not overwrite registry
OVERWRITE = TRUE

# --- problem design ---

# problem design
pdes = list(# hypersphere = CJ(p.inf = 4, p.noise = c(20, 100), n = c(200, 1000))),
			ionosphere = data.table(id = 287),
			australian = data.table(id = 146818),
			heart = data.table(id = 282),
			pima = data.table(id = 37),
			glass = data.table(id = 4573))


# --- Specify algorithm design ---

# Machine learning algorithms to be benchmarked
LEARNERS = list("SVM" = cpoSelector() %>>% makeLearner("classif.ksvm", kernel = "rbfdot"),
	"kknn" = cpoSelector() %>>% makeLearner("classif.kknn"))

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
MAXEVAL = 10L


# feature initialization of initial population
INITIALIZATION = list("none" = NULL, "unif" = list(dist = runif), "rgeom0.3" = list(dist = rgeom, prob = 0.3))


# Filtering and Initialization hyperparameters
FILTER_METHOD = list("none" = "none", "auc" = "auc")
FILTER_PARAMS = list("none" = NA, "auc" = list(expectfeats = 5, minprob = 0.1, maxprob = 0.9))

RESAMPLING = list("5CV" = makeResampleDesc("CV", iters = 5, stratify = TRUE))


ades = CJ(learner = c("SVM", "kknn"), 
	mu = c(15L, 40L, 100L), lambda = c(15L, 30L),
	maxeval = MAXEVAL, 
	filter.method = c("auc"),
	resampling = c("5CV"),
	initialization = c("none", "unif"),
	sorted = FALSE)

# add baseline with random sampling
baseline = CJ(learner = unique(ades$learner), 
	mu = MAXEVAL, lambda = 1L,
	maxeval = MAXEVAL, filter.method = "auc", 
	resampling = c("5CV"), initialization = c("none"), 
	sorted = FALSE)

# add baseline
ades = rbind(ades, baseline)

REPLICATIONS = 1L


