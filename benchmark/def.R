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
pdes = list(hypersphere = data.table(p.inf = 4, p.noise = c(20, 100), n = c(200, 1000))) #,
			# ionosphere = data.table(id = 287))


# --- Specify algorithm design ---

# Machine learning algorithms to be benchmarked
LEARNERS = list("SVM" = cpoSelector() %>>% makeLearner("classif.ksvm", kernel = "polydot"),
	"kknn" = cpoSelector() %>>% makeLearner("classif.kknn"))

# Tuning parameter sets to be benchmarked
PAR.SETS = list(
	SVM = pSS(	  
		C: numeric[10^(-3), 10^3],
		degree: integer[1, 20]
	),
	kknn = pSS(
		k: integer[1, 50],
		distance: numeric[0, 100])#,
		#kernel: discrete[rectangular, optimal, triangular, triweight, biweight, cos, inv, gaussian])
)

# Maximum number of evaluations allowed
MAXEVAL = 5000L


# feature initialization of initial population
INITIALIZATION = list("none" = NULL, "unif" = list(dist = runif), "rgeom0.3" = list(dist = rgeom, prob = 0.3))


# Filtering and Initialization hyperparameters
FILTER_METHOD = list("none" = "none", "auc" = "auc")
FILTER_PARAMS = list("none" = NA, "auc" = list(expectfeats = 5, minprob = 0.1, maxprob = 0.9))

RESAMPLING = list("5CV" = makeResampleDesc("CV", iters = 5, stratify = TRUE))


ades = CJ(learner = c("SVM", "kknn"), 
	mu = c(40, 100, 200), lambda = c(0.05, 0.1, 0.2),
	maxeval = MAXEVAL, 
	filter.method = c("none"),
	resampling = c("5CV"),
	initialization = c("none", "unif"),
	sorted = FALSE)

# add baseline with random sampling
baseline = CJ(learner = unique(ades$learner), 
	mu = MAXEVAL, lambda = 1L,
	maxeval = MAXEVAL, filter.method = "none", 
	resampling = c("5CV"), initialization = c("none"), 
	sorted = FALSE)

# add baseline
ades = rbind(ades, baseline)

REPLICATIONS = 1L


