packages = c("mlr", "ecr", "OpenML", "magrittr", "mlrCPO", "data.table", "farff")

sapply(packages, require, character.only = TRUE)

source("../datagen.R")
source("../ecrshims.R")
source("../selectorcpo.R")
source("../customnsga2.R")
source("../operators.R")

# do not overwrite registry
OVERWRITE = FALSE

# --- problem design ---

# problem design
pdes = list(hypersphere = data.table(p.inf = 4, p.noise = c(10, 100, 200), n = 100),
			lin.toy.data = data.table(n = 100),
			ionosphere = data.table(id = 287))


# --- Specify algorithm design ---

# Machine learning algorithms to be benchmarked
LEARNERS = list("SVM" = cpoSelector() %>>% makeLearner("classif.ksvm", kernel = "polydot"))

# Tuning parameter sets to be benchmarked
PAR.SETS = list(
	SVM = pSS(	  
	C: numeric[10^(-3), 10^3],
	degree: integer[1, 20]
	)
)

# Maximum number of evaluations allowed
MAXEVAL = 20L

# Filtering and Initialization hyperparameters
FILTER_METHOD = list("none" = "none", "auc" = "auc")
FILTER_PARAMS = list("none" = NA, "auc" = list(expectfeats = 5, minprob = 0.1, maxprob = 0.9))

RESAMPLING = list("10CV" = makeResampleDesc("CV", iters = 10, stratify = TRUE))


ades = CJ(learner = c("SVM"), 
	mu = c(10), lambda = c(0.1, 0.5),
	maxeval = MAXEVAL, 
	filter.method = c("none", "auc"),
	resampling = c("10CV"),
	sorted = FALSE)

# add baseline with random sampling
baseline = CJ(learner = unique(ades$learner), mu = MAXEVAL, lambda = 1L,
	maxeval = MAXEVAL, filter.method = "none", resampling = c("10CV"), sorted = FALSE)

# add baseline
ades = rbind(ades, baseline)

REPLICATIONS = 1




# Datasets used by Bourani et al. 
# TASK_IDS = list("australian" = 146818, 
# 				"ionosphere" = 116,
# 				"heart" = 12717,
# 				"pima" = 146241,
# 				"glass" = 40,
# 				"german" = 12715,
# 				"sonar" = 269,
# 				"vehicle" = 53)
