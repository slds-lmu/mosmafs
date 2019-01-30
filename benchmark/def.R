library(data.table)

# do not overwrite registry
OVERWRITE = FALSE

# --- problem design ---

# problem design
pdes = list(hypersphere = data.table(p.inf = 4, p.noise = c(10, 100, 1000), n = 1000),
			vehicle = data.table(id = 53))


# --- Specify algorithm design ---

# Machine learning algorithms to be benchmarked
LEARNERS = list("SVM" = cpoSelector() %>>% makeLearner("classif.ksvm", kernel = "polynomial"))

# Tuning parameter sets to be benchmarked
PAR.SETS = list(
	SVM = pSS(	  
	C: numeric[0.1, 10],
	degree: integer[1, 10],
	)
)

# EA hyperparameters
MU = 15L
LAMBDA = 3L 
MAXEVAL = 500L

# Filtering and Initialization hyperparameters
FILTER_METHOD = list("auc" = "auc")
FILTER_PARAMS = list("auc" = list(expectfeats = 5, minprob = 0.1, maxprob = 0.9))


ades = CJ(learner = c("SVM"), 
	mu = MU, lambda = LAMBDA,
	maxeval = MAXEVAL, 
	filter.method = c(NA, "auc"),
	sorted = FALSE)


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
