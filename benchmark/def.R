library(data.table)

# do not overwrite registry
OVERWRITE = FALSE

# --- Specify problem design ---
# p.inf: number of informative features
# p.noise: number of uninformative, noisy features
# n: number of observations
# task.type: hypersphere data or linear data
p.inf = 4
p.noise = c(10, 100, 1000)
n = 1000

task.type = list("hypersphere" = create.hypersphere.data)

# problem design
pdes = list("hypersphere" = CJ(p.inf = p.inf, p.noise = p.noise, n = n))

# --- Specify algorithm design ---
# This needs to be completed
MU = 15L
LAMBDA = 5L 
MAXEVAL = 5L

LEARNERS = list("SVM" = cpoSelector() %>>% makeLearner("classif.ksvm", kernel = "polydot"))

PAR.SETS = list(
	SVM = pSS(	  
	degree: integer[1, 10],
	C: numeric[0.1, 10]
	)
)

ades = CJ(learner = c("SVM"), 
	mu = MU, 
	lambda = LAMBDA,
	maxeval = MAXEVAL, 
	sorted = FALSE)

REPLICATIONS = 10


# Datasets used by Bourani et al. 
# TASK_IDS = list("australian" = 146818, 
# 				"ionosphere" = 116,
# 				"heart" = 12717,
# 				"pima" = 146241,
# 				"glass" = 40,
# 				"german" = 12715,
# 				"sonar" = 269,
# 				"vehicle" = 53)
