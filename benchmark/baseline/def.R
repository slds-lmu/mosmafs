packages = c("batchtools", "ecr", "magrittr", "mosmafs", "ParamHelpers", "mlr", "mlrCPO", "parallelMap", "RWeka", "mlrMBO")

# source the prob design
source("../probdesign.R")

datafolder = "../data"

# do not overwrite registry
OVERWRITE = FALSE

# Maximum number of evaluations allowed
MAXEVAL = 4000L

# Filtering and Initialization hyperparameters
# According to Guyon we take a information theoretic, a single classifier based and a correlation based filter value
FILTER = list("none" = NULL, "JMI_auc_var" = c("praznik_JMI", "auc", "variance"))

SURROGATE = list(randomForest = cpoImputeConstant("__MISSING__") %>>% makeLearner("regr.randomForest", se.method = "jackknife", keep.inbag = TRUE, predict.type = "se"),
	              km.nugget = cpoDummyEncode() %>>% makeLearner("regr.km", predict.type = "se", par.vals = list(nugget.estim = TRUE, nugget.stability = 10e-8))
)

INFILL = list("cb" = makeMBOInfillCritCB())

ades.random = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVAL, 
			filter = c("none", "JMI_auc_var"),
			initialization = c("none", "unif"), 
			sorted = FALSE)

ades.mbo = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVAL, 
			filter = c("JMI_auc_var"),
			infill = c("cb"),
			surrogate = c("randomForest"),
			MBMOmethod = c("parego"),
			sorted = FALSE)

REPLICATIONS = 10L
