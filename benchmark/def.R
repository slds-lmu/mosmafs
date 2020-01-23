# --- packages needed
# devtools::install_github("bertcarnell/lhs")
packages = c("batchtools", "data.table", "ecr", "magrittr", "mosmafs", "ParamHelpers", "mlr", "mlrCPO", "parallelMap", "mlrMBO", "doParallel")
lapply(packages, library, character.only = TRUE)

# --- source the prob design
source("probdesign.R")
pdes = lapply(datasets, function(x) data.table(rinst.iter = 1:10))
names(pdes) = datasets

source("helpers.R")

# --- data folder 
datafolder = "data"

# --- do never overwrite registry
OVERWRITE = FALSE

# --- Maximum number of evaluations allowed
MAXEVAL = 2000L

# --- Maximum runtime
MAXTIME = 3600L * 40L

# --- Number of inner cross-validation iterations
CVITERS = 10L

# --- Parent SelectionS
PARENTSEL = list("selSimple" = ecr::setup(selSimple), "selNondom" = ecr::setup(selNondom), "selTournamentMO" = ecr::setup(selTournamentMO), "selTournament" = ecr::setup(selTournament))

# --- Feature mutation 
FEATURE_MUT = list("mutBitflipCHW" = ecr::setup(mutBitflipCHW), "mutBitflip" = mutBitflip, "mutUniformMetaResetSHW" = mutUniformMetaResetSHW)

# --- Filtering and Initialization hyperparameters
# --- According to Guyon we take a information theoretic, a single classifier based and a correlation based filter value
FILTER = list("none" = NULL,
	"custom" = c("FSelectorRcpp_information.gain", "randomForest_importance", "praznik_JMI", "auc", "praznik_CMIM", "DUMMY"))

# --- definitions of MBO baseline 
SURROGATE = list(randomForest = cpoImputeConstant("__MISSING__") %>>% makeLearner("regr.randomForest", se.method = "sd", predict.type = "se"),
	              km.nugget = cpoDummyEncode() %>>% makeLearner("regr.km", predict.type = "se", par.vals = list(nugget.estim = TRUE, nugget.stability = 10e-8))
)

INFILL = list("cb" = makeMBOInfillCritCB())


# --- Specify algorithmic designs 
ades.random = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVAL, 
			filter = c("none", "custom"),
			initialization = c("none", "unif"), 
			cv.iters = CVITERS,
			sorted = FALSE)

# ades.mbo = CJ(learner = c("SVM", "kknn", "xgboost"), 
# 			maxeval = MAXEVAL, 
# 			filter = c("custom"),
# 			infill = c("cb"),
# 			surrogate = c("randomForest"),
# 			MBMOmethod = c("parego"),
# 			propose.points = c(2L),
# 			maxtime = MAXTIME, 
# 			sorted = FALSE)

ades.mosmafs = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVAL, 
			filter = c("none", "custom"),
			cv.iters = CVITERS,
			initialization = c("none", "unif", "geom"), 
			lambda = 15L,
			mu = 80L,
			parent.sel = c("selTournamentMO", "selTournament"),
			chw.bitflip = c(FALSE, TRUE),
			adaptive.filter.weights = c(FALSE,TRUE),
			filter.during.run = c(FALSE, TRUE),
			multi.objective = c(TRUE, FALSE),
			tune.hyperparams = c(FALSE, TRUE),
			tune.iters = c(0L, 500L),
			sorted = FALSE)

ades.no_feature_sel = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVAL, 
			maxtime = MAXTIME,
			cv.iters = CVITERS,
			filter = c("custom"), 
			surrogate = c("randomForest"), # "km.nugget"),
			infill = c("cb"),
      		filter.during.run = c(FALSE, TRUE), 
      		propose.points = 15L, 
      		start.recon.iter = 80L, 
      		step.size = 120L, 
      		sorted = FALSE)

ades.mbo_multicrit = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVAL, 
			maxtime = MAXTIME,
			cv.iters = CVITERS,
			filter = c("custom"), 
			surrogate = c("randomForest"), # "km.nugget"),
			infill = c("cb"),
      		propose.points = 15L, 
      		adaptive.filter.weights = c(FALSE, TRUE),
      		sorted = FALSE)


REPLICATIONS = 1L
