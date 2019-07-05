# --- packages needed
packages = c("batchtools", "ecr", "magrittr", "mosmafs", "ParamHelpers", "mlr", "mlrCPO", "parallelMap", "mlrMBO", "doParallel")

# --- source the prob design
source("probdesign.R")
pdes = lapply(datasets, function(x) data.table(rinst.iter = 1:10))
names(pdes) = datasets

# --- data folder 
datafolder = "data"

# --- do never overwrite registry
OVERWRITE = FALSE

# --- Maximum number of evaluations allowed
MAXEVAL = 4000L

# --- Maximum runtime
MAXTIME = 3600L * 40L

# --- Number of inner cross-validation iterations
CVITERS = 10L

# --- Parent SelectionS
PARENTSEL = list("selSimple" = ecr::setup(selSimple), "selNondom" = ecr::setup(selNondom), "selTournamentMO" = ecr::setup(selTournamentMO))

# --- Feature mutation 
FEATURE_MUT = list("mutBitflipCHW" = ecr::setup(mutBitflipCHW), "mutBitflip" = mutBitflip, "mutUniformMetaResetSHW" = mutUniformMetaResetSHW)

# --- Filtering and Initialization hyperparameters
# --- According to Guyon we take a information theoretic, a single classifier based and a correlation based filter value
FILTER = list("none" = NULL,
	"custom" = c("FSelectorRcpp_information.gain", "randomForestSRC_var.select", "praznik_JMI", "auc", "praznik_CMIM", "DUMMY"))

# --- definitions of MBO baseline 
SURROGATE = list(randomForest = cpoImputeConstant("__MISSING__") %>>% makeLearner("regr.randomForest", se.method = "jackknife", keep.inbag = TRUE, predict.type = "se"),
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

ades.mbo = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVAL, 
			filter = c("custom"),
			infill = c("cb"),
			surrogate = c("randomForest"),
			MBMOmethod = c("parego"),
			propose.points = c(2L),
			maxtime = MAXTIME, 
			sorted = FALSE)

ades.mosmafs = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVAL, 
			filter = c("none"), # "custom"),
			cv.iters = CVITERS,
			initialization = c("none", "unif"), 
			lambda = 2L,
			mu = 3L,
			parent.sel = c("selTournamentMO"),
			chw.bitflip = c(FALSE, TRUE),
			adaptive.filter.weights = c(FALSE), # TRUE),
			filter.during.run = c(FALSE),# TRUE),
			sorted = FALSE)

ades.no_feature_sel = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVAL, 
			maxtime = MAXTIME,
			cv.iters = CVITERS,
			sorted = FALSE)
REPLICATIONS = 1L
