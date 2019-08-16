packages = c("batchtools", "ecr", "magrittr", "mosmafs", "ParamHelpers", "mlr", "mlrCPO", "parallelMap", "mlrMBO", "doParallel")

# source the prob design
source("probdesign.R")
source("helpers.R")

datafolder = "data"

# test registry should be overwritten
OVERWRITE = TRUE

# Maximum number of evaluations allowed
MAXEVAL = 30L

# Maximum runtime
MAXTIME = 3600L * 40L

# number of inner cross-validation iterations
CVITERS = 3L

# Parent Selections
PARENTSEL = list("selSimple" = ecr::setup(selSimple), "selNondom" = ecr::setup(selNondom), "selTournamentMO" = ecr::setup(selTournamentMO), "selTournament" = ecr::setup(selTournament))

FEATURE_MUT = list("mutBitflipCHW" = ecr::setup(mutBitflipCHW), "mutBitflip" = mutBitflip, "mutUniformMetaResetSHW" = mutUniformMetaResetSHW)

# Filtering and Initialization hyperparameters
# According to Guyon we take a information theoretic, a single classifier based and a correlation based filter value

pdes = lapply(datasets, function(x) data.table(rinst.iter = 1:2))
names(pdes) = datasets

makeFilter(
  name = "Featureless",
  desc = "Dummy filter",
  pkg = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = "numerics",
  fun = function(task, nselect, na.rm = TRUE, ...) {
    data = getTaskData(task)
    sapply(getTaskFeatureNames(task), function(feat.name) {
      if (feat.name == "attribute_1")
      	1000
      else 
      	0
    })
  })


FILTER = list("none" = NULL,
	"custom" = c("FSelectorRcpp_information.gain", "randomForestSRC_var.select", "praznik_JMI", "auc", "praznik_CMIM", "DUMMY"), 
	"testfilter" = c("Featureless"), 
  "testfilter2" = c("praznik_JMI"))

SURROGATE = list(randomForest = cpoImputeConstant("__MISSING__") %>>% makeLearner("regr.randomForest", se.method = "jackknife", keep.inbag = TRUE, predict.type = "se"),
	              km.nugget = cpoDummyEncode() %>>% makeLearner("regr.km", predict.type = "se", par.vals = list(nugget.estim = TRUE, nugget.stability = 10e-8))
)

INFILL = list("cb" = makeMBOInfillCritCB())

ades.random = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVAL, 
			filter = c("none", "custom", "testfilter"),
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
            filter = c("none", "custom"),
            cv.iters = CVITERS,
            initialization = c("none", "unif"), 
            lambda = 2L,
            mu = 3L,
            parent.sel = c("selTournamentMO", "selTournament"),
            chw.bitflip = c(FALSE, TRUE),
            adaptive.filter.weights = c(FALSE,TRUE),
            filter.during.run = c(FALSE, TRUE),
            multi.objective = TRUE,
            tune.hyperparams = FALSE,
            tune.iters = 500,
            sorted = FALSE)

ades.no_feature_sel = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVAL, 
			maxtime = MAXTIME,
			cv.iters = CVITERS,
			filter = c("custom"), 
			surrogate = c("randomForest"),
			infill = c("cb"),
      		filter.during.run = c(FALSE, TRUE), 
      		propose.points = 15L, 
      		sorted = FALSE)


ades.mbo_multicrit = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVAL, 
			maxtime = MAXTIME,
			cv.iters = CVITERS,
			filter = c("custom"), 
			surrogate = c("randomForest"), # "km.nugget"),
			infill = c("cb"),
      		propose.points = 15L, 
      		sorted = FALSE)


REPLICATIONS = 1L