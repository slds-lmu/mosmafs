packages = c("batchtools", "ecr", "magrittr", "mosmafs", "ParamHelpers", "mlr", "mlrCPO", "parallelMap", "RWeka", "mlrMBO")

# source the prob design
source("../probdesign.R")

datafolder = "../data"

# do not overwrite registry
OVERWRITE = FALSE

# Maximum number of evaluations allowed
MAXEVAL = 4000L

# Parent SelectionS
PARENTSEL = list("selSimple" = setup(selSimple), "selDomHV" = setup(selDomHV, ref.point = c(1, 1)), "selNondom" = setup(selNondom), "selBinaryTournament" = setup(selTournamentMO))

FEATURE_MUT = list("mutBitflipCHW" = setup(mutBitflipCHW), "mutBitflip" = mutBitflip)


# Filtering and Initialization hyperparameters
# According to Guyon we take a information theoretic, a single classifier based and a correlation based filter value
FILTER = list("none" = NULL, "JMI_auc_var" = c("praznik_JMI", "auc", "variance"),
	"custom" = c("FSelectorRcpp_information.gain", "randomForestSRC_var.select", "praznik_JMI", "auc", "praznik_CMIM"))

SURROGATE = list(randomForest = cpoImputeConstant("__MISSING__") %>>% makeLearner("regr.randomForest", se.method = "jackknife", keep.inbag = TRUE, predict.type = "se"),
	              km.nugget = cpoDummyEncode() %>>% makeLearner("regr.km", predict.type = "se", par.vals = list(nugget.estim = TRUE, nugget.stability = 10e-8))
)

INFILL = list("cb" = makeMBOInfillCritCB())

ades.random = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVAL, 
			filter = c("none", "custom"),
			initialization = c("none", "unif"), 
			sorted = FALSE)

ades.mbo = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = 2000L, 
			filter = c("custom"),
			infill = c("cb"),
			surrogate = c("randomForest"),
			MBMOmethod = c("parego"),
			propose.points = c(10L),
			sorted = FALSE)

ades.mosmafs = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVAL, 
			filter = c("none", "custom"),
			initialization = c("none", "unif"), 
			lambda = c(15),
			mu = 80,
			parent.sel = c("selDomHV"),
			feature.mut = c("mutBitflip", "mutBitflipCHW"),
			sorted = FALSE)

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