packages = c("batchtools", "ecr", "magrittr", "mosmafs", "ParamHelpers", "mlr", "mlrCPO", "parallelMap", "mlrMBO", "doParallel", "FSelectorRcpp")

# source the prob design
source("probdesign.R")

datafolder = "data"

# do not overwrite registry
OVERWRITE = FALSE

# Maximum number of evaluations allowed
MAXEVAL = 4000L

# Parent SelectionS
PARENTSEL = list("selSimple" = ecr::setup(selSimple), "selNondom" = ecr::setup(selNondom), "selBinaryTournament" = ecr::setup(selTournamentMO))


FEATURE_MUT = list("mutBitflipCHW" = ecr::setup(mutBitflipCHW), "mutBitflip" = mutBitflip, "mutUniformMetaResetSHW" = mutUniformMetaResetSHW)

# Filtering and Initialization hyperparameters
# According to Guyon we take a information theoretic, a single classifier based and a correlation based filter value
FILTER = list("none" = NULL,
	"custom" = c("FSelectorRcpp_information.gain", "randomForestSRC_var.select", "praznik_JMI", "auc", "praznik_CMIM", "DUMMY"))

ades.random = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVAL, 
			filter = c("none", "custom"),
			initialization = c("none", "unif"), 
			sorted = FALSE)

ades.mosmafs = CJ(learner = c("xgboost"), 
			maxeval = MAXEVAL, 
			filter = c("none", "custom"),
			initialization = c("none", "unif"), 
			lambda = 15L,
			mu = 80,
			parent.sel = c("selTournamentMO"),
			chw.bitflip = c(FALSE, TRUE),
			adaptive.filter.weights = c(FALSE, TRUE),
			filter.during.run = c(FALSE, TRUE),
			sorted = FALSE)

REPLICATIONS = 10L
