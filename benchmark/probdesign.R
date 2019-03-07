# problem design

# --- problem design ---
datafolder = "../data"
datasets = c("sonar", "ionosphere", "madelon")#, "arcene", "dexter")

# --- specify learners ---
# Machine learning algorithms to be benchmarked
LEARNERS = list("SVM" = makeLearner("classif.ksvm", kernel = "rbfdot"),
	"kknn" = makeLearner("classif.kknn"),
	"xgboost" = makeLearner("classif.xgboost")
	)

# Tuning parameter sets to be benchmarked
PAR.SETS = list(
	SVM = pSS(	  
		C: numeric[10^(-3), 10^3], # according to Fröhlich et al. 
		sigma: numeric[10^(-3), 10^3]
	),
	kknn = pSS(
		k: integer[1, 50],
		distance: numeric[1, 100]),
		#kernel: discrete[rectangular, optimal, triangular, triweight, biweight, cos, inv, gaussian])
	xgboost = makeParamSet(
	  makeNumericParam("eta", lower = 0.01, upper = 0.2),
	  makeNumericParam("gamma", lower = -7, upper = 6, trafo = function(x) 2^x),
	  makeIntegerParam("max_depth", lower = 3, upper = 20),
	  makeNumericParam("colsample_bytree", lower = 0.5, upper = 1),
	  makeNumericParam("colsample_bylevel", lower = 0.5, upper = 1),
	  makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x) 2^x),
	  makeNumericParam("alpha", lower = -10, upper = 10, trafo = function(x) 2^x),
	  makeNumericParam("subsample", lower = 0.5, upper = 1)
	)
)
