# problem design
datafolder = "data"

# --- problem design ---
datasets = c("sonar", "ionosphere",
	"hill-valley", "wdbc", "tecator", "lsvt", "isolet", "cnae-9", 
	"clean1", "semeion", "AP_Breast_Colon", "arcene", 
	"AP_Colon_Kidney", "madelon", "madeline")

datasets.earlystop = c("AP_Breast_Colon", "arcene", "AP_Colon_Kidney", "madelon", "madeline")

# --- specify learners ---
# Machine learning algorithms to be benchmarked
LEARNERS = list("SVM" = makeLearner("classif.ksvm", kernel = "rbfdot"),
	"kknn" = makeLearner("classif.kknn"),
	"xgboost" = makeLearner("classif.xgboost", id = "classif.xgboost", eval_metric = "error", objective = "binary:logistic")
	)

# Tuning parameter sets to be benchmarked
# TODO: kernel parameter SVM?? 
PAR.SETS = list(
	SVM = makeParamSet(
		makeNumericParam("C", lower = -10, 10, trafo = function(x) 2^x),
		makeNumericParam("sigma", lower = -10, 10, trafo = function(x) 2^x)
	),
	kknn = makeParamSet(
		makeIntegerParam("k", lower = 1L, upper = 50L),
		makeNumericParam("distance", lower = 1, upper = 100),
		makeDiscreteParam("kernel", values = c("rectangular", "optimal", "triangular", "biweight"))
	),
	xgboost = makeParamSet(
		# do early stopping instead for the biggere datasets
	  	makeIntegerParam("nrounds", lower = 1L, upper = 2000L),	
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
