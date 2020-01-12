learner = "xgboost"

dataset = "AP_Breast_Colon"

args = CJ(
	dataset = c("madelon", "madeline", "AP_Colon_Kidney", "arcene"),
	nrounds = c(10, 100, 1000, 2000),
	max_depth = c(3, 10, 20), 
	per_feats = c(10, 50, 100),
	early_stopping = FALSE
)

args2 = CJ(
	dataset = c("madelon", "madeline", "AP_Colon_Kidney", "arcene"),
	nrounds = 2000, 
	max_depth = c(3, 10, 20),
	per_feats = c(10, 50, 100),
	early_stopping = TRUE
)

args = rbind(args, args2)

readDataAndRinst = function(data, rinst.iter, ...) {
	task = readRDS(file.path(data, "task.rds"))
	rin = readRDS(file.path(data, "rin.rds"))
	# hyperparams = readRDS(file.path(data, "hyperparams_500.rds"))[[rinst.iter]]

	train.task = subsetTask(task, rin$train.inds[[rinst.iter]])
	test.task = subsetTask(task, rin$test.inds[[rinst.iter]])

	list(train.task = train.task, test.task = test.task)#, hyperparams = hyperparams)
}

