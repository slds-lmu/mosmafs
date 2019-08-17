library(data.table)
library(ggplot2)
library(batchtools)
library(ecr)
library(gridExtra)
library(Hmisc)
library(dplyr)
library(plyr)
library(ggpubr)

source("helpers.R")

# --- 1. Metadata

# path to store plots
plotspath = "results_plots"

# path to store latex tables
latex_path = "latex_temp"

# path where raw results are stored
data_path = "results_raw"


# --- 2. Experiments that have been run 

# a) Algorithm versions
experiments_list = list(
	O = data.table(algorithm = "mosmafs", filter = "none", initialization = "none", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, multi.objective = NA, parent.sel = "selTournamentMO", tune.hyperparams = NA),
	OI = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, multi.objective = NA, parent.sel = "selTournamentMO", tune.hyperparams = NA),
	OIFi = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, multi.objective = NA, parent.sel = "selTournamentMO", tune.hyperparams = NA),
	OIFiFm = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = TRUE, multi.objective = NA, parent.sel = "selTournamentMO", tune.hyperparams = NA),
	OIFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = NA, parent.sel = "selTournamentMO", tune.hyperparams = NA),
	OIH = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, multi.objective = NA, parent.sel = "selTournamentMO", tune.hyperparams = NA),
	OIHFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = NA, parent.sel = "selTournamentMO", tune.hyperparams = NA),
	RS = data.table(algorithm = "randomsearch", initialization = "none", filter = "none", chw.bitflip = NA, adaptive.filter.weights = NA, filter.during.run = NA),
	RSI = data.table(algorithm = "randomsearch", initialization = "unif", filter = "none", chw.bitflip = NA, adaptive.filter.weights = NA, filter.during.run = NA),
	RSIF = data.table(algorithm = "randomsearch", initialization = "unif", filter = "custom", chw.bitflip = NA, adaptive.filter.weights = NA, filter.during.run = NA),
	BS1RF = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = FALSE, surrogate = "randomForest", infill = "cb", propose.points = 15L),
	BS2RF = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = TRUE, surrogate = "randomForest", infill = "cb", propose.points = 15L),
	BS5SO = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = FALSE, parent.sel = "selTournament"),
	BSMO = data.table(algorithm = "mbo_multicrit", filter = "custom", surrogate = "randomForest", infill = "cb", propose.points = 15L),
	OIHFiFmS_no_hyperpars = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = TRUE, parent.sel = "selTournamentMO", tune.hyperparams = FALSE)
	)
experiments = rbindlist(experiments_list, fill = TRUE)
experiments$variant = names(experiments_list)

# b) Datasets
problems = data.table(problem = c(
	"wdbc", "ionosphere", "sonar", "hill-valley", "clean1", 
	"tecator", "USPS", "madeline", "lsvt", "madelon", "isolet", "cnae-9", "semeion"),
	n = c(569, 351, 208, 1212, 476, 240, 1424, 3140, 126, 2600, 600, 240, 319),
	p = c(30, 34, 60, 100, 168, 124, 256, 259, 310, 500, 617, 856, 256))

# problems madelon and madeline did not run through
problems = problems[- which(problem %in% c("madeline", "madelon", "USPS"))]
problems = problems[order(problems$p), ]

# --- 3. Create Visualizations

files_loc = list.files(data_path, recursive = TRUE, full.names = TRUE)
files_loc = grep("/.*/", files_loc, value = TRUE)
files = lapply(files_loc, readRDS)
res = rbindlist(files, fill = TRUE)

# --- Analyze mosmafs
limits = list(c(0.5, 1))
metrics = c("eval.domHV", "true.hout.domHV", "naive.hout.domHV")

# All results 
for (lim in limits) {
	prompt = "all"
	dir.create(file.path(plotspath, prompt))
	for (metric in metrics) {
		dir.create(file.path(plotspath, prompt, gsub("\\.", "", metric)))
		plotMosmafsOptResult(res = res, plotspath = plotspath, 
		experiments = experiments, 
		metric =  metric, prompt = prompt, limits = lim)#, height = 8, width = 7)
	}
}

# Mosmafs only  
for (lim in limits) {
	prompt = "mosmafs"
	dir.create(file.path(plotspath, prompt))
	for (metric in metrics) {
		dir.create(file.path(plotspath, prompt, gsub("\\.", "", metric)))
		plotMosmafsOptResult(res = res, plotspath = plotspath, 
		experiments = experiments[algorithm == "mosmafs", ], 
		metric =  metric, prompt = prompt, limits = lim)#, height = 8, width = 7)
	}
}

# Mosmafs against RS baselines   
for (lim in limits) {
	prompt = "baselines_rs"
	dir.create(file.path(plotspath, prompt))
	for (metric in metrics) {
		dir.create(file.path(plotspath, prompt, gsub("\\.", "", metric)))		
		plotMosmafsOptResult(res = res, plotspath = plotspath, 
		experiments = experiments[variant %in% c("O", "OIH", "OIHFiFmS", "RS", "RSI", "RSIF"), ], 
		metric =  metric, prompt = prompt, limits = lim)#, height = 8, width = 7)
	}
}

# Mosmafs against MBO baselines   
for (lim in limits) {
	prompt = "baselines_mbo"
	dir.create(file.path(plotspath, prompt))
	for (metric in metrics) {
		dir.create(file.path(plotspath, prompt, gsub("\\.", "", metric)))		
		plotMosmafsOptResult(res = res, plotspath = plotspath, 
		experiments = experiments[variant %in% c("O", "OIH", "OIHFiFmS", "BSMO", "OIHFiFmS_no_hyperpars"), ], 
		metric =  metric, prompt = prompt, limits = lim)#, height = 8, width = 7)
	}
}


# Mosmafs against MBO baselines (including SO)  
for (lim in limits) {
	prompt = "baselines_mbo_with_SO"
	dir.create(file.path(plotspath, prompt))
	for (metric in metrics) {
		dir.create(file.path(plotspath, prompt, gsub("\\.", "", metric)))		
		plotMosmafsOptResult(res = res, plotspath = plotspath, 
		experiments = experiments[variant %in% c("O", "OIH", "OIHFiFmS", "BSMO", "BS1RF", "BS2RF", "OIHFiFmS_no_hyperpars"), ], 
		metric =  metric, prompt = prompt, limits = lim, plotRanks = FALSE)#, height = 8, width = 7)
	}
}























# --- 4. Debugging: Visualize single results
result = readRDS(file.path(data_path, "single_experiments_lsvt.rds"))

# get the first 100 points 

dfl = list()

for (i in 1:3) {
	exp_mosmafs = res[i, ]$result[[1]]$result
	fitnesses = popAggregate(exp_mosmafs$log, "fitness")
	hofitnesses = popAggregate(exp_mosmafs$log, "fitness.holdout")

	exp_mosmafs2 = res[i + 6, ]$result[[1]]$result
	fitnesses2 = popAggregate(exp_mosmafs2$log, "fitness")
	hofitnesses2 = popAggregate(exp_mosmafs2$log, "fitness.holdout")



	object <- res[i + 3, ]$result[[1]]
	task.p <- mlr::getTaskNFeats(object$train.task)
	df_mbo = data.table(data.frame(res[i + 3, ]$result[[1]]$result$opt.path))
	names(df_mbo)[which(names(df_mbo) %in% c("y_1", "y_2", "fitness.holdout.perf", "fitness.holdout.propfeat"))] = c("eval.perf", "eval.propfeat", "hout.perf", "hout.propfeat")
	df_mbo$eval.propfeat <- ceiling(task.p * df_mbo$eval.propfeat) / task.p
	df_mbo$hout.propfeat <- ceiling(task.p * df_mbo$hout.propfeat) / task.p
	df_mbo = df_mbo[, c("eval.perf", "eval.propfeat", "hout.perf", "hout.propfeat")]
	df_mbo$variant = c("mbo")
	df_mbo$evals = 1:nrow(df_mbo)
	df_mbo$gen = 1

	# --- take the first population / first 80 steps
	df_mosmafs = do.call(rbind, lapply(1:length(fitnesses), function(i)
		cbind(as.data.frame(t(fitnesses[[i]])), as.data.frame(t(hofitnesses[[i]])))))
	names(df_mosmafs) = c("eval.perf", "eval.propfeat", "hout.perf", "hout.propfeat")
	# df_mosmafs = unique(df_mosmafs, by = names(df_mosmafs))
	df_mosmafs$variant = c("mosmafs")
	df_mosmafs$evals = 1:nrow(df_mosmafs)
	df_mosmafs$gen = rep(0:(length(fitnesses) - 1), each = 80)
	df_mosmafs$evals = 80 + 15 * df_mosmafs$gen

	df_mosmafs2 = do.call(rbind, lapply(1:length(fitnesses2), function(i)
		cbind(as.data.frame(t(fitnesses2[[i]])), as.data.frame(t(hofitnesses2[[i]])))))
	names(df_mosmafs2) = c("eval.perf", "eval.propfeat", "hout.perf", "hout.propfeat")
	df_mosmafs2$variant = c("mosmafs2")
	df_mosmafs2$evals = 1:nrow(df_mosmafs2)
	df_mosmafs2$gen = rep(0:(length(fitnesses2) - 1), each = 80)
	df_mosmafs2$evals = 80 + 15 * df_mosmafs2$gen

	dfl[[i]] = rbind(df_mbo, df_mosmafs, df_mosmafs2)

	dfl[[i]]$diff = dfl[[i]]$hout.perf - dfl[[i]]$eval.perf

	dfl[[i]]$exp = i
}


df = rbindlist(dfl, fill = TRUE)
df$overestim = df$diff > 0


plist = list()
i = 1

for (maxgen in c(0, 100, 200, 250)){

	maxevals = maxgen * 15 + 80

	# FOR MOSMAFS, ONLY PLOT CURRENT GENERATION 
	dfp = df[evals <= maxevals & exp == 2, ]
	dfp = dfp[variant == "mbo" | (variant != "mbo" & gen == maxgen)]
	# dfp = dfp[gen == maxgen]
	dfp$id = paste(dfp$variant, "_", dfp$exp, sep = "")

	dfp = lapply(unique(dfp$id), function(x) {
		df_sub = dfp[id == x, ]
		idx = which(df_sub$variant != "mbo" | doNondominatedSorting(t(as.matrix(df_sub[, c("eval.perf", "eval.propfeat")])))$ranks == 1)
		#idx = which(doNondominatedSorting(t(as.matrix(df_sub[, c("eval.perf", "eval.propfeat")])))$ranks == 1)
		df_sub[idx, ]
	})

	dfp = do.call(rbind, dfp)

	plist[[i]] = ggplot() + geom_point(data = dfp, aes(x = eval.perf, y = eval.propfeat))
	plist[[i]] = plist[[i]] + geom_point(data = dfp, aes(x = hout.perf, y = eval.propfeat), shape = 2)
	plist[[i]] = plist[[i]] + geom_segment(data = dfp, aes(x = eval.perf, xend = hout.perf, y = eval.propfeat, yend = eval.propfeat, colour = evals, lty = overestim))
	plist[[i]] = plist[[i]] + facet_grid(. ~ variant) + theme_bw()
	plist[[i]] 

	i = i + 1
}

p = do.call(grid.arrange, c(plist, ncol = 2, nrow = 2))


p = ggplot() + geom_point(data = df, aes(x = evals, y = eval.perf), color = "orange")
p = p + geom_point(data = df, aes(x = evals, y = hout.perf), color = "green")
p = p + theme_bw()
p = p + facet_grid( . ~ variant)



df[, mean(diff), by = c("variant")]






































# res = readRDS(paste(data_path, "/res.rds", sep = ""))
# parfrnt = readRDS(paste(data_path, "pareto_examples/paretofront.rds", sep = ""), )

# b) Create rank plots 

# --- mosmafs 
res_fil = res[algorithm == "mosmafs", ]
plotRanks(res_fil, plotspath, experiments_dt = experiments_dt[algorithm == "mosmafs", ], metric =  "eval.domHV", prompt = c("mosmafs"), limits = c(0.2, 1))#, height = 8, width = 7)


# ---. best vs baselines








# Show what performed best
res_mbo = res[algorithm == "no_feature_sel", ]
res_mbo = rbind(res_mbo, res[variant %in% c("OIHFiFmS", "O"), ])

# see paper Fig. 2
plotRanks(res_mbo, plotspath, metric = "eval.domHV", limits = c(0.2, 1))#, height = 8, width = 7)

# see paper Appendix Fig. 4
plotRanks(res_mosmafs, plotspath, metric = "naive.hout.domHV", limits = c(0.2, 1))#, height = 8, width = 7)

# c) Create table with Evaluations to Randomsearch 
# see paper Table 3
calculateEvalsToRandomsearch(res, path = latex_path)

# --- TODO
# d) Create table with Evaluations to MBO 


# e) Create a summary of methods after
# - 4000 evaluations, see table 2 and Appendix table 7
calculateSummaryOfMethods(latex_path, res, maxevals = 4000L) 
# - 2000 evaluations, see Appendix table 8
calculateSummaryOfMethods(latex_path, res, maxevals = 2000L) 


# --- TODO
# e) Create summary in a single-objective manner, i.e. only w.r.t. mmce


# --- TODO: add MBO in this table 
# f) Paretofronts for every problem
# see Fig. 1, and Appendix Fig. 5, 6, 7
plotFrontPerProblem(plotspath, parfrnt)


# --- TODO
# g) Create a plot for runtime evaluation
# see plot in b), but just with runtime as x-axis
# for randomsearch, you have to create runtime artificially 


# --- TODO
# h) Create a table for runtime evaluation (see Table 3)
# "runtime to randomsearch "



p = ggplot(data = op) + geom_point(aes(x = y_1, y = y_2, col = filter))
p










#--------------------------------

res2 <- copy(res)

res2 <-  problems[res2, on = "problem"]

res2[, overfit := sapply(result, function(re) {
	c(tail(re$eval.domHV - re$naive.hout.domHV, 1), -2)[1]
})]

res2[, overfit := sapply(result, function(re) {
	c(tail(re$eval.domHV - re$true.hout.domHV, 1), -2)[1]
})]


res2$result <- NULL

mosmafsres <- res2[experiments_list$OIHFiFmS, on = colnames(experiments_list$OIHFiFmS)]
mbores <- res2[experiments_list$BSMO, on = colnames(experiments_list$BSMO)]

ftbl <- rbind(mosmafsres, mbores)

ftblmean <- ftbl[, list(overfit = mean(overfit)), by = c("n", "algorithm", "problem", "p")]

ggplot(ftbl, aes(x = problem, y = overfit, color = algorithm)) +
  geom_boxplot()

ggplot(ftbl, aes(x = 1 / n, y = overfit, color = algorithm)) + geom_point() + geom_smooth(method = "lm")
ggplot(ftbl, aes(x = log(n), y = overfit, color = algorithm)) + geom_point() + geom_smooth(method = "lm")

ggplot(ftbl, aes(x = p, y = overfit, color = algorithm)) + geom_point() + geom_smooth(method = "lm")
ggplot(ftbl, aes(x = log(p), y = overfit, color = algorithm)) + geom_point() + geom_smooth(method = "lm")


ggplot(ftblmean, aes(x = n, y = overfit, color = algorithm)) + geom_point()
ggplot(ftblmean, aes(x = 1/n, y = overfit, color = algorithm)) + geom_point()


str(mosmafsres)

names(res$result[[1]])
names(res)

summary(lm(overfit ~ I(1 / n), data = ftbl[algorithm == "mosmafs", ]))
summary(lm(overfit ~ I(log(n)), data = ftbl[algorithm == "mosmafs", ]))

summary(lm(overfit ~ I(1 / n) * algorithm, data = ftbl))


summary(lm(overfit ~ I(1/p), data = ftbl[algorithm == "mosmafs", ]))
summary(lm(overfit ~ I(log(p)), data = ftbl[algorithm == "mosmafs", ]))

summary(lm(I(problems$p) ~ I(1/problems$n)))