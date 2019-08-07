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
	O = data.table(algorithm = "mosmafs", filter = "none", initialization = "none", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, multi.objective = NA, parent.sel = "selTournamentMO"),
	OI = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, multi.objective = NA, parent.sel = "selTournamentMO"),
	OIFi = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, multi.objective = NA, parent.sel = "selTournamentMO"),
	OIFiFm = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = TRUE, multi.objective = NA, parent.sel = "selTournamentMO"),
	OIFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = NA, parent.sel = "selTournamentMO"),
	OIH = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = FALSE, filter.during.run = FALSE, multi.objective = NA, parent.sel = "selTournamentMO"),
	OIHFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE, multi.objective = NA, parent.sel = "selTournamentMO"),
	RS = data.table(algorithm = "randomsearch", initialization = "none", filter = "none", chw.bitflip = NA, adaptive.filter.weights = NA, filter.during.run = NA),
	RSI = data.table(algorithm = "randomsearch", initialization = "unif", filter = "none", chw.bitflip = NA, adaptive.filter.weights = NA, filter.during.run = NA),
	RSIF = data.table(algorithm = "randomsearch", initialization = "unif", filter = "custom", chw.bitflip = NA, adaptive.filter.weights = NA, filter.during.run = NA),
	BS1RF = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = FALSE, surrogate = "randomForest", infill = "cb", propose.points = 15L),
	BS2RF = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = TRUE, surrogate = "randomForest", infill = "cb", propose.points = 15L), 
	BSMO = data.table(algorithm = "mbo_multicrit", filter = "custom", surrogate = "randomForest", infill = "cb", propose.points = 15L)
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
files = lapply(files_loc, readRDS)
res_full = rbindlist(files, fill = TRUE)

# --- Filtering, as not all experiments are completed yet
res = res_full[learner != "xgboost", ]
res = res[variant %in% experiments$variant, ]
res = res[, count := .N, by = c("problem")]
res = res[count == 260, ]

# --- Analyze mosmafs
limits = list(c(0, 1), c(0.5, 1), c(0.8, 1), c(0.85, 0.95))

# --- a) inner evaluation
for (lim in limits) {
	plotRanks(res = res, plotspath = plotspath, 
	experiments = experiments[algorithm == "mosmafs", ], 
	metric =  "eval.domHV", prompt = c("mosmafs"), limits = lim)#, height = 8, width = 7)
}

# --- b) outer evaluation
for (lim in limits) {
	plotRanks(res = res, plotspath = plotspath, 
	experiments = experiments[algorithm == "mosmafs", ], 
	metric =  "naive.hout.domHV", prompt = c("mosmafs"), limits = lim)#, height = 8, width = 7)
}

# --- c) outer evaluation (true hout domHV)
for (lim in limits) {
	plotRanks(res = res, plotspath = plotspath, 
	experiments = experiments[algorithm == "mosmafs", ], 
	metric =  "true.hout.domHV", prompt = c("mosmafs"), limits = lim)#, height = 8, width = 7)
}

# --- Analyze the best 2 of mosmafs against randomsearch baselines 

# --- a) inner evaluation
for (lim in limits) {
	plotRanks(res = res, plotspath = plotspath, 
	experiments = experiments[variant %in% c("O", "OIH", "OIHFiFmS", "RS", "RSI", "RSIF"), ], 
	metric =  "eval.domHV", prompt = c("baselines_rs"), limits = lim)#, height = 8, width = 7)
}

# --- b) outer evaluation
for (lim in limits) {
	plotRanks(res = res, plotspath = plotspath, 
	experiments = experiments[variant %in% c("O", "OIH", "OIHFiFmS", "RS", "RSI", "RSIF"), ], 
	metric =  "naive.hout.domHV", prompt = c("baselines_rs"), limits = lim)#, height = 8, width = 7)
}

# --- c) outer evaluation (true hout domHV)
for (lim in limits) {
	plotRanks(res = res, plotspath = plotspath, 
	experiments =  experiments[variant %in% c("O", "OIH", "OIHFiFmS", "RS", "RSI", "RSIF"), ], 
	metric =  "true.hout.domHV", prompt = c("baselines_rs"), limits = lim)#, height = 8, width = 7)
}


# --- Analyze the best 2 of mosmafs against baselines MBO

# --- a) inner evaluation
for (lim in limits) {
	plotRanks(res = res, plotspath = plotspath, 
	experiments = experiments[variant %in% c("O", "OIH", "OIHFiFmS", "BS1RF", "BS2RF", "BSMO"), ], 
	metric =  "eval.domHV", prompt = c("baselines_mbo"), limits = lim)#, height = 8, width = 7)
}

# --- b) outer evaluation
for (lim in limits) {
	plotRanks(res = res, plotspath = plotspath, 
	experiments = experiments[variant %in% c("O", "OIH", "OIHFiFmS", "BS1RF", "BS2RF", "BSMO"), ], 
	metric =  "naive.hout.domHV", prompt = c("baselines_mbo"), limits = lim)#, height = 8, width = 7)
}

# --- c) outer evaluation (true hout domHV)
for (lim in limits) {
	plotRanks(res = res, plotspath = plotspath, 
	experiments =  experiments[variant %in% c("O", "OIH", "OIHFiFmS", "BS1RF", "BS2RF", "BSMO"), ], 
	metric =  "true.hout.domHV", prompt = c("baselines_mbo"), limits = lim)#, height = 8, width = 7)
}


# --- 4. Debugging: Visualize single results
res = readRDS(file.path(data_path, "single_experiments_hill-valley.rds"))

# get the first 100 points 

dfl = list()

for (i in 1:3) {
	exp_mosmafs = res[i, ]$result[[1]]$result
	fitnesses = popAggregate(exp_mosmafs$log, "fitness")
	hofitnesses = popAggregate(exp_mosmafs$log, "fitness.holdout")

	df_mbo = data.table(data.frame(res[i + 3, ]$result[[1]]$result$opt.path))
	names(df_mbo)[c(5, 6, 18, 19)] = c("eval.perf", "eval.propfeat", "hout.perf", "hout.propfeat")
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
	dfl[[i]] = rbind(df_mbo, df_mosmafs)

	dfl[[i]]$diff = dfl[[i]]$hout.perf - dfl[[i]]$eval.perf

	dfl[[i]]$exp = i
}


df = rbindlist(dfl, fill = TRUE)
df$overestim = df$diff > 0


p = ggplot() + geom_histogram(data = df[evals <= maxevals, ], aes(diff, fill = variant))
p = p + facet_grid(variant ~ exp)
p

p = ggplot() + geom_histogram(data = df[evals <= maxevals, ], aes( eval.propfeat, fill = variant))
p = p + facet_grid(variant ~ exp) + ggtitle("Number of features")

dfr = df[, .(eval=seq_along(eval.perf), meanperf=cummean(eval.perf), meanhout = cummean(hout.perf)), by = c("variant", "exp")]

p = ggplot() + geom_line(data = dfr[eval <= 4000, ], aes(x = eval, y = meanperf, color = variant))
p = p + geom_line(data = dfr[eval <= 4000, ], aes(x = eval, y = meanhout, color = variant), lty = 2)
p = p + facet_grid(. ~ exp) + ggtitle("Number of features")
p

plist = list()
i = 1

for (maxgen in c(0, 100, 200, 250)){

	maxevals = maxgen * 15 + 80

	# FOR MOSMAFS, ONLY PLOT CURRENT GENERATION 
	dfp = df[evals <= maxevals & exp == 2, ]
	dfp = dfp[variant == "mbo" | (variant == "mosmafs" & gen == maxgen)]
	dfp$id = paste(dfp$variant, "_", dfp$exp, sep = "")

	dfp = lapply(unique(dfp$id), function(x) {
		df_sub = dfp[id == x, ]
		idx = which(doNondominatedSorting(t(as.matrix(df_sub[, c("eval.perf", "eval.propfeat")])))$ranks == 1)
		df_sub[idx, ]
	})

	dfp = do.call(rbind, dfp)

	plist[[i]] = ggplot() + geom_point(data = dfp, aes(x = eval.perf, y = eval.propfeat))
	plist[[i]] = plist[[i]] + geom_point(data = dfp, aes(x = hout.perf, y = eval.propfeat), shape = 2)
	plist[[i]] = plist[[i]] + geom_segment(data = dfp, aes(x = eval.perf, xend = hout.perf, y = eval.propfeat, yend = eval.propfeat, colour = evals, lty = overestim))
	plist[[i]] = plist[[i]] + facet_grid(. ~ variant) 
	plist[[i]]

	i = i + 1
}

p = do.call(grid.arrange, c(plist, ncol = 2, nrow = 2))

sapply(1:1000, mean(df[evals < mx, by = ]))

# p = ggplot() + geom_point(data = df, aes(x = evals, y = eval.perf), color = "orange")
# p = p + geom_point(data = df, aes(x = evals, y = hout.perf), color = "green")
# p = p + theme_bw()
# p = p + facet_grid( . ~ variant)



# df[, mean(diff), by = c("variant")]






































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
