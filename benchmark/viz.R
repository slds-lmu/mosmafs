library(data.table)
library(ggplot2)
library(batchtools)
library(ecr)
library(gridExtra)
library(Hmisc)
library(dplyr)
library(plyr)

source("helpers.R")

experiments = list(
	O = data.table(algorithm = "mosmafs", filter = "none", initialization = "none", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OI = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OIFi = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OIFiFm = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = TRUE),
	OIFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = TRUE, filter.during.run = TRUE),
	OIH = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OIHFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE),
	RS = data.table(algorithm = "randomsearch", initialization = "none", filter = "none"),
	RSI = data.table(algorithm = "randomsearch", initialization = "unif", filter = "none"),
	RSIF = data.table(algorithm = "randomsearch", initialization = "unif", filter = "custom")
	)

# no madelon 
# no madeline 
problems = data.table(problem = c(
	"wdbc", "ionosphere", "sonar", "hill-valley", "clean1", 
	"tecator", "USPS", "madeline", "lsvt", "madelon", "isolet", "cnae-9"),
	n = c(569, 351, 208, 1212, 476, 240, 1424, 3140, 126, 2600, 600, 240),
	p = c(30, 34, 60, 100, 168, 124, 256, 259, 310, 500, 617, 856))

problems = problems[- which(problem %in% c("madeline", "madelon"))]

# savepath
plotspath = "results_plots"
latex_path = "latex_temp"

# --- read the data ---
datapath = "results_raw"
learners = c("SVM", "kknn")
reslist = lapply(names(experiments), function(x) readRDS(file.path(datapath, x, "result.rds")))
res = do.call("rbind", reslist)
res = res[problem %in% problems$problem & learner %in% learners, ]
res$learner = factor(res$learner, levels = learners)
res$surrogate = NULL
res$MBMOmethod = NULL
res$propose.points = NULL

datapath = "results_raw_xgboost"
learners = c("xgboost")
reslist = lapply(names(experiments), function(x) readRDS(file.path(datapath, x, "result.rds")))
res_xgb = do.call("rbind", reslist)
res_xgb = res_xgb[problem %in% problems$problem & learner %in% learners, ]
res_xgb$learner = factor(res_xgb$learner, levels = learners)
res_xgb$job.id = res_xgb$job.id + 100000L

res = rbind(res, res_xgb)

# --- opt.paths per task per learner --- 
plotRanks(res, plotspath, metric = "eval.domHV", limits = c(0.2, 1))#, height = 8, width = 7)
plotRanks(res, plotspath, metric = "naive.hout.domHV", limits = c(0.2, 1))#, height = 8, width = 7)
calculateEvalsToRandomsearch(res, path = latex_path)
calculateSummaryOfMethods(res, maxevals = 4000L) 
calculateSummaryOfMethods(res, maxevals = 2000L) 


parfrnt = readRDS(paste("results_raw/pareto_examples/paretofront.rds", sep = ""))
parfrnt$surrogate = NULL
parfrnt$MBMOmethod = NULL
parfrnt$propose.points = NULL
parfrnt_xgb = readRDS(paste("results_raw_xgboost/pareto_examples/paretofront.rds", sep = ""))
parfrnt = rbind(parfrnt, parfrnt_xgb)

for (prob in problems$problem) {

	allparetos = parfrnt[problem == prob, ]
	allparetos = allparetos[expname %in% c("O", "OIHFiFmS", "RS", "RSI", "RSIF"), ]

	allparetos$expname = revalue(allparetos$expname, 
	    c("O" = "NSGAII", "OI" = "NSGAII+UI", "OIFi" = "NSGAII+UI+FI", "OIFiFm" = "NSGAII+UI+FI+FM", 
	        "OIFiFmS" = "NSGAII+UI+FI+FM(s.a.)", "OIH" = "NSGAII+UI+HP", "OIHFiFmS" = "NSGAII+UI+FI+HP+FM(s.a.)", 
	        "RS" = "RS", "RSI" = "RS+UI", "RSIF" = "RS+UI+IF"))
	allparetos$expname = factor(allparetos$expname, levels = c("RS", "RS+UI", "RS+UI+IF", "NSGAII", "NSGAII+UI+FI+HP+FM(s.a.)"))
	allparetos$learner = factor(allparetos$learner, levels = c("SVM", "kknn", "xgboost"))
	
	p = ggplot(allparetos, aes(x = mmce, y = featfrac, group = instance)) 
	p = p + geom_polygon(data = allparetos, fill = "grey", alpha = 0.05) 
	p = p + geom_line(data = allparetos, colour = "grey", alpha = 0.6) 
	p = p + geom_point(data = allparetos[point == TRUE], color = "#386cb0", alpha = 0.4)
	p = p + scale_colour_Publication() + theme_Publication() + scale_fill_Publication()
	# p = p + ylab("Value") + labs(colour = "Variant", lty = "Algorithm") 
	p = p + labs(colour = "", fill = "")
	p = p + facet_grid(expname ~ learner) 
	p = p + xlim(c(0, 1)) + ylim(c(0, 1)) + coord_fixed()
	p = p + xlab(expression(mmce[outer])) + ylab("Fraction of features selected")
	p = p + theme(legend.position = "none")
	# p = p + theme(
	#   strip.background = element_blank(),
	#   strip.text.x = element_blank(),
	#   strip.text.y = element_blank(),
	#   legend.position = "right", legend.direction = "vertical", legend.box = "vertical")
	# p = p + guides(colour = guide_legend(override.aes = list(size = 4, alpha = 0.6)))

	ggsave(file.path(plotspath, "front", paste("all_variants", "_", prob, ".pdf", sep = "")), p, width = 9, height = 12, device = "pdf")
}
