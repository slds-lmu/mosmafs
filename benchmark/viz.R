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

problems = data.table(problem = c(
	"wdbc", "ionosphere", "sonar", "hill-valley", "clean1", 
	"tecator", "USPS", "madeline", "lsvt", "madelon", "isolet", "cnae-9"),
	n = c(569, 351, 208, 1212, 476, 240, 1424, 3140, 126, 2600, 600, 240),
	p = c(30, 34, 60, 100, 168, 124, 256, 259, 310, 500, 617, 856))

# --- read the data ---
datapath = "results_raw"
plotspath = "results_plots/plots_final"
reslist = lapply(names(experiments), function(x) readRDS(file.path(datapath, x, "result.rds")))
res = do.call("rbind", reslist)
res = res[problem %in% problems$problem, ]

poplist = lapply(names(experiments), function(x) readRDS(file.path(datapath, x, "population.rds")))
pops = do.call("rbind", poplist)
pops = pops[problem %in% problems$problem, ]

# --- opt.paths per task per learner --- 
plotRanks(res, plotspath, metric = "eval.domHV", limits = c(0.2, 1))
plotRanks(res, plotspath, metric = "naive.hout.domHV", limits = c(0.2, 1))
calculateEvalsToRandomsearch(res)
calculateSummaryOfMethods(res, maxevals = 4000L) {
calculateSummaryOfMethods(res, maxevals = 2000L) {


for (prob in problems$problem) {

	parfrnt = readRDS(paste("results_raw/pareto_examples/paretofront.rds", sep = ""))

	allparetos = parfrnt[problem == prob, ]

	allparetos$expname = revalue(allparetos$expname, 
	    c("O" = "NSGAII", "OI" = "NSGAII+UI", "OIFi" = "NSGAII+UI+FI", "OIFiFm" = "NSGAII+UI+FI+FM", 
	        "OIFiFmS" = "NSGAII+UI+FI+FM (s.a.)", "OIH" = "NSGAII+UI+HP", "OIHFiFmS" = "NSGAII+UI+FI+HP+FM(s.a.)", 
	        "RS" = "RS", "RSI" = "RS+UI", "RSIF" = "RS+UI+IF"))
	allparetos$expname = factor(allparetos$expname, levels = c("RS", "RS+UI", "RS+UI+IF", "NSGAII", "NSGAII+UI+FI+HP+FM(s.a.)"))
	allparetos$learner = factor(allparetos$learner, levels = c("SVM", "kknn"))
	
	p = ggplot(allparetos, aes(x = mmce, y = featfrac, group = instance)) 
	p = p + geom_polygon(data = allparetos, fill = "grey", alpha = 0.05) 
	p = p + geom_line(data = allparetos, colour = "grey", alpha = 0.6) 
	p = p + geom_point(data = allparetos[point == TRUE], aes(color = expname), alpha = 0.4)
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

	ggsave(file.path(plotspath, "plots_final", "front", paste("all_variants", "_", prob, ".pdf", sep = "")), p, width = 9, height = 12, device = "pdf")
}
