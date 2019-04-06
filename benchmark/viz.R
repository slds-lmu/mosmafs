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

# problems madelon and madeline did not run through
problems = problems[- which(problem %in% c("madeline", "madelon"))]

# savepath
plotspath = "results_plots"
latex_path = "latex_temp"
data_path = "results_raw"

# --- read the data ---
res = readRDS(paste(data_path, "/res.rds", sep = ""))
parfrnt = readRDS(paste(data_path, "pareto_examples/paretofront.rds", sep = ""), )

# --- create results --- 
plotRanks(res, plotspath, metric = "eval.domHV", limits = c(0.2, 1))#, height = 8, width = 7)
plotRanks(res, plotspath, metric = "naive.hout.domHV", limits = c(0.2, 1))#, height = 8, width = 7)
calculateEvalsToRandomsearch(res, path = latex_path)
calculateSummaryOfMethods(latex_path, res, maxevals = 4000L) 
calculateSummaryOfMethods(latex_path, res, maxevals = 2000L) 
plotFrontPerProblem(plotspath, parfrnt)
