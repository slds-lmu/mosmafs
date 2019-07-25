library(data.table)
library(ggplot2)
library(batchtools)
library(ecr)
library(gridExtra)
library(Hmisc)
library(dplyr)
library(plyr)

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
experiments = list(
	O = data.table(algorithm = "mosmafs", filter = "none", initialization = "none", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OI = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OIFi = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OIFiFm = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = TRUE),
	OIFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = TRUE, filter.during.run = TRUE),
	OIH = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OIHFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE),
	RS = data.table(algorithm = "randomsearch", initialization = "none", filter = "none", chw.bitflip = NA, adaptive.filter.weights = NA, filter.during.run = NA),
	RSI = data.table(algorithm = "randomsearch", initialization = "unif", filter = "none", chw.bitflip = NA, adaptive.filter.weights = NA, filter.during.run = NA),
	RSIF = data.table(algorithm = "randomsearch", initialization = "unif", filter = "custom", chw.bitflip = NA, adaptive.filter.weights = NA, filter.during.run = NA),
	BS1RF = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = FALSE, surrogate = "randomForest", infill = "cb"),
	BS2RF = data.table(algorithm = "no_feature_sel", filter = "custom", "filter.during.run" = TRUE, surrogate = "randomForest", infill = "cb")
	)

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

# a) Read the raw data
files_loc = list.files(data_path, recursive = TRUE, full.names = TRUE)
files = lapply(files_loc, readRDS)
res = do.call(rbind, files)

res = readRDS(paste(data_path, "/res.rds", sep = ""))
parfrnt = readRDS(paste(data_path, "pareto_examples/paretofront.rds", sep = ""), )


# b) Create rank plots 

# see paper Fig. 2
plotRanks(res, plotspath, metric = "eval.domHV", limits = c(0.2, 1))#, height = 8, width = 7)
# see paper Appendix Fig. 4
plotRanks(res, plotspath, metric = "naive.hout.domHV", limits = c(0.2, 1))#, height = 8, width = 7)

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
