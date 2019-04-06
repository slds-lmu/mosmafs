library(batchtools)
library(dplyr)
library(mlr)
library(mlrCPO)

source("helpers.R")
source("probdesign.R")

# load registry
reg = loadRegistry("registry22", writeable = FALSE)
tab = summarizeExperiments(by = c("job.id", "algorithm", 
	"problem", "learner", "maxeval", "filter", "initialization", 
	"lambda", "mu", "parent.sel", "chw.bitflip", "adaptive.filter.weights",
	"filter.during.run"))
tab = tab[learner == "xgboost", ]
# tab = tab[maxeval %in% c(4000), ]
# tab = rbind(tab[lambda != 4L, ], tab[is.na(lambda), ])
done = ijoin(tab, findDone())

path = "results_raw_xgboost"
dir.create(path)

problems = c("wdbc", "ionosphere", "sonar", "hill-valley", "clean1", 
  "tecator", "USPS", "madeline", "lsvt", "madelon", "isolet", "cnae-9")

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

collectBenchmarkResults(path, experiments, tab)
collectParetofront(path, experiments = experiments[c("O", "OIHFiFmS", "RS", "RSI", "RSIF")], tab, problems, learners = c("xgboost"))