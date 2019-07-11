# ---
# SUBMISSION SCRIPT 
# ---

library(batchtools)
library(stringi)
library(dplyr)

# --- 1. Load Registry and Metadata
reg = loadRegistry("registry", writeable = TRUE)
tab = summarizeExperiments(
	by = c("job.id", "algorithm", "problem", "learner", "maxeval", "cv.iters", "filter", "initialization", 
	"lambda", "mu", "parent.sel", "chw.bitflip", "adaptive.filter.weights", "filter.during.run")
	)

source("probdesign.R")

resources.serial = list(
	walltime = 3600L * 48L, memory = 1024L * 2L,
	clusters = "serial", max.concurrent.jobs = 250L # get name from lrz homepage)
)

resources.serial.doublemem = list(
	walltime = 3600L * 48L, memory = 1024L * 4L,
	clusters = "serial", max.concurrent.jobs = 250L # get name from lrz homepage)
)


# --- 2. Experiments that will be run 

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
	RSIF = data.table(algorithm = "randomsearch", initialization = "unif", filter = "custom", chw.bitflip = NA, adaptive.filter.weights = NA, filter.during.run = NA)
	)

# b) Datasets
problems.serial = c("wdbc", "ionosphere", "sonar", "hill-valley", "clean1", 
	"tecator", "semeion", "lsvt", "isolet", "cnae-9")

# --- SUBMITTING STATUS
# --- RS      |   doing 	 |  290 / 300 DONE ( USPS - xgb missing)
# --- RSI     |   doing    	 |  290 / 300 DONE ( USPS - xgb missing)
# --- RSIF    |   doing    	 |  290 / 300 DONE ( USPS - xgb missing)
# --- O       |   doing      |    0 / 300 DONE 
# --- OI      |   doing      |    0 / 300 DONE 

experiment = "OIFi"
tosubmit = ijoin(tab, experiments[[experiment]], by = names(experiments[[experiment]]))
tosubmit = ijoin(tosubmit, findNotDone())
tosubmit = tosubmit[problem %in% problems.serial, ]
tosubmit = tosubmit[- which(problem == "USPS"), ]
tosubmit = tosubmit[mu != 3, ]
tosubmit = tosubmit[- which(job.id %in% findOnSystem()$job.id), ]
chunk.size = 5L
tosubmit$chunk = 1
nchunks = nrow(tosubmit) / chunk.size
tosubmit$chunk = rep(1:nchunks, each = chunk.size)

submitJobs(tosubmit, resources = resources.serial)

tosubmit = ijoin(tab, experiments[[experiment]], by = names(experiments[[experiment]]))
tosubmit = ijoin(tosubmit, findNotDone())
tosubmit = tosubmit[problem == "USPS", ]
tosubmit = tosubmit[mu != 3, ]

submitJobs(tosubmit, resources = resources.serial.doublemem)




toremove = tab[mu == 3, ]
removeExperiments(ids = toremove$job.id, reg = reg)



# done = ijoin(tab, findDone())
# done = done[- which(job.id == 1463), ]
# done = done[- which(job.id == 1467), ]
# done = done[- which(job.id == 1621), ]
# res = reduceResultsDataTable(done, function(x) x$runtime[3])
# runtime = ijoin(done, res)
# runtime$result = unlist(runtime$result)
# runtime$result.min = runtime$result / 60
# runtime$result.hr = runtime$result.min / 60


# resources.ivymuc = list(ncpus = 15L, 
# 	walltime = 3600L * 72L, memory = 1024L * 50L,
# 	clusters = "ivymuc") # get name from lrz homepage))

# resources.teramem = list(ncpus = 15L,
# 	walltime = 3600L * 48L, memory = 1024L * 200L,
# 	clusters = "inter",
# 	partition = "teramem_inter") # get name from lrz homepage))

# resources.mpp3 = list(ncpus = 15L,
# 	walltime = 3600L * 48L, memory = 1000L * 50L,
# 	clusters = "mpp3") # get name from lrz homepage))

# resources.mpp2 = list(ncpus = 15L,
# 	walltime = 3600L * 48L, memory = 1000L * 50L,
# 	clusters = "mpp2") # get name from lrz homepage))


