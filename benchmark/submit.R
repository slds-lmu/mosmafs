library(batchtools)
library(stringi)
library(dplyr)

resources.serial = list(
	walltime = 3600L * 48L, memory = 1024L * 2L,
	clusters = "serial", max.concurrent.jobs = 300L # get name from lrz homepage)
)


# Submit Random Search 
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

experiments2 = do.call("rbind", experiments)
experiments2$variant = names(experiments)

reg = loadRegistry("registry", writeable = TRUE)
tab = summarizeExperiments(by = c("job.id", "algorithm", 
	"problem", "learner", "maxeval", "filter", "initialization", 
	"lambda", "mu", "parent.sel", "chw.bitflip", "adaptive.filter.weights",
	"filter.during.run"))

source("probdesign.R")

# --- no madelon / no madeline 
problems.serial = c("wdbc", "ionosphere", "sonar", "hill-valley", "clean1", 
	"tecator", "USPS", "lsvt", "isolet", "cnae-9")

# --- SUBMITTING STATUS
# --- RS 
# --- RSI
# --- RSIF
experiment = "RSIF"
tosubmit = ijoin(tab, experiments[[experiment]], by = names(experiments[[experiment]]))
tosubmit = ijoin(tosubmit, findNotDone())
tosubmit = tosubmit[problem %in% problems.serial, ]
tosubmit = tosubmit[- which(job.id %in% findOnSystem()$job.id), ]
chunk.size = 3L
tosubmit$chunk = 1
nchunks = nrow(tosubmit) / chunk.size
tosubmit$chunk = rep(1:nchunks, each = chunk.size)

# done = ijoin(tosubmit, findDone())

submitJobs(tosubmit, resources = resources.serial)

# df = done[, replication := 1:length(job.id), by = c("learner", "problem")]

# status_finished = df[, max(replication), by = c("problem", "learner")]
# status_finished = status_finished[, sum(V1), by = c("problem", "learner")]
# status_finished




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


