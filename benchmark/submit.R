library(batchtools)
library(stringi)
library(dplyr)

resources.serial = list(
	walltime = 3600L * 48L, memory = 1024L * 4L,
	clusters = "serial" # get name from lrz homepage)
)

resources.ivymuc = list(ncpus = 15L, 
	walltime = 3600L * 72L, memory = 1024L * 50L,
	clusters = "ivymuc") # get name from lrz homepage))

resources.teramem = list(ncpus = 15L,
	walltime = 3600L * 48L, memory = 1024L * 200L,
	clusters = "inter",
	partition = "teramem_inter") # get name from lrz homepage))

resources.mpp3 = list(ncpus = 15L,
	walltime = 3600L * 48L, memory = 1000L * 50L,
	clusters = "mpp3") # get name from lrz homepage))

resources.mpp2 = list(ncpus = 15L,
	walltime = 3600L * 48L, memory = 1000L * 50L,
	clusters = "mpp2") # get name from lrz homepage))


reg = loadRegistry("registry", writeable = TRUE)
tab = summarizeExperiments(by = c("job.id", "algorithm", 
	"problem", "learner", "maxeval", "filter", "initialization", 
	"lambda", "mu", "parent.sel", "chw.bitflip", "adaptive.filter.weights",
	"filter.during.run"))
tab = tab[maxeval %in% c(2000, 4000), ]
tab = rbind(tab[lambda != 4L, ], tab[is.na(lambda), ])

source("probdesign.R")

chunk.size = 10L

problems.serial = c("sonar", "ionosphere", "hill-valley", "wdbc", "tecator", 
	"madeline", "gina_agnostic", "madelon", "lsvt", "isolet",
	"cnae-9", "clean1", "USPS")

problems.serial2 = c()

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

# --- LRZ ---  

# O done
# OI done
# OIFi submitted
# OIFiFm submitted
# OIFiFmS submitted
# OIH done
# RS done
# RSI done
# RSIF submitted
experiment = "RSIF"
tosubmit = ijoin(tab, experiments[[experiment]], by = names(experiments[[experiment]]))
tosubmit = tosubmit[problem %in% problems.serial, ]
done = ijoin(tosubmit, findDone())
df = done[, replication := 1:length(job.id), by = c("learner", "problem")]

status_finished = df[, max(replication), by = c("problem", "learner")]
status_finished = status_finished[, sum(V1), by = c("problem", "learner")]
status_finished
# tosubmit = tosubmit[, chunk := chunk(job.id, chunk.size = 10)
# nchunks = nrow(tosubmit) / chunk.size
tosubmit = ijoin(tosubmit, findNotDone())
ijoin(tosubmit, findOnSystem())
tosubmit = tosubmit[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit, resources = resources.serial)

# --- LRZ ivymuc ---  

# run on ivymuc: 
# USPS, clean1, 

prob = "madelon"
tosubmit = tab[problem %in% prob, ]
tosubmit = ijoin(experiments2, tosubmit)
done = ijoin(tosubmit, findDone())
df = done[, replication := 1:length(job.id), by = c("learner", "problem", "variant")]

status_finished = df[, max(replication), by = c("problem", "learner", "variant")]
status_finished = status_finished[, sum(V1), by = c("problem", "learner", "variant")]
status_finished
tosubmit = ijoin(tosubmit, findNotDone())
ijoin(tosubmit, findOnSystem())
tosubmit = tosubmit[- which(job.id %in% findOnSystem()$job.id), ]
chunk.size = 7L
tosubmit$chunk = 1
nchunks = nrow(tosubmit) / chunk.size

tosubmit$chunk = rep(1:nchunks, each = chunk.size)

submitJobs(tosubmit, resources = resources.mpp2)
