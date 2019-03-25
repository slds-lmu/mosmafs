library(batchtools)
library(stringi)
library(dplyr)

resources.serial = list(
	walltime = 3600L * 48L, memory = 1024L * 2L,
	clusters = "serial" # get name from lrz homepage)
)

resources.ivymuc = list(ncpus = 5L,
	walltime = 3600L * 48L, memory = 1024L * 9L,
	clusters = "ivymuc") # get name from lrz homepage))

resources.teramem = list(ncpus = 5L,
	walltime = 3600L * 48L, memory = 1024L * 60L,
	clusters = "inter",
	partition = "teramem_inter") # get name from lrz homepage))

resources.mpp3 = list(ncpus = 5L,
	walltime = 3600L * 48L, memory = 1024L * 9L,
	clusters = "mpp3") # get name from lrz homepage))


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
	"madeline", "gina_agnostic", "madelon", "dilbert", "eating", "philippine",
	"lsvt", "semeion", "isolet")

problems.hugemem = c("gisette", "Bioresponse")

problems.dortmund =  setdiff(setdiff(datasets, problems.serial), problems.hugemem)

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
experiment = "OIFiFm"
tosubmit = ijoin(tab, experiments[[experiment]], by = names(experiments[[experiment]]))
tosubmit = tosubmit[problem %in% problems.serial, ]
# tosubmit = tosubmit[, chunk := chunk(job.id, chunk.size = 10)
# nchunks = nrow(tosubmit) / chunk.size
tosubmit = ijoin(tosubmit, findNotDone())
tosubmit = tosubmit[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit, resources = resources.serial)

# --- LRZ hugemem ---  

# Bioresponse, gisette only 
experiment = "O"
tosubmit = ijoin(tab, experiments[[experiment]], by = names(experiments[[experiment]]))
tosubmit = tosubmit[problem %in% problems.hugemem, ]
nchunks = nrow(tosubmit) / chunk.size
tosubmit$chunk = rep(1:nchunks, each = chunk.size)
tosubmit = tosubmit[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit, resources = resources.teramem)


# --- LIDO ---  
# memory is per node here! 
resources.lido = list(walltime = 3600L * 1L, memory = 1024L * 16L)
# O submitted
# OI 
# OIFi 
# OIFiFm .
# OIFiFmS 
# OIH 
# RS 
# RSI 
# RSIF 
problems.dortmund = "philippine"
experiment = "OIFi"
tosubmit = ijoin(tab, experiments[[experiment]], by = names(experiments[[experiment]]))
tosubmit = tosubmit[problem %in% problems.dortmund, ]
tosubmit = ijoin(tosubmit, findNotDone(), by = "job.id")
tosubmit = tosubmit[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(tosubmit, resources = resources.lido)