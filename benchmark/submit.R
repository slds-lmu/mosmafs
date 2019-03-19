library(batchtools)
library(stringi)
library(dplyr)

resources.serial = list(
	walltime = 3600L * 48L, memory = 1024L * 2L,
	clusters = "serial" # get name from lrz homepage)
)

resources.mpp3 = list(ncpus = 15L,
	walltime = 3600L * 48L, memory = 1024L * 20L,
	clusters = "mpp3") # get name from lrz homepage))

reg = loadRegistry("registry", writeable = TRUE)
tab = summarizeExperiments(by = c("job.id", "algorithm", 
	"problem", "learner", "maxeval", "filter", "initialization", 
	"lambda", "mu", "parent.sel", "chw.bitflip", "adaptive.filter.weights",
	"filter.during.run", "surrogate", "MBMOmethod", "propose.points"))
tab = tab[maxeval %in% c(2000, 4000), ]

source("probdesign.R")

chunk.size = 10L


# submit mosmafs / mbo without filter
# probs x lrns x initilization x chw.bitflip 
# 5 x 3 x 2 x 2 x 10 = 600
i = 1
datasets[i]
tosubmit = tab[algorithm %in% c("randomsearch"), ]
# tosubmit = tosubmit[problem %in% datasets[i], ]
tosubmit = tosubmit[mu == 80, ]
notdone = ijoin(tosubmit, findNotDone(), by = "job.id")
notdone = notdone[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(notdone, resources = resources.serial)


# submit MBObaseline
# 12 x 3 x 10 = 360
tosubmit = tab[problem %in% c("Bioresponse", "gisette", "dexter", "AP_Lung_Uterus"), ]
notdone = tosubmit[lambda != 4L, ]
nchunks = floor(nrow(notdone) / chunk.size)
notdone$chunk = rep(1:nchunks, each = chunk.size)
notdone = notdone[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(notdone[31:338, ], resources = resources.mpp3)

# submit randomsearch without filter
# probs x lrns x initilization x chw.bitflip 
# 5 x 3 x 2 x 10 = 300
# tosubmit = tab[algorithm == "randomsearch", ]
# notdone = ijoin(tosubmit, findNotDone(), by = "job.id")
# # notdone = notdone[- which(job.id %in% findOnSystem()$job.id), ]
# submitJobs(notdone, resources = resources.serial)

# random search on serial





