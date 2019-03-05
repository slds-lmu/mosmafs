library(batchtools)
library(stringi)
stri_detect_fixed = stringi::stri_detect_fixed

reg = loadRegistry("registry", writeable = TRUE)
tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "initialization", "mu", "lambda", "maxeval", "filter.method", "learner", "parent.sel", "feature.mut"))

# test some things before start 
# test madelon task
# testjob = tab[problem == "madelon" & filter.method == "none" & feature.mut == "mutBitflip", ][1, ]
# testjob$maxeval = 100L
# submitJobs(testjob$job.id, resources = list(ncpus = 28L, 
# 	walltime = 60 * 30L, memory.limit = 1024L * 2L,
# 	clusters = "mpp2" # get name from lrz homepage
# 	)
# )

tosubmit = ijoin(tab, findNotDone(), by = "job.id")
tosubmit = tosubmit[!is.na(feature.mut), ]

tosubmit = tosubmit[learner == "kknn" & job.id > 370, ]

submitJobs(tosubmit[, c("job.id")], resources = list( 
	walltime = 3600 * 5L, memory.limit = 1024L * 2L,
	clusters = "serial" # get name from lrz homepage
	)
)

tosubmit = tosubmit[learner == "SVM" & job.id > 370, ]
chunk.size = 10L
nchunks = nrow(tosubmit) / chunk.size
tosubmit$chunk = rep(1:nchunks, each = chunk.size)

submitJobs(tosubmit[, c("job.id", "chunk")], resources = list(ncpus = 28L, 
	walltime = 3600 * 7L, memory.limit = 1024L * 2L,
	clusters = "mpp2" # get name from lrz homepage
	)
)

