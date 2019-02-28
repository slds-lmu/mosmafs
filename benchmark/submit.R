library(batchtools)
library(stringi)
stri_detect_fixed = stringi::stri_detect_fixed

reg = loadRegistry("registry", writeable = TRUE)
tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "initialization", "mu", "lambda", "maxeval", "filter.method", "learner", "parent.sel"))

tosubmit = ijoin(tab, findErrors(), by = "job.id")
tosubmit = tosubmit[parent.sel == "selDomHV", ]
# reg$cluster.functions = makeClusterFunctionsSocket()

chunk.size = 5L
nchunks = nrow(tosubmit) / chunk.size
tosubmit$chunk = rep(1:nchunks, each = chunk.size)

submitJobs(tosubmit[, c("job.id", "chunk")], resources = list(ncpus = 28L, 
	walltime = 3600 * 5L, memory.limit = 1024L * 2L,
	clusters = "mpp2" # get name from lrz homepage
	)
)

