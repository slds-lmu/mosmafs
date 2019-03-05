library(batchtools)
library(stringi)
stri_detect_fixed = stringi::stri_detect_fixed

reg = loadRegistry("registry2", writeable = TRUE)
tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "initialization", "mu", "lambda", "maxeval", "filter.method", "learner", "parent.sel"))

tosubmit = ijoin(tab, findExpired(), by = "job.id")
tosubmit = tosubmit[parent.sel == "selDomHV", ]
# reg$cluster.functions = makeClusterFunctionsSocket()

chunk.size = 5L
nchunks = nrow(tosubmit) / chunk.size
tosubmit$chunk = rep(1:nchunks, each = chunk.size)

submitJobs(tosubmit[, c("job.id")][24], resources = list(
	walltime = 3600 * 5L, memory.limit = 1024L * 5L,
	clusters = "serial" # get name from lrz homepage
	)
)

