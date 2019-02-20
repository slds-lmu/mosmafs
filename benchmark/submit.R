library(batchtools)
library(stringi)
stri_detect_fixed = stringi::stri_detect_fixed

reg = loadRegistry("registry", writeable = TRUE)
tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "initialization", "mu", "lambda", "maxeval", "filter.method", "learner", "parent.sel"))

tosubmit = ijoin(tab, findNotDone(), by = "job.id")

# reg$cluster.functions = makeClusterFunctionsSocket()

submitJobs(tosubmit$job.id, resources = list(
	walltime = 3600 * 10L, memory.limit = 1024L * 6L)
)

