library(batchtools)
library(stringi)
stri_detect_fixed = stringi::stri_detect_fixed

reg = loadRegistry("registry", writeable = TRUE)
tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "mu", "lambda", "maxeval", "p.inf", "p.noise", "n", "filter.method"))

tosubmit = ijoin(tab, findNotDone(), by = "job.id")
# tosubmit = tosubmit[- which(job.id %in% findOnSystem()$job.id), ]

# reg$cluster.functions = makeClusterFunctionsSocket()

submitJobs(tosubmit$job.id, resources = list(
	walltime = 3600 * 6L, memory.limit = 1024L * 2L)
)

submitJobs()

