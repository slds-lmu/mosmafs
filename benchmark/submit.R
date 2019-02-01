library(batchtools)
library(stringi)

reg = loadRegistry("registry", writeable = TRUE)
tab = summarizeExperiments(by = c("job.id", "problem", "p.inf", "p.noise", "n", "filter.method"))

tosubmit = ijoin(tab, findNotDone(), by = "job.id")

# tosubmit = tosubmit[- which(job.id %in% findOnSystem()$job.id), ]

# reg$cluster.functions = makeClusterFunctionsSocket()

submitJobs(tosubmit$job.id)
