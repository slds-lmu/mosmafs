library(batchtools)
library(stringi)
stri_detect_fixed = stringi::stri_detect_fixed

reg = loadRegistry("registry", writeable = TRUE)
tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "initialization", "mu", "lambda", "maxeval", "filter.method", "learner", "parent.sel"))

tosubmit = ijoin(tab, findNotDone(), by = "job.id")
tosubmit = tosubmit[- which(problem == "gisette" | problem == "ionosphere" | problem == "sonar" | problem == "madelon"), ]
tosubmit = tosubmit[which(maxeval == 10000), ]
# reg$cluster.functions = makeClusterFunctionsSocket()

tosubmit$chunk = rep(1:120, each = 10)

submitJobs(tosubmit[1:20, c("job.id", "chunk")], resources = list(ncpus = 28L, 
	walltime = 3600 * 10L, memory.limit = 1024L * 2L,
	clusters = "mpp2" # get name from lrz homepage
	)
)

