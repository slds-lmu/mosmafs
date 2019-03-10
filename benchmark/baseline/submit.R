library(batchtools)
library(stringi)
stri_detect_fixed = stringi::stri_detect_fixed

reg = loadRegistry("registry", writeable = TRUE)
tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "initialization", "maxeval", "filter", "learner", "surrogate"))

tosubmit = ijoin(tab, findNotDone(), by = "job.id")
tosubmit = tosubmit[- which(tosubmit$surrogate == "km.nugget"), ]
tosubmit = tosubmit[maxeval == 4000L, ]

# tosubmit = tosubmit[filter = "selDomHV", ]
# reg$cluster.functions = makeClusterFunctionsSocket()

# chunk.size = 5L
# nchunks = nrow(tosubmit) / chunk.size
# tosubmit$chunk = rep(1:nchunks, each = chunk.size)

submitJobs(tosubmit, resources = list(
	walltime = 3600 * 10L, memory = 1024L * 2L,
	clusters = "serial" # get name from lrz homepage
	)
)

