library(batchtools)
library(stringi)
stri_detect_fixed = stringi::stri_detect_fixed

reg = loadRegistry("registry", writeable = TRUE)
tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "maxeval", "p.inf", "p.noise", "n", "filter.method"))

submitJobs(c(1, 950))


reduceResultsList(1)


getIndividuals = function(ecr_result) {
	lapply(getPopulations(ecr_result$log), function(x) )
}

