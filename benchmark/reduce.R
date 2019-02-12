library(batchtools)

source("def.R")

# load registry
reg = loadRegistry("registry")

tab = summarizeExperiments(by = c("job.id", "problem", "p.inf", "p.noise", "n", "filter.method"))

# reduce done jobs 
res = reduceResultsDataTable(ijoin(tab, findDone(), by = "job.id")$job.id)
pars = unwrap(getJobPars())
res = ijoin(res, pars, by = "job.id")
res = ijoin(tab, res, by = "job.id")

saveRDS(res, "registry/res.rds")



