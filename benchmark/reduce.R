library(batchtools)

source("def.R")

# load registry
reg = loadRegistry("registry")
tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "initialization", "mu", "lambda", "maxeval", "p.inf", "p.noise", "n", "filter.method", "learner"))

# Extract all populations
runtime = reduceResultsDataTable(findDone(), function(x) x$runtime[[3]])
runtime = ijoin(tab, runtime, by = "job.id")
res = reduceResultsDataTable(findDone(), function(x) getIndividualsChromosomes(x$results))
res = ijoin(tab, res, by = "job.id")
res.fitnesses = reduceResultsDataTable(findDone(), function(x) fitnesses(x$results))
res.fitnesses = ijoin(tab, res.fitnesses, by = "job.id")

saveRDS(runtime, "runtime_test.rds")
saveRDS(res, "res_test.rds")
saveRDS(res.fitnesses, "res_fitnesses.rds")


