# --- TESTS IF BENCHMARK IS CONFIGURED CORRECTLY

library(batchtools)

reg = loadRegistry("registry_temp", writeable = TRUE)

testdata = "sonar"

tab = summarizeExperiments(by = c("job.id", "algorithm", 
	"problem", "learner", "maxeval", "filter", "initialization", 
	"lambda", "mu", "parent.sel", "chw.bitflip", "adaptive.filter.weights",
	"filter.during.run"))
tab = tab[problem %in% testdata, ]


# --- TEST RANDOMSEARCH

tosubmit = tab[algorithm %in% "randomsearch", ]

# uniform vs. non-uniform 
binom = testJob(1)
unif = testJob(2)

sapply(binom$result$last.population, function (x) mean(x$selector.selection))
sapply(unif$result$last.population, function (x) mean(x$selector.selection))
	
# filter initialization
res = testJob(5)
mean(sapply(res$result$last.population, function (x) x$selector.selection[1]))

# filter + nonuniform initialization 
res = testJob(6)
sapply(res$result$last.population, function (x) mean(x$selector.selection))
mean(sapply(res$result$last.population, function (x) x$selector.selection[1]))

# --- TEST MBO?! 
tosubmit = tab[algorithm %in% "no_feature_sel", ]
res = testJob(tosubmit[1, ])



# --- TEST MOSMAFS
tosubmit = tab[algorithm %in% "mosmafs", ]

# uniform vs. non-uniform 
res = testJob(43)

