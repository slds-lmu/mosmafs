# --- TESTS IF BENCHMARK IS CONFIGURED CORRECTLY

library(batchtools)

reg = loadRegistry("registry_temp", writeable = TRUE)

testdata = "sonar"

tab = summarizeExperiments(by = c("job.id", "algorithm", 
	"problem", "learner", "maxeval", "filter", "initialization", 
	"lambda", "mu", "parent.sel", "chw.bitflip", "adaptive.filter.weights",
	"filter.during.run", "surrogate", "infill", "propose.points", "maxtime", 
  "multi.objective"))
tab = tab[problem %in% testdata, ]


# --- TEST RANDOMSEARCH

tosubmit = tab[algorithm %in% "randomsearch", ]

# uniform vs. non-uniform 
binom = testJob(tosubmit[1, ])
unif = testJob(tosubmit[2, ])

sapply(binom$result$last.population, function (x) mean(x$selector.selection))
sapply(unif$result$last.population, function (x) mean(x$selector.selection))
	
# filter initialization
res = testJob(tosubmit[5, ]) # filter 
# expect 1 for sonar task
mean(sapply(res$result$last.population, function (x) x$selector.selection[1]))

# filter + nonuniform initialization 
res = testJob(tosubmit[6, ])
sapply(res$result$last.population, function (x) mean(x$selector.selection))
mean(sapply(res$result$last.population, function (x) x$selector.selection[1]))

# --- TEST MBO?! 

# pure MBO without feature selection
tosubmit = tab[algorithm %in% "no_feature_sel", ]
res = testJob(tosubmit[1, ])
res = testJob(tosubmit[4, ])

res = testJob(tosubmit[2, ])


# --- TEST MOSMAFS
tosubmit = tab[algorithm %in% "mosmafs", ]

# uniform vs. non-uniform 
binom = testJob(61)
sapply(binom$result$last.population, function (x) mean(x$selector.selection))

unif = testJob(63)
sapply(unif$result$last.population, function (x) mean(x$selector.selection))


# --- test single objective
tosubmit = tab[multi.objective == FALSE & parent.sel == "selTournament", ]
res = testJob(tosubmit[1, ])
