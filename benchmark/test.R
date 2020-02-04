# --- TESTS IF BENCHMARK IS CONFIGURED CORRECTLY

library(batchtools)

reg = loadRegistry("registry_temp", writeable = TRUE)

testdata = "sonar"

tab = summarizeExperiments(by = c("job.id", "algorithm", 
	"problem", "learner", "maxeval", "filter", "initialization", 
	"lambda", "mu", "parent.sel", "chw.bitflip", "adaptive.filter.weights",
	"filter.during.run", "surrogate", "infill", "propose.points", "maxtime", 
  "multi.objective", "tune.hyperparams", "tune.iters", "ensemble"))

tab = tab[problem %in% testdata, ]

# --- TEST RANDOMSEARCH

tosubmit = tab[algorithm %in% "randomsearch", ]

# uniform vs. non-uniform 
binom = testJob(tosubmit[initialization == "none", ][1, ])
unif = testJob(tosubmit[initialization == "unif", ][1, ])
geom = testJob(tosubmit[initialization == "geom", ][1, ])

sapply(binom$result$last.population, function (x) mean(x$selector.selection))
sapply(unif$result$last.population, function (x) mean(x$selector.selection))
	
# filter initialization
res = testJob(tosubmit[filter == "testfilter", ][1, ]) # filter 
# expect 1 for sonar task
mean(sapply(res$result$last.population, function (x) x$selector.selection[1]))

# filter + nonuniform initialization 
res = testJob(tosubmit[filter == "testfilter" & initialization == "unif", ][1, ])
sapply(res$result$last.population, function (x) mean(x$selector.selection))
mean(sapply(res$result$last.population, function (x) x$selector.selection[1]))


# --- TEST MBO

# pure MBO without feature selection
tosubmit = tab[algorithm %in% "no_feature_sel", ]
res = testJob(tosubmit[filter.during.run == TRUE, ][1, ])
res = testJob(tosubmit[filter.during.run == FALSE, ][1, ])


# --- TEST MOSMAFS
tosubmit = tab[algorithm %in% "mosmafs", ]

# uniform vs. non-uniform 
binom = testJob(tosubmit[initialization == "none" & tune.hyperparams == TRUE & tune.iters == 0 & multi.objective == TRUE, ][1, ])
sapply(binom$result$last.population, function (x) mean(x$selector.selection))

unif = testJob(tosubmit[initialization == "unif" & tune.hyperparams == TRUE & tune.iters == 0 & multi.objective == TRUE, ][1, ])
sapply(unif$result$last.population, function (x) mean(x$selector.selection))

# --- test single objective
singleobj = testJob(tosubmit[initialization == "unif" & tune.hyperparams == TRUE & tune.iters == 0 & multi.objective == FALSE & parent.sel == "selTournament", ][1, ])


# --- TEST MBO
tosubmit = tab[algorithm == "mbo_multicrit", ]
res = testJob(tosubmit[adaptive.filter.weights == TRUE, ][1, ])
res = testJob(tosubmit[adaptive.filter.weights == FALSE, ][1, ])

# --- TEST GEOM INIT
tosubmit = tab[algorithm == "mosmafs" & initialization == "geom", ]
res = testJob(tosubmit[2, ])



# --- TEST MOSMAFS WITH WARMSTART
tosubmit = tab[algorithm %in% "mosmafs" & chw.bitflip == TRUE & initialization == "geom" & tune.iters == 500L & tune.hyperparams == TRUE, ]
res = testJob(tosubmit[1, ])


# --- TEST MOSMAFS WITH PRETUNED HYPERPARAMS
tosubmit = tab[algorithm %in% "mosmafs" & chw.bitflip == TRUE & initialization == "geom" & tune.iters == 500L & tune.hyperparams == FALSE, ]
res = testJob(tosubmit[1, ])



# --- TEST MBO WITH FILTER ENSEMBLE
tosubmit = tab[algorithm == "no_feature_sel" & filter.during.run == TRUE & ensemble == TRUE, ]
res = testJob(tosubmit[1, ])
