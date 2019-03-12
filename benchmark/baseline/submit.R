library(batchtools)
library(stringi)
library(dplyr)

resources.serial = list(
	walltime = 3600L * 48L, memory = 1024L * 2L,
	clusters = "serial" # get name from lrz homepage)
)

reg = loadRegistry("registry", writeable = TRUE)
tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "initialization", "maxeval", "parent.sel", "feature.mut", 
	"filter", "learner", "surrogate", "lambda", "mu", "propose.points"))
tab = tab[- which(problem %in% c("convex", "dexter")), ]
tab = tab[filter %in% c("none", "custom"), ]

# check random search
tosubmit = tab[algorithm == "randomsearch" & maxeval == 4000L, ]
done = ijoin(tosubmit, findDone(), by = "job.id")
status = done %>% group_by(learner, problem, filter, initialization, maxeval) %>% summarize(n := length(algorithm))
status %>% filter(problem == "madelon")

notdone = ijoin(tosubmit, findNotDone(), by = "job.id")
notdone = notdone[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(notdone, resources = resources.serial)

# check MBO baseline / MBO with 2000 evals only
tosubmit = tab[algorithm == "MBObaseline" & propose.points == 10L & maxeval == 2000L, ]
done = ijoin(tosubmit, findDone(), by = "job.id")
status = done %>% group_by(learner, problem, filter, initialization, maxeval) %>% summarize(length(algorithm))

notdone = ijoin(tosubmit, findNotDone(), by = "job.id")
notdone = notdone[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(notdone, resources = resources.serial)


# check mosmafs
tosubmit = tab[algorithm == "mosmafs" & maxeval == 4000L & is.na(surrogate), ]
done = ijoin(tosubmit, findDone(), by = "job.id")
status = done %>% group_by(learner, problem, filter, initialization, maxeval, feature.mut) %>% summarize(length(algorithm))
status %>% filter(problem == "madelon")

notdone = ijoin(tosubmit, findNotDone(), by = "job.id")
notdone = notdone[- which(job.id %in% findOnSystem()$job.id), ]
submitJobs(notdone, resources = resources.serial)


# MISSING: MOSMAFS W/ FILTER MUTATION





# random search
# we do mosmafs and random search with 4000
# but mbo with 1000 only (will take to long otherwise)
# tosubmit = tosubmit[which(maxeval == 1000), ]
tosubmit = tosubmit[algorithm == "mosmafs", ]
tosubmit = tosubmit[is.na(surrogate), ]
tosubmit = tosubmit[maxeval == 4000, ]
# tosubmit = tosubmit[filter == "none", ]
# tosubmit = tosubmit[propose.points == 10, ]
tosubmit = ijoin(tosubmit, findNotDone(), by = "job.id")
tosubmit = tosubmit[- which(job.id %in% findOnSystem()$job.id), ]




ijoin(tosubmit, findDone())


# TODO: Remove Jobs that are not needed anymore

idx = tosubmit[(tosubmit$filter == "JMI_auc_var"), ]$job.id
removeJobs(idx)


# chunk.size = 5L
# nchunks = nrow(tosubmit) / chunk.size
# tosubmit$chunk = rep(1:nchunks, each = chunk.size)




