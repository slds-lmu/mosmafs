library(batchtools)

source("def.R")

if (file.exists("registry")) {
  if (OVERWRITE) {
    unlink("registry", recursive = TRUE)
    reg = makeExperimentRegistry(seed = 123L,
      packages = c("mlr", "ecr"), source = "def.R")
  } else {
    reg = loadRegistry("registry", writeable = TRUE)
  }
} else {
  reg = makeExperimentRegistry(seed = 123L,
    packages = c("mlr", "ecr"), source = "def.R")
}


# generate the problems
for (i in seq_along(TASK_IDS)) {

	id = names(TASK_IDS)[i]
	task = getOMLTask(TASK_IDS[[i]])

	# do we have a dynamic part here? 
    addProblem(reg = reg, name = id, data = task)
}


featuresel.tune = function(data, job, instance, ...) {

	# need to specify what is going on here...

}




