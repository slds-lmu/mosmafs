# ---
# BENCHMARK MOSMAFS
# ---


# ---
# 0. Load libraries
# ---

# TODO: packrat

packages = c("batchtools", "data.table", "ecr", "mlr", "mlrCPO", "mlrMBO", "mosmafs", 
  "magrittr", "parallelMap", "ParamHelpers", "packrat")

lapply(packages, library, character.only = TRUE)


# ---
# 1. Setup envorinoment (TEST / NO TEST) + load registry
# ---

TEST = TRUE

if (TEST) {
  deffile = "def_test.R"
  registry_name = "registry_temp2"
} else {
  deffile = "def.R"
  registry_name = "registry"
}

source(deffile)

if (TEST) {
    unlink(registry_name, recursive = TRUE)
    reg = makeExperimentRegistry(file.dir = registry_name, seed = 123L,
      packages = packages, source = deffile)
} else {
  if (file.exists(registry_name)) {
    if (OVERWRITE) {
      unlink(registry_name, recursive = TRUE)
      reg = makeExperimentRegistry(file.dir = registry_name, seed = 123L,
        packages = packages, source = deffile)
    } else {
      reg = loadRegistry(registry_name, writeable = TRUE)
    }
  } else {
      reg = makeExperimentRegistry(file.dir = registry_name, seed = 123L,
        packages = packages, source = deffile)
    }
}


# ---
# 2. Create problems
#     - for each dataset we store the path
#     - dataset + resampling instance = problem
# ---

readDataAndRinst = function(data, job, rinst.iter, ...) {
  task = readRDS(file.path(data, "task.rds"))
  rin = readRDS(file.path(data, "rin.rds"))

  train.task = subsetTask(task, rin$train.inds[[rinst.iter]])
  test.task = subsetTask(task, rin$test.inds[[rinst.iter]])
  
  list(train.task = train.task, test.task = test.task)
}

for (i in 1:length(datasets)) {  
  addProblem(
    name = datasets[i], 
    data = paste(datafolder, datasets[i], sep = "/"), 
    fun = readDataAndRinst,
    reg = reg
    )
}

# ---
# 3. Add algorithms
# ---

source("algorithms/randomsearch.R")
addAlgorithm(name = "randomsearch", reg = reg, fun = randomsearch)

source("algorithms/mbobaselines.R")
addAlgorithm(name = "no_feature_sel", reg = reg, fun = no_feature_sel)

source("algorithms/mosmafs.R")
addAlgorithm(name = "mosmafs", reg = reg, fun = mosmafs)


# ---
# 4. Add Experiments
# ---

addExperiments(reg = reg, 
  prob.designs = pdes,
  algo.designs = list(
  randomsearch = ades.random, 
  no_feature_sel = ades.no_feature_sel,
  mosmafs = ades.mosmafs), 
  repls = REPLICATIONS)
