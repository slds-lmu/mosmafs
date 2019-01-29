library(batchtools)
library("magrittr")

source("def.R")
source("../datagen.R")
source("../ecrshims.R")
source("../selectorcpo.R")
source("../customnsga2.R")
source("../operators.R")


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


for (i in seq_along(task.type)) {

  id = names(task.type)[i]
  data = task.type[i]

  addProblem(reg = reg, name = id, data = data, fun = function(data, job, p.inf, p.noise, n) {
    dataset = data[[1]](p.inf, n)
    task = dataset %>% create.classif.task(id = names(data)) %>% task.add.random.cols(num = p.noise)
     list(task = task)
  })
}


mosmafs = function(data, job, instance, learner, lambda, mu, maxeval) {

  dataset = create.hypersphere.data(4, 100) 
  task = dataset %>% create.classif.task(id = "test") %>% task.add.random.cols(num = 10)
  learner = "SVM"

  # task, learner and parameter set
  task = instance$task
  lrn = LEARNERS[[learner]]
  resinst = makeResampleInstance(makeResampleDesc("CV", iters = 10, stratify = TRUE), task = task) 
  
  # --- parameter set ---
  ps = PAR.SETS[[learner]]
  ps = c(ps, pSS(selector.selection: logical^getTaskNFeats(task)))
  
  # --- create operators ---
  mutator = combine.operators(ps,
    numeric = mutGauss,
    logical = mutBitflip,
    integer = mutUniformInt,
    selector.selection = mutBitflip
  )

  crossover = combine.operators(ps,
    numeric = recSBX,
    integer = recIntSBX,
    logical = recUnifCrossover)

  # --- create fitness function ---
  fitness.fun = function(args) {
    args = args[intersect(names(args), getParamIds(getParamSet(lrn)))]
    val = resample(setHyperPars(lrn, par.vals = args), task, resinst, show.info = FALSE)$aggr
    propfeat = mean(args$selector)
    c(val, propfeat)
  }

  initials = sampleValues(ps, mu, discrete.names = TRUE)

  results = my.nsga2(
    fitness.fun = fitness.fun, n.objectives = 2L, minimize = TRUE,
    mu = mu, lambda = lambda,
    mutator = mutator, recombinator = crossover,
    representation = "custom",
    initial.solutions = initials,
    log.pop = TRUE,
    terminators = list(stopOnIters(15)))

  return(results)
}

addAlgorithm(name = "mosmafs", reg = reg, fun = mosmafs)

addExperiments(reg = reg, 
  prob.designs = pdes, 
  algo.designs = list(mosmafs = ades),
  repls = REPLICATIONS)

# tab = summarizeExperiments(by = c("job.id", "problem", "p.inf", "p.noise", "n"))





