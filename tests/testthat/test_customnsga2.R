context("customnsga2")

test_that("slickEcr, initEcr, continueEcr", {
  ps.simple <- pSS(
    a: numeric [0, 10],
    selector.selection: logical^10)
  
  mutator.simple <- combine.operators(ps.simple,
    a = mutGauss,
    selector.selection = mutBitflipCHW)
  
  crossover.simple <- combine.operators(ps.simple,
    a = recSBX,
    selector.selection = recPCrossover)
  
  initials <- sampleValues(ps.simple, 30, discrete.names = TRUE)
  
  fitness.fun <- smoof::makeMultiObjectiveFunction(
    sprintf("simple test"),
    has.simple.signature = FALSE, par.set = ps.simple, n.objectives = 2, 
    noisy = TRUE,
    ref.point = c(10, 1),
    fn = function(args, fidelity = NULL, holdout = FALSE) {
      pfeat <- mean(args$selector.selection)
      c(perform = args$a, pfeat = pfeat)
    })
  
  fitness.fun.single <- smoof::makeMultiObjectiveFunction(
    sprintf("simple test"),
    has.simple.signature = FALSE, par.set = ps.simple, n.objectives = 1, 
    noisy = TRUE,
    ref.point = c(10),
    fn = function(args, fidelity = NULL, holdout = FALSE) {
      propfeat <- mean(args$selector.selection)
      c(propfeat = propfeat)
    })
  
  generations <- 10
  lambda <- 10
  
  results <- slickEcr(fitness.fun = fitness.fun, lambda = lambda, population = initials, 
    mutator = mutator.simple, recombinator = crossover.simple, generations = 10)
  
  results.single <- slickEcr(fitness.fun.single, lambda = lambda, 
    population = initials, mutator = mutator.simple, recombinator = crossover.simple, 
    generations = generations, 
    parent.selector = selSimple, 
    survival.selector = selTournament)
  
  
  results.single.comma <- slickEcr(fitness.fun.single, lambda = lambda, 
    population = initials, mutator = mutator.simple, recombinator = crossover.simple, 
    generations = generations, 
    parent.selector = selSimple, 
    survival.selector = selTournament, 
    survival.strategy = "comma", n.elite = 10)
  
  # output initEcr 
  init.result <- initEcr(fitness.fun = fitness.fun.single, 
    population = initials) 
  init.statistics = getStatistics(init.result$log)
  
  expect_class(init.result, "MosmafsResult")
  expect_data_frame(getStatistics(init.result$log), any.missing = FALSE, 
    nrows = 1)
  expect_equal(init.statistics$gen, 0)
  expect_list(init.result$last.population)
  
  # output slickEcr
  statistics <- getStatistics(results$log)
  expect_equal(statistics$gen, 0:generations)
  expect_equal(statistics$state[1], "init")
  expect_equal(statistics$state[-1], rep("generation", generations))
  expect_equal(length(results$pareto.set), nrow(results$pareto.front))
  expect_class(results, c("MosmafsResult"))
  expect_equal(length(results$last.population), length(initials))
  expect_list(results$pareto.set)
  expect_data_frame(results$pareto.front, types = numeric(0L), 
    any.missing = FALSE)
  for(nam in grep("(fitness.obj.[123].min)|(fitness.domHV)", 
    colnames(statistics), value = TRUE)) {
    expect_numeric(statistics[, nam])
  }
  statistics.single <- getStatistics(results.single.comma$log)
  for(nam in grep("(fitness.obj.[123].min)", 
    colnames(statistics), value = TRUE)) {
    expect_numeric(statistics[, nam])
  }
  expect_null(statistics.single$fitness.domHV)
  expect_list(results.single$best.x, null.ok = FALSE)
  expect_numeric(results.single$best.y, null.ok = FALSE)
  
  expect_error(slickEcr(fitness.fun = fitness.fun.single, lambda = lambda, 
    population = initials, mutator = mutator.simple, 
    recombinator = crossover.simple, generations = generations), 
    "survival.selector does not support single-objective fitness")
  
  expect_error(slickEcr(fitness.fun = fitness.fun.single, lambda = lambda, 
    population = initials, mutator = mutator.simple, 
    parent.selector = selTournamentMO,
    recombinator = crossover.simple, generations = generations), 
    "parent.selector does not support single-objective fitness")
  
  expect_error(slickEcr(fitness.fun = print, lambda = lambda, 
    population = initials, mutator = mutator.simple, 
    recombinator = crossover.simple, generations = generations), 
    "fitness.fun must be a SMOOF function")
  
  expect_error(initEcr(fitness.fun = print, population = initials), 
    "fitness.fun must be a SMOOF function")
  
  # continueEcr for single objective 
  cont.result <- continueEcr(ecr.object = init.result, generations = 10, 
    lambda = 10, mutator = mutator.simple, recombinator = crossover.simple, 
    parent.selector = selSimple,
    survival.selector = selTournament, 
    p.recomb = 0.7, p.mut = 0.3,
    survival.strategy = "plus", n.elite = 0, fidelity = NULL,
    unbiased.fidelity = TRUE)
  expect_data_frame(getStatistics(cont.result$log), nrows = 11)
  expect_error(continueEcr(ecr.object = init.result, generations = 10, 
    mutator = mutator.simple, recombinator = crossover.simple, 
    parent.selector = selSimple,
    survival.selector = selTournament, 
    p.recomb = 0.7, p.mut = 0.3,
    survival.strategy = "plus", n.elite = 0, fidelity = NULL,
    unbiased.fidelity = TRUE), 
    "lambda is not given")
  # multifidelity not in init defined, but now in continue
  fidelity <- data.frame(
    c(1, 6, 10),
    c(1, 3, 5))
  expect_error(continueEcr(ecr.object = init.result, generations = 10, 
    lambda = 10, mutator = mutator.simple, recombinator = crossover.simple, 
    parent.selector = selSimple,
    survival.selector = selTournament, 
    p.recomb = 0.7, p.mut = 0.3,
    survival.strategy = "plus", n.elite = 0, fidelity = fidelity,
    unbiased.fidelity = TRUE), 
    "Can't use multifidelity when ecr.object was initialized without multifidelity")
  # no last fidelity given 
  init_result2 <- initEcr(fitness.fun = fitness.fun.single, 
    population = initials, fidelity = fidelity)
  init_result2$last.fidelity = NULL
  expect_error(continueEcr(ecr.object = init_result2, generations = 10, 
    lambda = 10, mutator = mutator.simple, recombinator = crossover.simple, 
    parent.selector = selSimple,
    survival.selector = selTournament, 
    p.recomb = 0.7, p.mut = 0.3,
    survival.strategy = "plus", n.elite = 0, fidelity = fidelity,
    unbiased.fidelity = TRUE), 
    "Inconsistent ecr.object: 'last.fidelity' not given, but 'fidelity' is.")

})


test_that("multiFidelity with 2 columns", {
  task.whole <- mlr::bh.task
  rows.whole <- sample(1:nrow(getTaskData(task.whole)))
  task <- subsetTask(task.whole, rows.whole[1:250])
  task.hout <- subsetTask(task.whole, rows.whole[250:505])
  
  lrn <- makeLearner("regr.lm")
  
  ps.simple <- pSS(
    a: numeric [0, 10])
  
  nRes <- function(n) {
    makeResampleDesc("Subsample", split = 0.9, iters = n)
  }
  
  fitness.fun <- makeObjective(learner = lrn, task = task, ps = ps.simple, 
    resampling = nRes, holdout.data = task.hout, worst.measure = .Machine$double.xmax)
  
  ps.simple <- getParamSet(fitness.fun)
  
  initials <- sampleValues(ps.simple, 15, discrete.names = TRUE)
  
  fidelity <- data.frame(
    c(1, 6, 10),
    c(1, 3, 5))
  
  mutator.simple <- combine.operators(ps.simple,
    numeric = ecr::setup(mutGauss, sdev = 0.1),
    selector.selection = mutBitflipCHW)
  
  crossover.simple <- combine.operators(ps.simple,
    numeric = recPCrossover,
    selector.selection = recPCrossover)
  
  gen = 10
  
  results.mufi <- slickEcr(
    fitness.fun = fitness.fun,
    lambda = 5,
    population = initials,
    mutator = mutator.simple,
    recombinator = crossover.simple,
    generations = gen,
    fidelity = fidelity)
  statistics.mufi <- getStatistics(results.mufi$log)
  
  expect_data_frame(results.mufi$fidelity, null.ok = FALSE)
  expect_equal(results.mufi$last.fidelity, fidelity[nrow(fidelity), ncol(fidelity)])
  expect_equal(statistics.mufi$gen, 0:gen)
  expect_equal(statistics.mufi$state[1], "init")
  expect_equal(statistics.mufi$state[-1], rep("generation", gen))
  expect_equal(length(results.mufi$pareto.set), nrow(results.mufi$pareto.front))
  expect_class(results.mufi, c("MosmafsResult"))
  expect_equal(length(results.mufi$last.population), length(initials))
  
  
  fidelity <- data.frame(
    c(1, 6),
    c(1, 3))
  
  results.mufi2 <- slickEcr(
    fitness.fun = fitness.fun,
    lambda = 5,
    population = initials,
    mutator = mutator.simple,
    recombinator = crossover.simple,
    generations = gen,
    fidelity = fidelity)
  statistics.mufi2 <- getStatistics(results.mufi$log)
  
  expect_equal(results.mufi2$last.fidelity, fidelity[nrow(fidelity), ncol(fidelity)])
  expect_equal(length(results.mufi2$pareto.set), nrow(results.mufi2$pareto.front))
  expect_class(results.mufi2, c("MosmafsResult"))
  expect_equal(length(results.mufi2$last.population), length(initials))
  
  expect_error(slickEcr(fitness.fun = fitness.fun,
    lambda = 5,population = initials, mutator = mutator.simple,
    recombinator = crossover.simple, generations = gen, fidelity = 
      data.frame(c(1, 2), c("a", 3))), 
    "Must be of type 'numeric', not 'factor'")
  
  expect_error(slickEcr(fitness.fun = fitness.fun, lambda = 5,population = initials, mutator = mutator.simple,
    recombinator = crossover.simple, generations = gen, fidelity = 
      data.frame(c(1, 2))), 
    "'fidelity' failed: Must have at least 2 cols")

}) 

test_that("survival.strategy as function works", {
  ps.simple <- pSS(
    a: numeric [0, 10], 
    selector.selection: logical^7)
  
  mutator.simple <- combine.operators(ps.simple,
    a = mutGauss,
    selector.selection = mutBitflipCHW)
  
  crossover.simple <- combine.operators(ps.simple,
    a = recSBX,
    selector.selection = recPCrossover)
  
  initials <- sampleValues(ps.simple, 30, discrete.names = TRUE)
  
  fitness.fun <- smoof::makeMultiObjectiveFunction(
    sprintf("simple test"),
    has.simple.signature = FALSE, par.set = ps.simple, n.objectives = 2, 
    noisy = TRUE,
    ref.point = c(10, 1),
    fn = function(args, fidelity = NULL, holdout = FALSE) {
      propfeat <- mean(args$selector.selection)
      c(perf = args$a, propfeat = propfeat)
    })
  
  survival.strategy <- function(control, population, offspring, fitness, fitness.offspring) {
    merged.pop <- c(population, offspring)
    merged.fit <- cbind(fitness, fitness.offspring)
    fitness <- ecr:::transformFitness(merged.fit, control$task, 
      control$selectForSurvival)
    # only select by one objective
    surv.idx <- order(fitness[1,])[1:length(population)]

    fitness = merged.fit[, surv.idx, drop = FALSE]
    fitness = makeFitnessMatrix(fitness, control)
    return(list(population = merged.pop[surv.idx], fitness = fitness))
  }
  
  
  generations <- 10
  lambda <- 10
  expect_class(results <- slickEcr(fitness.fun = fitness.fun, lambda = lambda, 
    population = initials, 
    mutator = mutator.simple, recombinator = crossover.simple, 
    survival.strategy = survival.strategy, generations = generations), 
    "MosmafsResult")
  expect_list(results$pareto.set, min.len = 1)
  expect_true(all(diff(getStatistics(results$log)$fitness.obj.1.min) <= 0))

})

test_that("vectorized fitness evaluation", {
  ps.simple <- pSS(
    a: numeric [0, 10],
    selector.selection: logical^10)
  
  mutator.simple <- combine.operators(ps.simple,
    a = mutGauss,
    selector.selection = mutBitflipCHW)
  
  crossover.simple <- combine.operators(ps.simple,
    a = recSBX,
    selector.selection = recPCrossover)
  
  initials <- sampleValues(ps.simple, 30, discrete.names = TRUE)
  
  fitness.fun <- smoof::makeMultiObjectiveFunction(
    sprintf("simple test"),
    has.simple.signature = FALSE, par.set = ps.simple, n.objectives = 2, 
    noisy = TRUE,
    ref.point = c(10, 1),
    fn = function(args, fidelity = NULL) {
      propfeat <- rowMeans(args[, grepl("selector.selection", names(args))])
      fitness = mapply(function(propfeat, perform) {
        c(propfeat, perform)
      }, propfeat, args$a)
  })
  fitness.fun <- setMosmafsVectorized(fitness.fun)

  generations <- 10
  lambda <- 10
  expect_class(results <- slickEcr(fitness.fun = fitness.fun, lambda = lambda, 
    population = initials, 
    mutator = mutator.simple, recombinator = crossover.simple, generations = generations), 
    "MosmafsResult")
  stats <- collectResult(results)
  expect_true(all(diff(stats$eval.obj.1.min) <= 0))
  expect_true(all(diff(stats$eval.obj.2.min) <= 0))
  expect_numeric(diff(stats$eval.domHV), lower = 0-1e-5)
  expect_list(results$pareto.set, min.len = 1)
  
  
  # only one objective
  fitness.fun <- smoof::makeMultiObjectiveFunction(
    sprintf("simple test"),
    has.simple.signature = FALSE, par.set = ps.simple, n.objectives = 1, 
    noisy = TRUE,
    ref.point = c(1),
    fn = function(args, fidelity = NULL) {
      propfeat <- rowMeans(args[, grepl("selector.selection", names(args))])
    })
  fitness.fun <- setMosmafsVectorized(fitness.fun)
  
  generations <- 10
  lambda <- 10
  expect_class(results <- slickEcr(fitness.fun = fitness.fun, lambda = lambda, 
    population = initials, survival.selector = selGreedy,
    mutator = mutator.simple, recombinator = crossover.simple, generations = generations), 
    "MosmafsResult")
  stats <- collectResult(results)
  expect_true(all(diff(stats$eval.min)<= 0))
  expect_data_frame(stats, nrows = 11)
  
})

test_that("fidelity with 3 columns", {
  task.whole <- mlr::bh.task
  rows.whole <- sample(1:nrow(getTaskData(task.whole)))
  task <- subsetTask(task.whole, rows.whole[1:250])
  task.hout <- subsetTask(task.whole, rows.whole[250:505])
  
  lrn <- makeLearner("regr.lm")
  
  ps.simple <- pSS(
    a: numeric [0, 10])
  
  nRes <- function(n) {
    makeResampleDesc("Subsample", split = 0.9, iters = n)
  }
  
  fitness.fun <- makeObjective(learner = lrn, task = task, ps = ps.simple, 
    resampling = nRes, holdout.data = task.hout, worst.measure = .Machine$double.xmax)
  
  ps.simple <- getParamSet(fitness.fun)
  
  initials <- sampleValues(ps.simple, 15, discrete.names = TRUE)
  
  fidelity <- data.frame(
    c(1, 6, 10),
    c(1, 3, 5), 
    c(1, 5, 7))
  
  mutator.simple <- combine.operators(ps.simple,
    numeric = ecr::setup(mutGauss, sdev = 0.1),
    selector.selection = mutBitflipCHW)
  
  crossover.simple <- combine.operators(ps.simple,
    numeric = recPCrossover,
    selector.selection = recPCrossover)
  
  gen = 11
  
  results.mufi <- slickEcr(
    fitness.fun = fitness.fun,
    lambda = 5,
    population = initials,
    mutator = mutator.simple,
    recombinator = crossover.simple,
    generations = gen,
    fidelity = fidelity)
  statistics.mufi <- getStatistics(results.mufi$log)
  

  expect_equal(length(results.mufi$pareto.set), nrow(results.mufi$pareto.front))
  expect_class(results.mufi, c("MosmafsResult"))
  expect_equal(length(results.mufi$last.population), length(initials))
  
})

test_that("maximization", {
  ps.simple <- pSS(
    a: numeric [0, 10])
  
  mutator.simple <- combine.operators(ps.simple,
    a = mutGauss)
  
  crossover.simple <- combine.operators(ps.simple,
    a = recSBX)
  
  set.seed(10)
  initials <- sampleValues(ps.simple, 30, discrete.names = TRUE)
  lambda <- 10
  
  # Smoof function with argument minimize = FALSE
  fitness.fun <- smoof::makeMultiObjectiveFunction(
    sprintf("simple test"),
    has.simple.signature = FALSE, par.set = ps.simple, n.objectives = 2, 
    noisy = TRUE, ref.point = c(10, 1), minimize = c(FALSE, TRUE),
    fn = function(args, fidelity = NULL, holdout = FALSE) {
      pfeat <- mean(args$a)
      c(perform = args$a, pfeat = pfeat)
    })
  
  expect_error(slickEcr(fitness.fun = fitness.fun, lambda = lambda, population = initials, 
    mutator = mutator.simple, recombinator = crossover.simple, generations = 10), 
    "maximization not supported yet")
  
  # maximization with negative objective
  fitness.fun.single <- smoof::makeSingleObjectiveFunction(
    sprintf("simple test"),
    has.simple.signature = FALSE, par.set = ps.simple, 
    noisy = TRUE,
    fn = function(args, fidelity = NULL, holdout = FALSE) {
      c(perform = -args$a)
    })

  set.seed(10)
  results.single <- slickEcr(fitness.fun.single, lambda = lambda, 
    population = initials, mutator = mutator.simple, recombinator = crossover.simple, 
    generations = 50, p.mut = 0.5,
    parent.selector = selSimple, 
    survival.selector = selTournament)
  
  expect_equal(results.single$best.x[[1]]$a, 10, tolerance = 0.3)
  
})






