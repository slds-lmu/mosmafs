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
      propfeat <- mean(args$selector.selection)
      c(perf = args$a, propfeat = propfeat)
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
  init.result = initEcr(fitness.fun = fitness.fun.single, 
    population = initials) 
  init.statistics = getStatistics(init.result$log)
  
  expect_class(init.result, "MosmafsResult")
  expect_data_frame(getStatistics(init.result$log), any.missing = FALSE, 
    nrows = 1)
  expect_equal(init.statistics$gen, 0)
  expect_list(init.result$last.population)
  
  # output slickEcr/continueEcr
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

})


test_that("multiFidelity operators work", {
  ### TO DO
  
}) 
  