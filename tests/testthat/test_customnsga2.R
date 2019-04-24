context("customnsga2")

test_that("slickEcr", {
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
  
  fitness.fun.single<- smoof::makeMultiObjectiveFunction(
    sprintf("simple test"),
    has.simple.signature = FALSE, par.set = ps.simple, n.objectives = 1, 
    noisy = TRUE,
    ref.point = c(10),
    fn = function(args, fidelity = NULL, holdout = FALSE) {
      propfeat <- mean(args$selector.selection)
      c(propfeat = propfeat)
    })
  
  generations = 10
  lambda = 10
  
  set.seed(100)
  results <- slickEcr(fitness.fun = fitness.fun, lambda = lambda, population = initials, 
    mutator = mutator.simple, recombinator = crossover.simple, generations = 10)
  
  
  # results.comma <- slickEcr(fitness.fun = fitness.fun, lambda = lambda, population = initials, 
  #   mutator = mutator.simple, recombinator = crossover.simple, generations = 10, 
  #   survival.strategy = "comma", n.elite = 10)
  
  
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
  
  
  # output tests
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
  
  statistics.single <- getStatistics(results.single$log)
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

test_that("initEcr", {
  ps.simple <- pSS(
    a: numeric [0, 10],
    selector.selection: logical^10)

  initials <- sampleValues(ps.simple, 30, discrete.names = TRUE)
  
  fitness.fun.single<- smoof::makeMultiObjectiveFunction(
    sprintf("simple test"),
    has.simple.signature = FALSE, par.set = ps.simple, n.objectives = 1, 
    noisy = TRUE,
    ref.point = c(10),
    fn = function(args, fidelity = NULL, holdout = FALSE) {
      propfeat <- mean(args$selector.selection)
      c(propfeat = propfeat)
    })
  
  init.result = initEcr(fitness.fun = fitness.fun.single, 
    population = initials) 
  init.statistics = getStatistics(init.result$log)
  
  expect_class(init.result, "MosmafsResult")
  expect_data_frame(getStatistics(init.result$log), any.missing = FALSE, 
    nrows = 1)
  expect_equal(init.statistics$gen, 0)
  expect_list(init.result$last.population)
  
  # continueEcr(ecr.object = init.result, generations = 10, lambda = 10, 
  #   mutator = mutator.simple, recombinator = crossover.simple,
  #   p.recomb = 0.7, p.mut = 0.3, survival.strategy = "plus",
  #   parent.selector = selSimple, 
  #   survival.selector = selTournament)

  
  # expect_utils::tail(getPopulations(ecr.object$log), 1)[[1]]$fitness
  # ctrl <- ecr.object$control
  # 
  # lambda <- lambda %??% ecr.object$lambda
  # mutator <- mutator %??% ctrl$mutate
  # recombinator <- recombinator %??% ctrl$recombine
  # parent.selector <- parent.selector %??% ctrl$selectForMating
  # survival.selector <- survival.selector %??% ctrl$selectForSurvival
  # p.recomb <- p.recomb %??% ctrl$p.recomb
  # p.mut <- p.mut %??% ctrl$p.mut
  # survival.strategy <- survival.strategy %??% ecr.object$survival.strategy
  
})
  