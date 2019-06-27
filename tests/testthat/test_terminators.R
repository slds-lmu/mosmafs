context("terminators")

test_that("terminator terminate the right way", {
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
  
  # Number evaluations
  expect_string(mosmafsTermEvals(3)(data.frame(evals = c(1:3))), 
    "Number of total evaluations 3 reached limit 3")
  expect_null(mosmafsTermEvals(3)(data.frame(evals = c(1:2))))
  nr.evals <- function(lambda) {round((100 - 30)/lambda)}
  lambda <- 5
  results <- slickEcr(fitness.fun = fitness.fun, 
    lambda = lambda, 
    population = initials, 
    mutator = mutator.simple, recombinator = crossover.simple, 
    generations = list(
      mosmafsTermEvals(100)))
  expect_list(results$pareto.set, min.len = 1)
  expect_true(tail(getStatistics(results$log)$gen, 1) == nr.evals(lambda)) 

  
  # Number generations
  expect_string(mosmafsTermGenerations(3)(data.frame(gens = c(1:3))), 
    "Number of total generations 3 reached limit 3")
  expect_null(mosmafsTermGenerations(3)(data.frame(gens = c(1:2))))
  nr.generations <- 3
  results <- slickEcr(fitness.fun = fitness.fun, lambda = lambda, 
    population = initials, 
    mutator = mutator.simple, recombinator = crossover.simple,
    generations = list(
      mosmafsTermGenerations(nr.generations)))
  expect_list(results$pareto.set, min.len = 1)
  expect_true(tail(getStatistics(results$log)$gen, 1) == nr.generations) 
  
  #term time
  expect_string(mosmafsTermTime(0.1)(data.frame(runtime = c(0.01, 0.2, 0.05))), 
    "Total runtime 0.2 reached limit 0.1")
  expect_null(mosmafsTermTime(1)(data.frame(runtime = c(0.5, 0.7, 0.1))))
  max.time <- 0.1
  results <- slickEcr(fitness.fun = fitness.fun, lambda = lambda,
    population = initials,
    mutator = mutator.simple, recombinator = crossover.simple,
    generations = list(
      mosmafsTermTime(max.time)))
  time = getStatistics(results$log.newinds)$runtime.sum
  expect_true(sum(time) >= max.time)
  expect_true(sum(head(time, -1)) < max.time)
  
  # Hypervolumn stagnation
  result.test <- data.frame(eval.domH = rep(1, 4), 
    gen = 1:4)
  expect_string(mosmafsTermStagnationHV(3)(result.test))
  expect_string(mosmafsTermStagnationHV(2)(result.test))
  result.test$eval.domH[4] = 1.5
  expect_null(mosmafsTermStagnationHV(3)(result.test))
  # reeval 
  result.test <- data.frame(eval.domH = rep(1, 7), 
    gen = 1:7, fid.reeval = c(T, F, T, F, T, F, F))
  expect_null(mosmafsTermStagnationHV(3)(result.test))
  result.test$fid.reeval[5] = F
  expect_string(mosmafsTermStagnationHV(3)(result.test))
  expect_null(mosmafsTermStagnationHV(3)(data.frame()))
  stag <- 2
  results <- slickEcr(fitness.fun = fitness.fun, lambda = 10, 
    population = initials, p.recomb = 0, p.mut = 0, # no mutation and recombination
    survival.strategy = "plus",
    mutator = mutator.simple, recombinator = crossover.simple, 
    generations = list(mosmafsTermStagnationHV(stag)))
  stats <- getStatistics(results$log)
  expect_true(tail(stats$gen, 1) == stag)
  diff <- diff(stats$fitness.domHV)
  expect_true(all.equal(rep(0, length(diff)), diff))
  
  # Stagnation objectives
  result.test2 = data.frame(gen = c(1, 2, 3, 4), 
    eval.obj1.mean = rep(1.5, 4), eval.obj2.mean = c(2, 0, 1, 3))
  term <- mosmafsTermStagnationObjStatistic(3)
  expect_null(mosmafsTermStagnationObjStatistic(3)(result.test2))
  expect_string(mosmafsTermStagnationObjStatistic(3, objective.index = 1)(result.test2), 
    "Mean objective performance did not improve for 3 generations")
  result.test2$eval.obj1.mean[4] = 1.8
  expect_string(mosmafsTermStagnationObjStatistic(3, objective.index = 1)(result.test2), 
    "Mean objective performance did not improve for 3 generations")
  result.test2$eval.obj2.mean = rep(1, 4)
  expect_string(mosmafsTermStagnationObjStatistic(3)(result.test2), 
    "Mean objective performance did not improve for 3 generations")
  result.test2$eval.obj1.mean[4] = 1.3
  expect_null(mosmafsTermStagnationObjStatistic(3)(result.test2))
  result.test2 <- data.frame(eval.obj1.mean = rep(1, 7), 
    gen = 1:7, fid.reeval = c(T, F, T, F, T, F, F))
  expect_null(mosmafsTermStagnationObjStatistic(3)(result.test2))
  result.test2$fid.reeval[5] = F
  expect_string(mosmafsTermStagnationObjStatistic(3)(result.test2), 
    "Mean objective performance did not improve for 3 generations")
  expect_null(mosmafsTermStagnationObjStatistic(3)(data.frame()))
  
  survival.strategy <- function(control, population, offspring, fitness, fitness.offspring) {
    return(list(population = population, fitness = fitness))
  }
  for (stat in c("min", "mean", "max")) {
    results <- slickEcr(fitness.fun = fitness.fun, lambda = 1, 
      population = initials, p.recomb = 0, p.mut = 0, # no mutation and recombination
      survival.strategy = survival.strategy,
      mutator = mutator.simple, recombinator = crossover.simple, 
      generations = list(mosmafsTermStagnationObjStatistic(3, 
        objective.index = c(1, 2), obj.stat = stat), mosmafsTermGenerations(10)))
    stats = collectResult(results)
    expect_true(tail(stats$gen, 1) == 3)
  }
})  



test_that("termination with fidelity", {

  expect_string(mosmafsTermFidelity(1.5)(data.frame(cum.fid = c(1, 1.2, 1.5, 1.4))))
  expect_null(mosmafsTermFidelity(1.5)(data.frame(cum.fid = rep(1, 5))))
  expect_string(mosmafsTermStagnationHV(stag = 3, stag.index = "fidelity")(
    data.frame(eval.domH = rep(1, 4), 
      cum.fid = 1:4)), "HV performance did not increase for 3")
  
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
  
  gen <- 10
  
  max.fidelity <- 42
  
  results.mufi <- slickEcr(
    fitness.fun = fitness.fun,
    lambda = 5,
    population = initials,
    mutator = mutator.simple,
    recombinator = crossover.simple,
    generations = list(mosmafsTermFidelity(max.fidelity), mosmafsTermGenerations(10)),
    fidelity = fidelity)
  stats = collectResult(results.mufi)
  assert_true(tail(stats$cum.fid, 1) >= max.fidelity)
  

})
