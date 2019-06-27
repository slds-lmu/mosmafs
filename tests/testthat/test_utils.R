context("utils")

test_that("listToDf", {
  
  temp <- c("a", "b", "c")
  charToFactor <- function(levels){
    sapply(as.character(levels), function(x)
      factor(x, levels=levels),
      simplify = FALSE)
  }
  
  ps.simple <- pSS(
    num: numeric [0, 10],
    int: integer[0, 10] [[trafo = function(x) x / 10]],
    char: discrete [temp], 
    charvec: discrete [temp]^5, 
    fac: discrete [charToFactor(temp)],
    facvec: discrete [charToFactor(temp)]^3,
    selector.selection: logical^10
  )
  
  ind.list <- sampleValues(ps.simple, 3, discrete.names = TRUE, trafo = TRUE)
  result <- listToDf(ind.list, ps.simple)
  
  expect_data_frame(result, any.missing = FALSE, nrows = 3, 
    ncols = sum(getParamLengths(ps.simple)))
  expect_data_frame(listToDf(list(ind.list[[1]]), ps.simple), 
    any.missing = FALSE, nrows = 1, ncols = sum(getParamLengths(ps.simple)))
  expect_data_frame(result[, grep("(charvec)|(fac)", names(result))], types = "factor")
  test <- lapply(result[, grep("(charvec)|(fac)", names(result))],  
    function(x) return(expect_identical(levels(x), temp)))
  
  # Check trafo
  expect_double(result$int, lower = 0, upper = 1)
  ind.list <- sampleValues(ps.simple, 3, discrete.names = TRUE, trafo = FALSE)
  result <- listToDf(ind.list, ps.simple)
  expect_integer(result$int)

  # Check what happens if factor in list 
  ind.list.error <- ind.list
  ind.list.error[[2]]$char <- factor(ind.list.error[[2]]$char)
  expect_error(listToDf(ind.list.error, ps.simple))
  
  
  fitness.fun <- smoof::makeMultiObjectiveFunction(
    sprintf("simple test"),
    has.simple.signature = TRUE, par.set = ps.simple, n.objectives = 2, 
    noisy = TRUE,
    ref.point = c(10, 1),
    fn = function(args, fidelity = NULL) {
      next
    })
  
  expect_warning(setMosmafsVectorized(fitness.fun), 
    "attribute has.simple.signature of fn was set to TRUE")
  
  # Check if dataframe in case of one discrete parameter 
  ps.simple <- pSS(
    disc: discrete[charToFactor(temp)]
  )
  
  ind.list <- sampleValues(ps.simple, 1, discrete.names = TRUE, trafo = TRUE)
  result <- listToDf(ind.list, ps.simple)
  expect_data_frame(result, nrows = 1, ncol = 1)
  
  ps.simple.d <- pSS(
    disc: discrete[temp]
  )
  ps.simple.dv <- pSS(
    disc: discrete[temp]^3
  )
  ind.list <- sampleValues(ps.simple.d, 1, discrete.names = TRUE, trafo = TRUE)
  result <- listToDf(ind.list, ps.simple.d)
  expect_data_frame(result, nrows = 1, ncol = 1, any.missing = FALSE)
  
  ind.list <- sampleValues(ps.simple.dv, 1, discrete.names = TRUE, trafo = TRUE)
  result <- listToDf(ind.list, ps.simple.dv)
  expect_data_frame(result, nrows = 1, ncol = 3, any.missing = FALSE)
})
  
test_that("initSelector", {
  ps.simple <- pSS(
    a: numeric [1, 10], 
    b: discrete [a, b], 
    selector.selection: logical^10)
  
  
  newly.generated <- function(list1, list2, vector.name) {
    not.newly.generated <- mapply(function(ind1, ind2) {
      all(ind1[[vector.name]] == ind2[[vector.name]])
      }, list1, list2)
  expect_true(!all(not.newly.generated))
  }
  
  
  initials <- sampleValues(ps.simple, 100, discrete.names = TRUE)
  initials.new <- initSelector(initials)
  expect_true(all(unlist(lapply(initials.new, function(x) any(x$selector.selection)))))
  newly.generated(initials, initials.new, "selector.selection")
  
  # Condition NULL
  initials <- sampleValues(ps.simple, 100, discrete.names = TRUE)
  set.seed(100)
  initials.new <- initSelector(initials, reject.condition = NULL)
  expect_true(any(unlist(lapply(initials.new, function(x) !any(x$selector.selection)))))
  newly.generated(initials, initials.new, "selector.selection")
  
  # Other selector 
  ps.simple.new <- c(ps.simple, pSS(use.original: logical^5))
  initials <- sampleValues(ps.simple.new, 100, discrete.names = TRUE)
  init.use.original <- initSelector(initials, "use.original", reject.condition = all)
  # use.original newly generated? 
  newly.generated(initials, init.use.original, "use.original")
  expect_true(any(unlist(lapply(init.use.original, function(x) !all(x$use.original)))))
  
  # Condition NULL
  set.seed(100)
  init.use.original.wth <- initSelector(initials, "use.original", reject.condition = NULL)
  newly.generated(initials, init.use.original.wth, "use.original")
  # some elemnts all TRUE
  expect_true(any(unlist(lapply(init.use.original.wth, function(x) all(x$use.original)))))
  
  # Numeric
  ps.simple <- pSS(
    a: numeric [1, 10], 
    b: discrete [a, b], 
    selector.selection: logical^10)
  
  # With filter strategy and mutator
  task <- mlr::pid.task
  filters <- c("praznik_JMI", "auc", "anova.test", "variance", "DUMMY")
  fima <- makeFilterMat(task, filters = filters)
  
  ps.strat <- pSS(
    maxdepth: integer[1, 30],
    filterweights: numeric[0.001, 0.999]^length(filters), 
    selector.selection: logical^8)
  
  ind <- sampleValues(ps.strat, 10, discrete.names = TRUE)
  new.ind <- initSelector(ind, 
    soften.op = ecr::setup(mutUniformMetaResetSHW, p = 1),
    soften.op.strategy = makeFilterStrategy(fima, "filterweights"))
  expect_list(new.ind, any.missing = FALSE, len = 10)
  
  # Mutator returning only 1s
  # Test for line 'new.selection <- new.selection > 0.5'
  mut.simple <- makeMutator(function(ind) {
    return(rep(1, length(ind)))
  }, supported = "binary")
  inds <- sampleValues(ps.strat, 10, discrete.names = TRUE)
  new.inds <- initSelector(inds, 
    soften.op = ecr::setup(mut.simple))
  invisible(lapply(new.inds, function(x) expect_true(all(x$selector.selection))))

})


test_that("popAggregate and availableAttributes", {
  ps.simple <- pSS(
    a: numeric [0, 10],
    selector.selection: logical^10)
  
  mutator.simple <- combine.operators(ps.simple,
    a = mutGauss,
    selector.selection = mutBitflipCHW)
  
  crossover.simple <- combine.operators(ps.simple,
    a = recSBX,
    selector.selection = recPCrossover)
  
  initials <- sampleValues(ps.simple, 10, discrete.names = TRUE)
  
  fitness.fun <- smoof::makeMultiObjectiveFunction(
    sprintf("simple test"),
    has.simple.signature = FALSE, par.set = ps.simple, n.objectives = 2, 
    noisy = TRUE,
    ref.point = c(10, 1),
    fn = function(args, fidelity = NULL, holdout = FALSE) {
      propfeat <- mean(args$selector.selection)
      c(perf = args$a, propfeat = propfeat)
  })
  
  gen <- 3
  
  results <- slickEcr(fitness.fun = fitness.fun, 
    lambda = 10, 
    population = initials, 
    mutator = mutator.simple, recombinator = crossover.simple, 
    generations = gen) 
  
  ## popAggregate
  # data.frame
  aggr.df <- popAggregate(results$log, extract = c("runtime", "fitness"), 
    data.frame = TRUE)
  expect_list(aggr.df, len = gen + 1)
  expect_data_frame(aggr.df[[1]], nrows = 10, ncols = 3)
  aggr.df <- popAggregate(results$log, extract = c("runtime"), 
    data.frame = TRUE)
  expect_data_frame(aggr.df[[1]], nrows = 10, ncols = 1)
  
  # matrix
  aggr.mat <- popAggregate(results$log, extract = c("runtime", "fitness"), 
    data.frame = FALSE)
  expect_matrix(aggr.mat[[1]], nrows = 3, ncols = 10)
  
  # list
  aggr.list <- popAggregate(results$log, extract = c("runtime", "fitness"), 
    simplify = FALSE)
  expect_list(aggr.list, len = gen + 1) 
  expect_list(aggr.list[[1]], len = 10)
  
  
  # expect_error
  expect_error(popAggregate(results$log, extract = "hoh"))
  expect_error(popAggregate(results, extract = "fitness"), 
    "'log' failed: Must inherit from class 'ecr_logger'")
  expect_error(popAggregate(results$log, extract = "fitness", 
    data.frame = 1))
  
  ## availableAttributes
  expect_character(availableAttributes(results$log, check = TRUE), 
    len = 4, unique = TRUE, any.missing = FALSE)
  empty.pop <- results$log
  empty.pop$env$pop[[1]]$population <- NULL
  expect_error(availableAttributes(empty.pop, check = TRUE), 
    "population of size 0 not allowed")
  missing.pop <- results$log
  missing.pop$env$pop <- NULL
  expect_character(availableAttributes(missing.pop), len = 0)
}) 


