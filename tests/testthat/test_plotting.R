context("plotting")

test_that("paretoEdges", {
  fitness <- matrix(
    c(0.7, 2, 1, 
    1, 3, 0.5,
    1, 3, 2), byrow = TRUE,
    nrow = 3, ncol = 3)
  ref.point <- c(3, 3, 3)
  expect_error(paretoEdges(fitness, ref.point), 
    "Must have exactly 2 cols")
  expect_error(paretoEdges(fitness[, 1:2], ref.point), 
    "'refpoint' failed: Must have length 2")
  edges <- paretoEdges(fitness[, 1:2], ref.point[1:2])
  expect_data_frame(edges, nrows = 3, ncols = 3, 
    types = c("numeric", "numeric", "logical"))
  expect_equal(as.numeric(edges[2, 1:2]), fitness[1, 1:2])
  expect_equal(edges[, 3], c(FALSE, TRUE, FALSE))
  
  fitness <- matrix(
    c(0.7, 2, 
      1.5, 1, 
      1, 2,
      0.5, 3), byrow = TRUE,
    nrow = 4, ncol = 2)
  edges = paretoEdges(fitness, refpoint = c(3, 3))
  
  # plot:
  require(ggplot2)
  p <- ggplot(edges, aes(x = X1, y = X2)) + 
    geom_line() + 
    geom_point(data = edges[edges$point, ], shape = "x", size = 5) 
})

test_that("fitnesses", {
  ps.simple <- pSS(
    a: numeric [0, 10],
    selector.selection: logical^10)
  
  mutator.simple <- combine.operators(ps.simple,
    a = mutGauss,
    selector.selection = mutBitflipCHW)
  
  crossover.simple <- combine.operators(ps.simple,
    a = recSBX,
    selector.selection = recPCrossover)
  
  set.seed(1234)
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
  
  generations <- 10
  lambda <- 10
  
  results <- slickEcr(fitness.fun = fitness.fun, lambda = lambda, population = initials, 
    mutator = mutator.simple, recombinator = crossover.simple, generations = 10)
  
  fit <- fitnesses(results)
  expect_data_frame(fit, nrows = 11 * 30, ncols = 3, types = c("numeric", "numeric", "integer"))
  expect_equal(unique(fit$gen), 0:10)
  expect_true(all(as.data.frame(table(fit$gen))$Freq == 30))
  
})

