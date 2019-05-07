context("objective")

test_that('makeObjective', {
  task.whole <- mlr::bh.task
  rows.whole <- sample(1:nrow(getTaskData(task.whole)))
  task <- subsetTask(task.whole, rows.whole[1:250])
  task.hout <- subsetTask(task.whole, rows.whole[250:505])
  lrn <- makeLearner("regr.rpart")
  
  ps.simple <- pSS(
    maxdepth: integer[1, 30],
    minsplit: integer[2, 30],
    cp: numeric[0.001, 0.999])

  
  nRes <- function(n) {
    makeResampleDesc("Subsample", split = 0.9, iters = n)
  }
  
  expect_error(makeObjective(lrn, task, ps.simple, nRes, holdout.data = task.hout), 
    "Assertion on 'worst.measure' failed: Must be finite")
  
  fitness.fun.mos <- makeObjective(lrn, task, ps.simple, nRes, holdout.data = task.hout, 
    worst.measure = 100)
  
  expect_true("selector.selection" %in% 
      ParamHelpers::getParamIds(getParamSet(fitness.fun.mos)))
  
  expect_error(makeObjective(lrn, mlr::iris.task, ps.simple, nRes, holdout.data = task.hout, 
    worst.measure = 100), "Learner 'regr.rpart' must be of type 'classif', not: 'regr'")
  
  
  
  
  # mutator.simple <- combine.operators(ps.simple,
  #   numeric = ecr::setup(mutGauss, sdev = 0.1),
  #   integer = ecr::setup(mutGaussInt, sdev = 3),
  #   selector.selection = mutBitflipCHW)
  # 
  # crossover.simple <- combine.operators(ps.simple,
  #   numeric = recPCrossover,
  #   integer = recPCrossover,
  #   selector.selection = recPCrossover)
  # 
  # initials <- sampleValues(ps.simple, 32, discrete.names = TRUE)
  # 
  # fidelity <- data.frame(
  #   c(1, 6, 11),
  #   c(1, 3, 5))
  # 
  # run.gen.mufi <- slickEcr(
  #   fitness.fun = fitness.fun.mos,
  #   lambda = 16,
  #   population = initials,
  #   mutator = mutator.simple,
  #   recombinator = crossover.simple,
  #   generations = 5, 
  #   fidelity = fidelity)
  # expect_data_frame(run.gen.mufi$pareto.front, ncol = 2)
  # expect_numeric(run.gen.mufi$pareto.front$y2, upper = 1)
  
})

# test_that("makeBaselineObjective", {
#   obj2 <- makeBaselineObjective(lrn, task,
#     filters = c("praznik_JMI", "auc", "anova.test"),
#     ps = ps, resampling = cv5, num.explicit.featsel = 2,
#     holdout.data = task.hout)
#   
#   ctrl <- makeMBOControl(n.objectives = 2) %>%
#     setMBOControlInfill(makeMBOInfillCritDIB()) %>%
#     setMBOControlTermination(iters = 100)
#   
# })
