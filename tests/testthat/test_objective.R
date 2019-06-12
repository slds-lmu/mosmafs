context("objective")

test_that('makeObjective', {
  task.whole <- mlr::bh.task
  rows.whole <- sample(1:nrow(getTaskData(task.whole)))
  task <- subsetTask(task.whole, rows.whole[1:250])
  task.hout <- subsetTask(task.whole, rows.whole[251])
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
  
  fitness.fun.mos <- makeObjective(lrn, task, ps.simple, nRes, 
    measure = mse, 
    holdout.data = task.hout, worst.measure = 100)
  
  ps.obj  <- attr(fitness.fun.mos, "par.set")
  
  args <- list(maxdepth = 1, minsplit = 1, 
    cp = 0.5, selector.selection = c(rep(T, 12), F))
  
  res <- fitness.fun.mos(args, fidelity = 2, holdout = TRUE)
  
  learner <- setHyperPars(lrn, par.vals = args[!names(args) %in% c("selector.selection")])
  model <- train(learner, task)
  prd <- predict(model, task.hout)
  mse.hout <- performance(prd, list(mse), task, model)[1]
  expect_equal(res[["perf"]], mse.hout[["mse"]])
  
  expect_numeric(res, len = 2)
  expect_equal(names(res), c("perf", "propfeat"))
  expect_equal(res[["propfeat"]], 12/13)
  
  expect_class(fitness.fun.mos, "smoof_multi_objective_function")
  
  expect_true("selector.selection" %in% 
      ParamHelpers::getParamIds(getParamSet(fitness.fun.mos)))
  
  expect_true(all(attr(fitness.fun.mos, "minimize") %in% TRUE))
  
  expect_equal(attr(fitness.fun.mos, "vectorized"), FALSE)
  
  expect_equal(attr(fitness.fun.mos, "n.objectives"), 2)
  
  expect_error(makeObjective(lrn, mlr::iris.task, ps.simple, nRes, holdout.data = task.hout, 
    worst.measure = 100), "Learner 'regr.rpart' must be of type 'classif', not: 'regr'")
  
  expect_error(makeObjective(lrn, task, c(ps.simple,
    pSS(selector.selection: logical^getTaskNFeats(task))), 
    nRes, holdout.data = task.hout, worst.measure = 100), 
    "selector.selection is not allowed to be part of 'ps'")
  
  
  # without holdout
  fitness.fun.mos <- makeObjective(lrn, task, ps.simple, resampling = cv5, 
    measure = mse, worst.measure = 100)
  
  res <- fitness.fun.mos(args, fidelity = 2, holdout = FALSE)
  expect_numeric(res, len = 2)
  
  res <- fitness.fun.mos(args, fidelity = 2, holdout = TRUE)
  expect_true(all(is.infinite(res)))
  
  # with fidelity 0 
  res <- fitness.fun.mos(args, fidelity = 0, holdout = FALSE)
  expect_true(all(res == 0))
  
})

  
test_that("valuesFromNames", {

  temp <- c("a", "b", "c")
  
  charToFactor<- function(levels){
    sapply(as.character(levels), function(x)
      factor(x, levels=levels),
      simplify = FALSE)
  }
  
  
  ps <- pSS(
    num: numeric [0, 10],
    int: integer[0, 10] [[trafo = function(x) x / 10]],
    char: discrete [temp], 
    charvec: discrete [temp]^5, 
    fac: discrete [charToFactor(temp)],
    facvec: discrete [charToFactor(temp)]^3,
    selector.selection: logical^10
  )
  
  
  samp <- sampleValues(ps, n = 2, discrete.names = TRUE)
  expect_error(valuesFromNames(ps, value = samp), 
    "Must have length 7, but has length 2")
  vsamp <- valuesFromNames(ps, value = samp[[1]])
  expect_list(vsamp, len = length(samp[[1]]))
  
  expect_factor(vsamp$fac)
  expect_list(vsamp$facvec, len = length(samp[[1]]$facvec))
  invisible(lapply(vsamp$facvec, function(x) expect_factor(x, len = 1)))
  expect_character(vsamp$char)
  expect_list(vsamp$charvec, len = length(samp[[1]]$charvec))
  expect_numeric(vsamp$num)
  
  # one parameter
  ps <- pSS(charvec: discrete [temp]^5)
  vsamp <- valuesFromNames(ps, value = sampleValue(ps, discrete.names = TRUE))
  expect_list(vsamp, len = 1)
  expect_list(vsamp$charvec, len = ps$pars$charvec$len)
  invisible(lapply(vsamp$charvec, function(x) expect_character(x, len = 1)))
})


test_that("makeBaselineObjective", {
  task.whole <- mlr::iris.task
  rows.whole <- sample(1:nrow(getTaskData(task.whole)))
  task <- subsetTask(task.whole, rows.whole[1:139])
  task.hout <- subsetTask(task.whole, rows.whole[140:150])
  
  lrn <- makeLearner("classif.rpart")
  
  ps <- pSS(
    maxdepth: integer[1, 30],
    minsplit: integer[2, 30],
    cp: numeric[0.001, 0.999])
  filters <- c("praznik_JMI", "anova.test")
  
  # no measure given
  assert_class(makeBaselineObjective(lrn, task, filters = filters, ps = ps, 
    resampling = cv5), "smoof_multi_objective_function")
  
  # measure given
  obj <- makeBaselineObjective(lrn, task,
    filters = filters,  measure = acc,
    ps = ps, resampling = cv5, holdout.data = task.hout)
  
  expect_class(obj, "smoof_multi_objective_function")
  
  nam <- getParamIds(attr(obj, "par.set"))
  expect_equal(length(grep("mosmafs.select.weights", nam)), length(filters))
  
  res <- obj(list(maxdepth = 3, minsplit = 2, cp = 0.5, 
    mosmafs.nselect = 2,
    mosmafs.select.weights.1 = 1,
    mosmafs.select.weights.2 = 0.0))
  
  expect_numeric(res, len = 2, any.missing = FALSE)
  args <- list(maxdepth = 3, minsplit = 2, cp = 0.5)
  learner <- setHyperPars(lrn, par.vals = args)
  model <- train(learner, task)
  prd <- predict(model, task.hout)
  mse.hout <- performance(prd, list(acc), task, model)[1] * -1
  expect_equal(attr(res, "extra")$fitness.holdout.perf, 
    mse.hout[["acc"]])
  
  expect_equal(res[["propfeat"]], 0.5)
  expect_equal(attr(res, "extras")$fitness.holdout.propfeat, 0.5)
  
  ### with num.explicit.featsel
  mbo.two <- makeBaselineObjective(lrn, task,
    filters = filters,  measure = acc,
    ps = ps, resampling = cv5, holdout.data = task.hout, 
    num.explicit.featsel = 2)
  args <- list(maxdepth = 1, minsplit = 2, cp = 0.5, 
    mosmafs.nselect = 2,
    mosmafs.iselect.1 = 0, 
    mosmafs.iselect.2 = 0,
    mosmafs.select.weights.1 = 1,
    mosmafs.select.weights.2 = 0.0)
  set.seed(1234)
  val.1 <- mbo.two(args)[[1]]
  
  # Compare to acc computed by hand
  args <- args[1:3]
  learner <- setHyperPars(lrn, par.vals = args)
  task.sub <- subsetTask(task, features = c(4,3))
  set.seed(1234)
  val.2 <- resample(learner, task.sub, cv5,
    list(acc), show.info = FALSE)$aggr[[1]]
  expect_equal(val.1, -val.2)
  
  ### with mbo 
  require("mlrMBO")
  ctrl <- makeMBOControl(n.objectives = 2) %>%
    setMBOControlInfill(makeMBOInfillCritDIB()) %>%
    setMBOControlTermination(iters = 1)
  
  attr(obj, "noisy") <- FALSE
  mbo_res <- mbo(obj, control = ctrl)
  
  expect_class(mbo_res, "MBOMultiObjResult")
  
  # only one filter value
  filters <- c("anova.test")
  objFil <- makeBaselineObjective(lrn, task, filters = filters, ps = ps, 
    resampling = cv5)
  
  expect_true(all(!(getParamIds(getParamSet(objFil)) %in% 
      c("mosmafs.select.weights.1", "mosmafs.select.weights.2"))))
  
  res <- objFil(list(maxdepth = 3, minsplit = 2, cp = 0.5, 
    mosmafs.nselect = 2,
    mosmafs.select.weights.1 = 1,
    mosmafs.select.weights.2 = 0.0))
  
  expect_error(objFil(list(maxdepth = 3, minsplit = 2, cp = 0.5)), 
    "mosmafs.nselect must be an element in list 'x'")
    
})


# test_that("class in training data, not in test data ", {
#   train_data <- data.frame(one = as.factor(
#     sample(c("a", "b", "c"), size = 10, replace = TRUE)), 
#     y = factor(sample(c(0, 1), size = 10, replace = TRUE)))
#   test_data = data.frame(one = factor(sample(c("d", "e"), 
#     size = 4, replace = TRUE)), 
#     y = factor(sample(c(0, 1), size = 4, replace = TRUE)))
#   
#   example.task <- makeClassifTask(data = train_data, target = "y")
#   hold.task <- makeClassifTask(data = test_data, target = "y")
#   lrn <- makeLearner("classif.rpart")
#   
#   nRes <- function(n) {
#     makeResampleDesc("Subsample", split = 0.9, iters = n)
#   }
#   
#   ps = pSS(
#     one = NA: discrete [c("a", "b", "c")]
#   )
#   
#   exp.obj <- makeObjective(lrn, example.task, ps,  nRes, 
#     holdout.data = hold.task)
#   exp.obj(list(one = "10", selector.selection = FALSE), fidelity = 2)
#   
# })


test_that("measure to be maximized, is multiplied by -1", {
  task <- mlr::iris.task
  
  learner <- makeLearner("classif.rpart")
  
  ps.simple <- pSS(
    maxdepth: integer[1, 30],
    minsplit: integer[2, 30],
    cp: numeric[0.001, 0.999])
  
  nRes <- function(n) {
    makeResampleDesc("Subsample", split = 0.9, iters = n)
  }
  
  fitness.fun.mos <- makeObjective(learner, task, ps.simple, nRes, 
    measure = acc)
  expect_equal(attr(fitness.fun.mos, "ref.point")[[1]], 0)
  
  ps.obj  <- attr(fitness.fun.mos, "par.set")
  
  args <- list(maxdepth = 1, minsplit = 1, 
    cp = 0.5, selector.selection = c(rep(T, 4)))
  
  res <- fitness.fun.mos(args, fidelity = 5)
  expect_true(res[[1]] < 0)
  
  fitness.fun.mos.baseline <- makeBaselineObjective(learner, task, 
    filters = "anova.test", ps.obj, measure = acc, resampling = cv5)
  expect_equal(attr(fitness.fun.mos.baseline, "ref.point")[[1]], 0)
  
  res_baseline <- fitness.fun.mos.baseline(
    list(maxdepth = 1, minsplit = 1, 
      cp = 0.5, selector.selection = c(rep(T, 4)), 
      mosmafs.nselect = 2))
  
  expect_true(res_baseline[[1]] < 0) 
  
  
})


test_that("")

