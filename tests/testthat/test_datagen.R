context("datagen")


test_that("linear data", {
  # linear model data
  
  ldata <- create.linear.data(100, 100, permute = FALSE)
  expect_matrix(ldata$X, nrow = 100, ncol = 100)
  expect_numeric(ldata$Y, len = 100)
  expect_true(all(diff(ldata$beta) < 0))
  
  # permute 
  ldata.per <- create.linear.data(100, 100)
  expect_true(!all(diff(ldata.per$beta) < 0))
  
  # annutation factor q
  expect_equal(create.linear.data(10, 10, q = 0, permute = FALSE)$beta, c(1, rep(0, 9)))
  expect_equal(create.linear.data(10, 10, q = 10, permute = FALSE)$beta, 
    1*10^seq(0, 9))
  
  # beta0 
  expect_equal(create.linear.data(10, 10, beta0 = 2, permute = FALSE)$beta[1], 2)
  
  # rho
  ldatarho <- create.linear.data(100, 100, rho = 1)
  expect_equal(c(cor(ldatarho$X)), rep(1, 100*100))
  expect_error(create.linear.data(100, 100, rho = 3), 
    "'rho' failed: Element 1 is not <= 1")
})


test_that("hypersphere data", {
  hdata <- create.hypersphere.data(1000, 1000)
  expect_list(hdata, len = 3)
  expect_matrix(hdata$X, ncols = 1000, nrows = 1000, mode = "numeric")
  expect_true(all(hdata$X <= 1 & hdata$X >= -1))
  
  # dist 
  dist.fun <- function(x) rep(1, x)
  hdatadist <- create.hypersphere.data(10, 10, dist = dist.fun)
  expect_true(all(hdatadist$X == 1))
  expect_true(all(hdatadist$Y == -1))
  expect_error(create.hypersphere.data(10, 10, dist = function(x) 1))
  
})
  


test_that("create.linear.toy.data", {
  expect_error(create.linear.toy.data("a"), 
    "'n' failed: Must be of type 'single integerish value'")
  df <- create.linear.toy.data(10)
  expect_list(df, len = 3)
  expect_logical(df$orig.features, len = 202)
  expect_true(all(df$Y %in% c(-1, 1)))
  expect_matrix(df$X, nrows = 10, ncols = 202)  
})

test_that("create task and clone task", {
  df <- create.linear.toy.data(10)
  df2 <- create.linear.toy.data(10)
  
  # regr.task
  regrtask <- create.regr.task("toy", data = df)
  expect_error(create.regr.task(data = df), 
    'argument "id" is missing, with no default')
  expect_class(create.regr.task("toy", df), "RegrTask")
  
  # classif.task
  clastask <- create.classif.task("toy", df)
  expect_class(clastask, "ClassifTask")
  expect_error(create.classif.task(data = df), 
    'argument "id" is missing, with no default')
  
  # clone task
  Y <- c(rep("+", 5), rep("-", 5))
  new.task <- clonetask(clastask, newdata = data.frame(X = df2$X[, 1:10], Y), 
  newid = "toy.new")
  assert_class(new.task, "ClassifTask")
  # wrong target variable
  expect_error(clonetask(clastask, newdata = data.frame(X = df2$X, Y = df2$Y), 
    newid = "toy.new"), 
    "'Y' failed: Must be of type 'factor', not 'double'")
  
  # Wrong dimension of data compared to orig.features
  expect_error(clonetask(regrtask, newdata = data.frame(X = df2$X[, 1:10], 
    Y = df2$Y), newid = "toy.new"))
  newregrtask <- clonetask(regrtask, newdata = data.frame(X = df2$X, 
    Y = df2$Y), newid = "toy.new")
  assert_class(newregrtask, "RegrTask")
})

test_that("add columns", {
  
  example.task <- mlr::bh.task
  n.col <- ncol(example.task$env$data)
  # orig.features already given
  example.task.2 <- create.regr.task("linear", data = create.linear.data(5, 5))
  
  # Random columns
  new.task <- task.add.random.cols(example.task, 10)
  expect_equal(ncol(new.task$env$data), n.col + 10)
  expect_logical(new.task$orig.features, len = (n.col - 1) + 10)
  expect_equal(sum(new.task$orig.features), n.col - 1)

  new.task.2 <- task.add.random.cols(example.task.2, 10)
  expect_equal(sum(new.task.2$orig.features), 1)
  expect_equal(length(grep("RANDOM", names(new.task.2$env$data))), 10)
  
  
  # Permutated columns
  new.task <- task.add.permuted.cols(example.task, 1)
  expect_equal(ncol(new.task$env$data), n.col + n.col - 1)
  expect_equal(length(grep("PERM", names(new.task$env$data))), n.col - 1)
  expect_logical(new.task$orig.features, len = (n.col*2 - 2))
  expect_equal(sum(new.task$orig.features), n.col - 1)
  
  new.task.2 <- task.add.permuted.cols(example.task.2, 2)
  expect_equal(sum(new.task.2$orig.features), 1)
  expect_equal(length(grep("PERM", names(new.task.2$env$data))), 10)
})

  
test_that("generating data", {
 
  ldata <- create.linear.data(1000, 1000, rho = .9)
  hdata <- create.hypersphere.data(2, 1000)

  
  ltask <- create.classif.task("linear", ldata)
  htask <- create.classif.task("hypersphere", hdata)

  htaskplus <- task.add.random.cols(htask, 5)
  htaskperm <- task.add.permuted.cols(htask, 4)

  expect_equal(sum(htaskperm$orig.features), 2)
})


