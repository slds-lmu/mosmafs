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
  
  
test_that("generating data", {
 
  ldata <- create.linear.data(1000, 1000, rho = .9)
  hdata <- create.hypersphere.data(2, 1000)

  
  ltask <- create.classif.task("linear", ldata)
  htask <- create.classif.task("hypersphere", hdata)

  htaskplus <- task.add.random.cols(htask, 5)
  htaskperm <- task.add.permuted.cols(htask, 4)

  expect_equal(sum(htaskperm$orig.features), 2)
})

test_that("linear data", {
  
})
