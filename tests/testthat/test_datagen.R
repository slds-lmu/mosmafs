context("datagen")


test_that("generating data", {
  ldata <- create.linear.data(1000, 1000, rho = .9)
  hdata <- create.hypersphere.data(2, 1000)

  ltask <- create.classif.task("linear", ldata)
  htask <- create.classif.task("hypersphere", hdata)

  htaskplus <- task.add.random.cols(htask, 5)
  htaskperm <- task.add.permuted.cols(htask, 4)

  expect_equal(sum(htaskperm$orig.features), 2)
})
