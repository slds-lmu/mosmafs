context("filtermat")

test_that("test filtermat column and row order", {
  task <- create.hypersphere.data(3, 2000) %>%
    create.classif.task(id = "sphere") %>%
    task.add.permuted.cols(10)
  filters <- c("variance")
  vari <- generateFilterValuesData(task, method = filters)
  fima <- makeFilterMat(task, filters = filters)
  expect_equal(rank(fima), rank(vari$data$value))
  expect_matrix(fima, ncols = 1, nrows = getTaskNFeats(task))
})
  