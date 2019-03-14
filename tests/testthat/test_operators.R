
context("operators")


test_that("intified operators", {

  testps <- mlrCPO::pSS(x: discrete[a, b, c], y: discrete[m, n, o],
    z: discrete[x, y, z]^3,
    one: logical, two: numeric[1, 10], three: numeric[0, 1])


  eco <- combine.operators(testps,
    discrete = mutRandomChoice,
    x = mutRandomChoice,
    logical = mutBitflip,
    numeric = mutGauss)


  initials <- sampleValues(testps, 1, discrete.names = TRUE)

  initials
  resdf = do.call(rbind, replicate(1000,
    as.data.frame(unlist(lapply(eco(initials[[1]]), as.list),
      recursive = FALSE)), simplify = FALSE))


  # debug(mutRandomChoice)

  eco(initials[[1]])

  resdf$two <- NULL
  resdf$three <- NULL
  lapply(resdf, table)



  initials <- sampleValues(testps, 2, discrete.names = TRUE)

  reco <- combine.operators(testps,
    discrete = recPCrossover,
    x = ecr::setup(recPCrossover, p = .5),
    logical = recCrossover,
    numeric = recSBX)

  expect_true(TRUE)
})


test_that("overallRankMO sorts as expected", {

  mat <- matrix(
      c(10, 1, 8, 2, 3, 3, 2, 4, 1, 10,
        10, 4, 6, 6, 4, 8, 9, 9),
      nrow = 2)

  mat <- mat[, c(1, 9, 2, 8, 3, 7, 4, 6, 5)]

  # plot(t(mat), pch = as.character(seq_len(ncol(mat))))

  or <- overallRankMO(mat)

  expect_set_equal(or[c(1, 9)], c(1, 2))

  expect_set_equal(or[c(3, 7)], c(3, 4))

  expect_equal(or[5], 5)
  expect_set_equal(or[c(4, 8)], c(6, 7))

  expect_equal(or[6], 8)
  expect_equal(or[2], 9)

  or <- overallRankMO(mat, "domhv", c(11, 10))

  expect_equal(or[7], 1)
  expect_equal(or[5], 2)
  expect_equal(or[3], 3)
  expect_equal(or[1], 4)
  expect_equal(or[9], 5)

  expect_equal(or[6], 6)
  expect_equal(or[4], 7)
  expect_equal(or[8], 8)
  expect_equal(or[2], 9)

  expect_error(overallRankMO(mat, "domhv", c(11, 12, 13)), "length 2.*but has length 3")
})

test_that("selTournamentMO works as expected", {

  set.seed(1)
  mat <- matrix(
      c(10, 1, 8, 2, 3, 3, 2, 4, 1, 10,
        10, 4, 6, 6, 4, 8, 9, 9),
      nrow = 2)

  mat <- mat[, c(1, 9, 2, 8, 3, 7, 4, 6, 5)]

  expect_set_equal(selTournamentMO(mat, 2, k = 1000, return.unique = TRUE), c(1, 9))

  x <- replicate(30, expect_set_equal(selTournamentMO(mat, 2, k = 8, return.unique = TRUE), c(1, 9)))

  y <- replicate(30, expect_set_equal(selTournamentMO(mat, 4, k = 6, return.unique = TRUE), c(1, 9, 3, 7)))

  expect_equal(selTournamentMO(mat, 10, "domhv", c(11, 10), k = 9), rep(7, 10))

  expect_equal(replicate(100, length(intersect(2, selTournamentMO(mat, 10, "domhv", c(11, 10), k = 2)))), rep(0, 100))

  expect_equal(replicate(100, length(intersect(2, selTournamentMO(mat, 10, "crowding", c(11, 10), k = 2)))), rep(0, 100))

  expect_equal(replicate(100, length(intersect(c(2, 8, 4), selTournamentMO(mat, 10, "domhv", c(11, 10), k = 4)))), rep(0, 100))

  expect_equal(replicate(100, length(intersect(c(2, 6, 4, 8), selTournamentMO(mat, 10, "crowding", c(11, 10), k = 5)))), rep(0, 100))

})
