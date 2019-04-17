
context("operators")


test_that("intified operators", {
  
  expect_error(intifyMutator(print), "Must inherit from class 'ecr_mutator'")
  expect_error(mutGaussInt(c(1.5, 2L)), "Must be of type 'integer'")
  expect_error(mutPolynomialInt(c(1L, 2L)), 'argument "lower" is missing, with no default')
  expect_error(mutUniformInt(c(1L, 2L), lower = c(1L, 2L)), 'argument "upper" is missing, with no default')
  expect_error(mutGaussInt(c(1L, 2L), lower = c("a", 3.5), upper = c(1L, 2L)),
    "'lower' failed: Must be of type 'integer'")
  expect_error(mutGaussInt(c(1L, 2L), lower = c(1L, 3L), upper = c("b", 2.5)),
    "'upper' failed: Must be of type 'integer'")
  expect_integer(mutGaussInt(c(1L, 2L), lower = c(1L, 3L), upper = c(3, 2)))
  expect_error(intifyRecombinator(print), "Must inherit from class 'ecr_recombinator'")
  expect_error(recIntSBX(list(c(1L, 2L))), "Must have length >= 2")
  expect_error(recIntSBX(list(c(1L, 2L),c(3, -3.5)), lower = c(1L, -5L), upper = c(3L, 8L)), 
    "elements of inds must be of type integer")
  expect_error(recIntSBX(list(c(1L, 2L),c(3L, -3L)), lower = c("a", -5L)), 
    "'lower' failed: Must be of type 'integer'")
  expect_error(recIntSBX(list(c(1L, 2L),c(3L, -3L))), '"lower" is missing')
  expect_error(recIntSBX(list(c(1L, 2L),c(3L, -3L)), lower = c(1L, -5L)), '"upper" is missing')
  expect_error(recIntSBX(list(c(1L, 2L),c(3L, -3L)), lower = c(1L, -5L), upper = c(5L, "b")), 
    "'upper' failed: Must be of type 'integer'")
  expect_list(recIntSBX(list(c(1L, 2L),c(3L, -3L)), lower = c(1L, -5L), upper = c(5L, 6L)))
  
  
  # testps <- mlrCPO::pSS(x: discrete[a, b, c], y: discrete[m, n, o],
  #   z: discrete[x, y, z]^3,
  #   one: logical, two: numeric[1, 10], three: numeric[0, 1])
  # 
  # 
  # eco <- combine.operators(testps,
  #   discrete = mutRandomChoice,
  #   x = mutRandomChoice,
  #   logical = mutBitflip,
  #   numeric = mutGauss)
  # 
  # 
  # initials <- sampleValues(testps, 1, discrete.names = TRUE)
  # 
  # initials
  # resdf = do.call(rbind, replicate(1000,
  #   as.data.frame(unlist(lapply(eco(initials[[1]]), as.list),
  #     recursive = FALSE)), simplify = FALSE))
  # 
  # 
  # # debug(mutRandomChoice)
  # 
  # eco(initials[[1]])
  # 
  # resdf$two <- NULL
  # resdf$three <- NULL
  # lapply(resdf, table)
  # 
  # 
  # 
  # initials <- sampleValues(testps, 2, discrete.names = TRUE)
  # 
  # reco <- combine.operators(testps,
  #   discrete = recPCrossover,
  #   x = ecr::setup(recPCrossover, p = .5),
  #   logical = recCrossover,
  #   numeric = recSBX)
  # 
  # expect_true(TRUE)
})

test_that("mutators and recombinators", {
  
  # recGaussian
  
  
  # testps <- mlrCPO::pSS(
  #   a: discrete[a, b, c],
  #   b: discrete[m, n, o],
  #   c: discrete[x, y, z]^3,
  #   d: logical,
  #   one: numeric[1, 10],
  #   two: numeric[-1, 1],
  #   three: integer[-5, 5])
  # 
  # eco <- combine.operators(testps,
  #   discrete = mutRandomChoice,
  #   d = mutBitflip, 
  #   numeric = mutGauss, 
    # integer = mutGaussInt)
  
  #initials <- sampleValues(testps, 1, discrete.names = TRUE)
  
  #eco(initials[[1]])
  


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
