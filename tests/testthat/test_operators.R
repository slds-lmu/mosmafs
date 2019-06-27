
context("operators")


test_that("intified operators", {
  
  expect_integer(mutGaussInt(c(1L, 2L), lower = c(1L, 3L), upper = c(3, 3L)), 
    any.missing = FALSE)
  expect_error(intifyMutator(print), "Must inherit from class 'ecr_mutator'")
  expect_error(mutGaussInt(c(1.5, 2L)), "Must be of type 'integerish'")
  expect_error(mutGaussInt(c(1L, 2L), lower = c("a", 3.5), upper = c(1L, 2L)),
    "'lower' failed: Must be of type 'integerish'")
  expect_error(mutGaussInt(c(1L, 2L), lower = c(1L, 3L), upper = c("b", 2.5)),
    "'upper' failed: Must be of type 'integerish'")
  expect_error(mutGaussInt(c(1L, 2L), lower = c(1L, 3L), upper = c(3, 4, 5)), 
    "Length of lower and upper must have same length as individual or 1.")
  expect_error(mutGaussInt(c(1L, 2L), lower = c(1L, 3L, 4), upper = c(3, 4)), 
    "Length of lower and upper must have same length as individual or 1.")
  
  expect_list(recIntSBX(list(c(1L, 2L),c(3L, -3L)), lower = c(1L, -5L), 
    upper = c(5L, 6L)))
  expect_error(recIntSBX(list(c(1L, 2L, 3L),c(3L, -3L)), lower = c(1L, -5L), 
    upper = c(5L, 6L)), "Length of components of individuals must be the same.")
  expect_error(intifyRecombinator(print), "Must inherit from class 'ecr_recombinator'")
  expect_error(recIntSBX(list(c(1L, 2L))), "Must have length >= 2")
  expect_error(recIntSBX(list(c(1L, 2L),c(3, -3.5)), lower = c(1L, -5L), 
    upper = c(3L, 8L)), 
    "Must be of type 'integerish'")
  expect_error(recIntSBX(list(c(1L, 2L),c(3L, -3L)), lower = c("a", -5L)), 
    "'lower' failed: Must be of type 'integerish'")
  expect_error(recIntSBX(list(c(1L, 2L),c(3L, -3L))), '"lower" is missing')
  expect_error(recIntSBX(list(c(1L, 2L),c(3L, -3L)), lower = c(1L, -5L)), 
    '"upper" is missing')
  expect_error(recIntSBX(list(c(1L, 2L),c(3L, -3L)), lower = c(1L, -5L), 
    upper = c(5L, "b")), 
    "'upper' failed: Must be of type 'integerish'")
  expect_error(recIntSBX(list(c(1L, 2),c(3L, -3L)), lower = c(1L, -5L, 3), 
    upper = c(5L, 6L)), "Length of lower and upper must have same length as one individual or 1.")
  expect_error(recIntSBX(list(c(1L, 2),c(3L, -3L)), lower = c(1L, -5L), 
    upper = c(5L, 6L, 7)), "Length of lower and upper must have same length as one individual or 1.")
  expect_error(recIntSBX(list(c(1L, 2),c(3L, -3L)), lower = c(1L, -5L), 
    upper = c(5L, 6L, 7)))

  exp = list(c(1L, 2L),c(3L, -3L))
  lower = c(1L, -5L)
  upper = c(5L, 6L)
  
  # recIntIntermediate
  expect_equal(recIntIntermediate(exp, lower = lower, upper = upper), 
    c(2L, 0L))
  expect_error(recIntIntermediate(exp, lower = c(1L, 3L), upper = c(1L, 2L)), 
    "elements of 'lower' must be component-wise smaller or equal to elements of 'upper")
  
  # recPolynomialInt
  set.seed(12230)
  expect_true(all(mutPolynomialInt(c(1L, 2L), lower = c(1L, 3L), upper = c(3L, 3L)),
    c(1L, 3L) %in% 1L:3L))
  expect_equal(mutPolynomialInt(c(1L, 1L), lower = c(1L, 1L), upper = c(2L, 2L)),
    c(1, 1))
  expect_error(mutPolynomialInt(c(1L, 2L), lower = c(1L, 3L), upper = c(3L, 2L)), 
    "elements of 'lower' must be component-wise smaller or equal to elements of 'upper")
  expect_error(mutPolynomialInt(c(1L, 2L)), 
    'argument "lower" is missing, with no default')
  
  # mutUniformInt
  expect_error(mutUniformInt(c(1L, 2L), lower = c(1L, 2L)), 
    'argument "upper" is missing, with no default')
  m <- mutUniformInt(c(1L, 5L), lower = c(1L, 4L), 
    upper = c(1L, 10L)) 
  expect_identical(m[1], 1L)
  expect_true(m[2] %in% 4L:10L)
  
  # intify Mutator for operator without lower and upper
  mutTest <- makeMutator(mutator = function(ind){
    return(ind)
  }, supported = "float")
  mutIntTest <- intifyMutator(mutTest)
  expect_equal(mutIntTest(exp[[1]], lower = lower, upper = upper), 
    exp[[1]])
  
})

test_that("mutators and recombinators", {
  
  # recGaussian
  expect_list(recGaussian(list(c(1, 2),c(3, -3.5)), lower = c(1, -3), 
    upper = c(3, 5)))
  expect_list(recGaussian(list(c(1, 3.5), c(4, 6)), lower = c(2, 3), 
    upper = c(8)))
  expect_error(recGaussian(list(c(1, 3.5, 6), c(2, 5))))
  expect_error(recGaussian(list(c(1, 3.5))), 
    "'inds' failed: Must have length 2")
  expect_error(recGaussian(list(c(1, 3, 4), c(2, 3))), 
    "Length of components of individuals must be the same")
  expect_error(recGaussian(list(c(1, 3.5), c(4, 6))), 
    '"lower" is missing')
  expect_error(recGaussian(list(c(1, 3.5), c(4, 6)), lower = c(1, 3)), 
    '"upper" is missing')
  expect_error(recGaussian(list(c(1, 3.5), c(4, 6)), lower = c("a", 3), 
    upper = c(5, 7)), 
    "'lower' failed: Must be of type 'numeric'")
  expect_error(recGaussian(list(c(1, 3.5), c(4, 6)), lower = c(1, 5, 7), 
    upper = c(5, 7)), 
    "Length of lower and upper must have same length as one individual or 1.")
  expect_error(recGaussian(list(c(1, 3.5), c(4, 6)), lower = c(2, 3), 
    upper = c("c", 7)), "'upper' failed: Must be of type 'numeric'")
  expect_error(recGaussian(list(c(1, 3.5), c(4, 6)), lower = c(2, 3, 3.5), 
    upper = c(8, 7)))
  
  # mutrandomchoice
  expect_character(mutRandomChoice(c("a", "m"), 
    list("1" = c("a", "b", "c", "d"), 
    "2" = c("m", "n")), p = 0))
  expect_character(mutRandomChoice(c("a", "m"), 
    list("1" = c("a", "b", "c", "m")), p = 1))
  expect_error(mutRandomChoice(c("a", "m", "x"), 
    list("1" = c("a", "b", "c", "d"), "2" = c("m", "n")), p = 0.8), 
    "length of values must be equal to length of ind")
  expect_error(mutRandomChoice(c("a"), values = c("1", "3")), 
    "'values' failed: Must be of type 'list'")
  expect_error(mutRandomChoice(c(23), values = list("a" = c("1", "3"))), 
    "'ind' failed: Must be of type 'character'")
  expect_error(mutRandomChoice(c("a", "m"), 
    list("1" = c("a", "b", "c", "d"), 
      "2" = c("m", "n")), p = 3), "'p' failed: Element 1 is not <= 1")
  
  #mutDoubleGeom
  expect_integerish(mutDoubleGeom(c(1L, 2L), lower = c(1L, 3L), upper = c(3, 2)))
  expect_error(mutDoubleGeom(c(1L, 2L)), 
    'argument "lower" is missing, with no default')
  expect_error(mutDoubleGeom(c(1L, 2L), lower = c(1L, 2L)), 
    'argument "upper" is missing, with no default')
  expect_error(mutDoubleGeom(c(1L, 2L), lower = c("a", 3.5), upper = c(1L, 2L)),
    "'lower' failed: Must be of type 'integerish'")
  expect_error(mutDoubleGeom(c(1L, 2L), lower = c(1L, 3L), upper = c("b", 2.5)),
    "'upper' failed: Must be of type 'integerish'")
  expect_error(mutDoubleGeom(c(1L, 2L), lower = c(1L, 3L), upper = c(3, 4, 5)), 
    "Length of lower and upper must have same length as individual or 1.")
  expect_error(mutDoubleGeom(c(1L, 2L), lower = c(1L, 3L, 4), upper = c(3, 4)), 
    "Length of lower and upper must have same length as individual or 1.")
  expect_error(mutDoubleGeom(c(1, 4), lower = c(2, 3), 
    upper = c(8, 7, 4)), 
    "Length of lower and upper must have same length as individual or 1.")
  expect_error(mutDoubleGeom(c(1L, 2L), lower = c(1L, 3L), upper = c(3, 2), p = NA), 
    "'p' failed: Contains missing values")
  expect_error(mutDoubleGeom(c(1L, 2L), lower = c(1L, 3L), upper = c(3, 2), geomp = -0.1), 
    "'geomp' failed: Element 0 ")
  
  expect_integerish(mutDoubleGeomScaled(c(1L, 2L), lower = c(1L, 3L), upper = c(3, 2)))
  expect_error(mutDoubleGeomScaled(c(1L, 2L)), 
    'argument "lower" is missing, with no default')
  expect_error(mutDoubleGeomScaled(c(1L, 2L), lower = c(1L, 2L)), 
    'argument "upper" is missing, with no default')
  expect_error(mutDoubleGeomScaled(c(1L, 2L), lower = c("a", 3.5), upper = c(1L, 2L)),
    "'lower' failed: Must be of type 'integerish'")
  expect_error(mutDoubleGeomScaled(c(1L, 2L), lower = c(1L, 3L), upper = c("b", 2.5)),
    "'upper' failed: Must be of type 'integerish'")
  expect_error(mutDoubleGeomScaled(c(1L, 2L), lower = c(1L, 3L), upper = c(3, 4, 5)), 
    "Length of lower and upper must have same length as individual or 1.")
  expect_error(mutDoubleGeomScaled(c(1L, 2L), lower = c(1L, 3L, 4), upper = c(3, 4)), 
    "Length of lower and upper must have same length as individual or 1.")
  expect_error(mutDoubleGeomScaled(c(1, 4), lower = c(2, 3), 
    upper = c(8, 7, 4)), 
    "Length of lower and upper must have same length as individual or 1.")
  expect_error(mutDoubleGeomScaled(c(1L, 2L), lower = c(1L, 3L), upper = c(3, 2), p = NA), 
    "'p' failed: Contains missing values")
  expect_error(mutDoubleGeomScaled(c(1L, 2L), lower = c(1L, 3L), upper = c(3, 2), sdev = -0.1), 
    "'sdev' failed: Element 0 ")
  
  # mutUniformParametric
  expect_numeric(mutUniformParametric(ind = c(1, 2, 3), p = 0.1, 
    lx = c(0.5, 0.6, 3), lower = 1, upper = c(5,5,5)))
  expect_error(mutUniformParametric(ind = c("a", 2, 3),p = 0.1, 
    lx = c(0.5, 0.6, 3), lower = c(1, 1, 1), upper = c(5,5,5)), 
    "'ind' failed: Must be of type 'numeric'")
  expect_error(mutUniformParametric(ind = c(1, 2, 3), p = 5, 
    lx = c(0.5, 0.6, 3), lower = c(1, 1, 1), upper = c(5,5,5)), 
    "'p' failed")
  expect_error(mutUniformParametric(ind = c(1, 2, 3), p = 0.5, 
    lx = c(0.5, 0.6, 3), lower = c("a"), upper = c(5,5,5)), 
    "'lower' failed: Must be of type 'numeric'")
  expect_error(mutUniformParametric(ind = c(1, 2, 3), p = 0.5, 
    lx = c(5, "a"), lower = 1, upper = 5), 
    "'lx' failed: Must be of type 'numeric'")
  expect_error(mutUniformParametricScaled(ind = c(1, 2, 3), p = c(0.9, 0.3), 
    sdev = c(0.5, 0.6, 3), lower = 1, upper = c(5,5,5)), 
    "p must have same length as individual or 1.")
  
  # recPCrossover
  expect_error(recPCrossover(inds = list(c(1, 2), c(3)), p = 1), 
    "Length of components of individuals must be the same")
  inds = list(c(1.5, 3), c(4, 5))
  expect_error(recPCrossover(inds = inds, p = c(0.1, 6, 8)), 
    "p must have same length as individual or 1")
  # if p = 1, elements of inds are just flipped around
  expect_true(length(setdiff(recPCrossover(inds = inds, p = 1), list(inds[[2]], inds[[1]]))) == 0)
  
  # mutUniformReset
  expect_integerish(mutUniformReset(ind = c(1, 1, 1, 0, 0), p = 0.5, reset.dist = 0.1), 
    lower = 0, upper = 1, len = 5)
  expect_true(all(mutUniformReset(ind = c(1, 1, 1, 0, 0), p = 1, reset.dist = 0.0) == 0))
  expect_error(mutUniformReset(ind = c(1, 1, 1, 0), p = c(0.9, 0.3), 
    reset.dist = 0.1), "Argument p must have same length as individual or 1.")
  
  #mutUniformMetaReset
  expect_integerish(mutUniformMetaReset(ind = c(1, 1, 1, 0, 0), p = 0.5, 
    reset.dist = matrix(c(0.1, 0.2, 0.1, 0.1, 0.2, 
    0.3, 0.3, 0.3, 0.2, 0.8), nrow = 5, ncol = 2), 
    reset.dist.weights = c(0.2, 0.3)), lower = 0, upper = 1, len = 5)
  expect_error(mutUniformMetaReset(ind = c(1, 1, 1, 0, 0), p = 0.5, 
    reset.dist = matrix(c(0.1, 0.2, 0.1, 0.1, 0.2, 
      0.3, 0.3, 0.3, 0.2, 0.8), nrow = 2, ncol = 5), 
    reset.dist.weights = c(0.2, 0.3, 0.5)), 
    "'reset.dists' failed: Must have exactly 5 rows, but has 2 rows")
  
  # mutGaussScaled
  expect_numeric(mutGaussScaled(runif(5, 0, 10), sdev = runif(5, 0, 1), p = 0.5, lower = rep(0, 5), 
    upper = rep(10, 5)), len = 5)
  expect_integerish(mutGaussIntScaled(sample(1:10, size = 5, replace = TRUE), 
    sdev = runif(5, 0, 1), p = 0.5, lower = rep(0, 5), upper = rep(10, 5)), len = 5)

  
  # selSimpleUnique
  # all selected elements need to be unique
  expect_true(all(!duplicated(selSimpleUnique(matrix(c(1.5, 2.7, 3, 7.5, 8, 9, 10, 5, 5), ncol = 9), 
    n.select = 9))))
  
  # mutBitflipCHW
  expect_integerish(mutBitflipCHW(ind = c(1, 1, 0, 0), p = 0.4), lower = 0, upper = 1)
  expect_error(mutBitflipCHW(ind = c(1, 1, 0, 0), p = 0.8), "'p' failed: Element 1 is not <= 0.5")
  
  
  # mutUnformResetSHW 
  a = c(1, 1, 1, 0, 0)
  expect_integerish(mutUniformResetSHW(ind = a, p = 0.5, reset.dist = 0.5))
  expect_true(all(mutUniformResetSHW(ind = a, p = 1, reset.dist = 0) == 0))
  expect_true(all(mutUniformResetSHW(ind = a, p = 1, reset.dist = 1) == 1))
  expect_true(all(mutUniformResetSHW(ind = a, p = 0, reset.dist = 1) == a))
  expect_error(mutUniformResetSHW(ind = a, p = c(0.5, 1), reset.dist = 1))
  
  # mutUniformMetaResetSHW
  expect_integerish(mutUniformMetaResetSHW(ind = c(1, 1, 1, 0, 0), p = 0.5, 
    reset.dist = matrix(c(0.1, 0.2, 0.1, 0.1, 0.2, 
      0.3, 0.3, 0.3, 0.2, 0.8), nrow = 5, ncol = 2), 
    reset.dist.weights = c(0.2, 0.3)), lower = 0, upper = 1, len = 5)
  expect_error(mutUniformMetaResetSHW(ind = c(1, 1, 1, 0, 0), p = 0.5, 
    reset.dist = matrix(c(0.1, 0.2, 0.1, 0.1, 0.2, 
      0.3, 0.3, 0.3, 0.2, 0.8), nrow = 2, ncol = 5), 
    reset.dist.weights = c(0.2, 0.3, 0.5)), 
    "'reset.dists' failed: Must have exactly 5 rows, but has 2 rows")
})


test_that("makeFilterStrategy", {  
  expect_function(makeFilterStrategy(as.matrix(c(0.1, 0.2, 0.3)), "param.name"))
  expect_error(makeFilterStrategy(as.matrix(c(0.3, 0.6, "a")), "param.name"), 
  "'reset.dists' failed: Must store numerics.")
})

test_that("overallRankMO sorts as expected", {

  # element 3 should be never selected
  expect_true(all(!(selTournamentMO(matrix(c(1.5, 2.7, 3, 7.5, 8, 9), nrow = 2, ncol = 3), 
    n.select = 2) %in% 3)))
  
  
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

  x <- replicate(30, expect_set_equal(selTournamentMO(mat, 2, k = 8, 
    return.unique = TRUE), c(1, 9)))

  y <- replicate(30, expect_set_equal(selTournamentMO(mat, 4, k = 6, 
    return.unique = TRUE), c(1, 9, 3, 7)))

  expect_equal(selTournamentMO(mat, 10, "domhv", c(11, 10), k = 9), rep(7, 10))
  expect_equal(replicate(100, length(intersect(2, selTournamentMO(mat, 10, 
    "domhv", c(11, 10), k = 2)))), rep(0, 100))
  expect_equal(replicate(100, length(intersect(2, selTournamentMO(mat, 10,
    "crowding", c(11, 10), k = 2)))), rep(0, 100))
  expect_equal(replicate(100, length(intersect(c(2, 8, 4), selTournamentMO(mat, 
    10, "domhv", c(11, 10), k = 4)))), rep(0, 100))
  expect_equal(replicate(100, length(intersect(c(2, 6, 4, 8), selTournamentMO(mat,
    10, "crowding", c(11, 10), k = 5)))), rep(0, 100))

})
