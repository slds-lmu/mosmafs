context("ecrshims")




test_that("ecrshims error messages", {

  param.set.numeric <- pSS(a: numeric[1, 3]^2, b: numeric[0, 1])
  param.set.integer <- pSS(ai: integer[1, 3]^2, bi: integer[0, 1])
  param.set.logical <- pSS(al: logical^2, bl: logical)
  param.set.logical.extended = pSS(ale: logical^2, ble: logical,
    cle: discrete[l="m", n=10], dle: discrete[a=exp, b=identity]^2)
  param.set.discrete <- pSS(cd: discrete[l="m", n=10, o=NULL],
    d: discrete[a=exp, b=identity, c=`[`]^2)
  fullps <- c(param.set.numeric, param.set.integer,
    param.set.logical.extended, param.set.discrete)
  param.set.requires <- pSS(a: discrete[c("a", "b")], 
    b: numeric[1,2][[requires = expression(a != "a")]])
  
  expect_error(combine.operators(param.set.numeric, .params.x = c("a", "b"),
    x = mutGauss, x = mutGauss), "unique")
  expect_error(combine.operators(pSS(logical: logical), logical = mutBitflip),
    "nameclash with special type names")
  expect_error(combine.operators(pSS(x: character), x = mutBitflip),
    "types of parameters in param.set.*subset")
  expect_error(combine.operators(param.set.numeric, .params.x = c("a", "b")),
    " x defined but without operator")
  expect_error(combine.operators(param.set.numeric, .params.x = c("a", "b", "b"),
    x = mutGauss), "Group Definitions.*duplicate")
  expect_error(combine.operators(param.set.numeric, .params.x = c("a", "b", "c"),
    x = mutGauss), " x contains.* c that are not")
  expect_error(combine.operators(c(param.set.numeric, param.set.integer),
    .params.x = c("a", "b", "ai"), x = mutGauss, ai = mutGauss),
    " x contains parameters of differing types numeric,integer")
  expect_error(combine.operators(param.set.numeric, .params.x = c("a", "b"),
    x = mutGauss, y = mutGauss),
    " y neither a special type nor a parameter name")
  expect_error(combine.operators(param.set.numeric, .params.x = c("a", "b"),
    x = mutGauss, b = mutGauss),
    " b with more than one assigned operator")
  expect_error(combine.operators(param.set.numeric),
    " a,b have neither an explicit operator given")
  expect_error(combine.operators(param.set.numeric, .params.x = c("a", "b"),
    x = identity),
    "list of operator arguments.*ecr_operator")
  expect_error(combine.operators(param.set.numeric, a = mutGauss, b = recSBX),
    "operators given must have at least one of the types ecr_")
  expect_error(combine.operators(param.set.numeric, a = recIntermediate,
    b = recSBX),
    "differing number of children")
  expect_error(combine.operators(param.set.numeric, a = recIntermediate,
    b = makeRecombinator(identity, supported = "custom", n.parents = 3,
      n.children = 1)),
    "differing number of parents")
  specrec <- makeRecombinator(identity, supported = "custom", n.parents = 2,
    n.children = 1)
  class(specrec) <- c(class(specrec), "ecr_mutator")
  expect_error(combine.operators(param.set.numeric, .params.x = c("a", "b"),
    x = specrec),
    "Only one type of operator")
  expect_error(combine.operators(param.set.numeric, .params.x = c("a", "b"),
    x = recIntermediate, .strategy.x = 1),
    "only contain.*function")
  expect_error(combine.operators(param.set.numeric, .params.x = c("a", "b"),
    x = recIntermediate,
    .strategy.x = identity, .strategy.a = identity),
    " a without corresponding function")
  expect_message(combine.operators(param.set.numeric, .params.x = c("a", "b"),
    x = recSBX, numeric = recPMX),
    " numeric, but no parameters of that type present")

  expect_error(combop <- combine.operators(param.set.logical, al = mutGauss,
    bl = mutBitflip),
    " al must have only .* but has parameters ind,lower,upper")
  
  expect_error(combine.operators(param.set.requires, a = mutRandomChoice, 
    b = mutGaussInt), "Parameters with requirements not currently supported.")
  
  expect_error(combine.operators(param.set.logical.extended, 
    .params.ble = c("ble", "ale")), 
    "nameclash with param.set or type names")

  
})
# TODO: mixing types with discrete nonbinary must be forbidden
test_that("ecrshims operation", {

  param.set.numeric <- pSS(a: numeric[1, 3]^2, b: numeric[0, 1])
  param.set.integer <- pSS(ai: integer[1, 3]^2, bi: integer[0, 1])
  param.set.logical <- pSS(al: logical^2, bl: logical)
  param.set.logical.extended = pSS(ale: logical^2, ble: logical,
    cle: discrete[l="m", n=10], dle: discrete[a=exp, b=identity]^2)
  param.set.discrete <- pSS(cd: discrete[l="m", n=10, o=NULL],
    d: discrete[a=exp, b=identity, c=`[`]^2)
  fullps <- c(param.set.numeric, param.set.integer,
    param.set.logical.extended, param.set.discrete)


  combop <- combine.operators(param.set.numeric, a = recSBX, b = recOX)
  expect_class(combop, "ecr_operator")
  expect_class(combop, "ecr_recombinator")
  expect_equal(ecr:::getNumberOfChildren.ecr_recombinator(combop), 2)
  expect_equal(ecr:::getNumberOfParentsNeededForMating.ecr_recombinator(combop), 2)

  combop <- combine.operators(param.set.numeric, a = recIntermediate,
    b = recIntermediate)
  expect_class(combop, "ecr_operator")
  expect_class(combop, "ecr_recombinator")
  expect_equal(ecr:::getNumberOfChildren.ecr_recombinator(combop), 1)
  expect_equal(ecr:::getNumberOfParentsNeededForMating.ecr_recombinator(combop), 2)

  combop <- combine.operators(param.set.numeric, a = mutGauss, b = mutScramble)
  expect_class(combop, "ecr_operator")
  expect_class(combop, "ecr_mutator")

  op <- combine.operators(param.set.numeric, a = ecr::setup(debugrec, extra = 1),
    b = ecr::setup(debugrec, extra = 2))
  expect_output(op(sampleValues(param.set.numeric, 2, discrete.names = TRUE)))

  op <- combine.operators(param.set.numeric,
    numeric = ecr::setup(debugrec, extra = 1))
  expect_output(op(sampleValues(param.set.numeric, 2, discrete.names = TRUE)))


  op <- combine.operators(fullps,
    numeric = ecr::setup(debugrec, extra = "numeric"),
    logical = ecr::setup(debugrec, extra = "logical"),
    integer = ecr::setup(debugrec, extra = "integer"),
    discrete = ecr::setup(debugrec, extra = "discrete"))

  expect_output(op(sampleValues(fullps, 2, discrete.names = TRUE))[[1]])

  op <- combine.operators(fullps,
    numeric = ecr::setup(debugmut, extra = "numeric"),
    logical = ecr::setup(debugmut, extra = "logical"),
    integer = ecr::setup(debugmut, extra = "integer"),
    discrete = ecr::setup(debugmut, extra = "discrete"))

  expect_output(op(sampleValue(fullps, discrete.names = TRUE)))

  op <- combine.operators(
    pSS(one: numeric[, ]^4, two: numeric[, ], three: numeric[0, ]),
    three = mutGauss,
    numeric = mutGauss,
    .strategy.numeric = function(ind) list(sdev = ind$three))


  ps <- pSS(a: logical,
    b: logical^3,
    c: discrete[x, y],
    d: discrete[xx, yy]^4,
    e: discrete[l, m, n],
    f: discrete[ll, mm, nn]^5)

  op <- combine.operators(ps,
    logical = ecr::setup(debugmut, extra = "logical"),
    discrete = ecr::setup(debugmut, extra = "discrete"))

  expect_output(op(list(a = TRUE, b = c(TRUE, TRUE, FALSE), c = "x",
    d = c("xx", "yy", "xx", "yy"), e = "l", f = c("ll", "mm", "nn", "mm", "ll"))))

  expect_output(op(list(a = FALSE, b = c(FALSE, FALSE, TRUE), c = "y",
    d = c("yy", "xx", "yy", "xx"), e = "m", f = c("mm", "ll", "nn", "ll", "mm"))))

  expect_output(op(list(a = FALSE, b = c(FALSE, FALSE, TRUE),
    c = factor("y", levels = c("x", "y")),
    d = list("yy", "xx", "yy", "xx"),
    e = factor("m", levels = c("l", "m", "n")),
    f = list("mm", "ll", "nn", "ll", "mm"))))

  ps <- makeParamSet(
      makeDiscreteParam("param1", c("x", "y")),
      makeDiscreteParam("param2", c("a", "b")))

  sampleValue(ps, discrete.names = TRUE)

  op <- combine.operators(ps,
    logical = makeMutator(function(x, ...) {
      print(x)
      x
    }, "custom"))

  expect_output(op(list(param1 = "x", param2 = "b")))

  op <- combine.operators(ps,
    discrete = makeMutator(function(x, ...) {
      print(x)
      x
    }, "custom"),
    .binary.discrete.as.logical = FALSE)

  expect_output(op(list(param1 = "x", param2 = "b")))

})




