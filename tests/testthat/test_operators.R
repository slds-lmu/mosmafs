
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

})
