test_that("listToDf", {
  
  temp <- c("a", "b", "c")
  charToFactor<- function(levels){
    sapply(as.character(levels), function(x)
      factor(x, levels=levels),
      simplify = FALSE)
  }
  
  ps.simple <- pSS(
    num: numeric [0, 10],
    int: integer[0, 10] [[trafo = function(x) x / 10]],
    char: discrete [temp], 
    charvec: discrete [temp]^5, 
    fac: discrete [charToFactor(temp)],
    facvec: discrete [charToFactor(temp)]^3,
    selector.selection: logical^10
  )
  
  ind.list <- sampleValues(ps.simple, 3, discrete.names = TRUE, trafo = TRUE)
  result <- listToDf(ind.list, ps.simple)
  
  expect_data_frame(result, any.missing = FALSE, nrows = 3, 
    ncols = sum(getParamLengths(ps.simple)))
  expect_data_frame(listToDf(list(ind.list[[1]]), ps.simple), 
    any.missing = FALSE, nrows = 1, ncols = sum(getParamLengths(ps.simple)))
  expect_data_frame(result[, grep("(charvec)|(fac)", names(result))], types = "factor")
  test <- lapply(result[, grep("(charvec)|(fac)", names(result))],  
    function(x) return(expect_identical(levels(x), temp)))
  
  # Check trafo
  expect_double(result$int, lower = 0, upper = 1)
  ind.list <- sampleValues(ps.simple, 3, discrete.names = TRUE, trafo = FALSE)
  result <- listToDf(ind.list, ps.simple)
  expect_integer(result$int)

  # Check what happens if factor in list 
  ind.list.error <- ind.list
  ind.list.error[[2]]$char <- factor(ind.list.error[[2]]$char)
  expect_error(listToDf(ind.list.error, ps.simple))
  

  
})
