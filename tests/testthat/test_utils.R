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
  
  
  fitness.fun <- smoof::makeMultiObjectiveFunction(
    sprintf("simple test"),
    has.simple.signature = TRUE, par.set = ps.simple, n.objectives = 2, 
    noisy = TRUE,
    ref.point = c(10, 1),
    fn = function(args, fidelity = NULL) {
      next
    })
  
  expect_warning(setMosmafsVectorized(fitness.fun), 
    "attribute has.simple.signature of fn was set to TRUE")
  
})



test_that("initSelector", {
  ps.simple <- pSS(
    a: numeric [1, 10], 
    b: discrete [a, b], 
    selector.selection: logical^10)
  
  
  newly.generated <- function(list1, list2, vector.name) {
    not.newly.generated <- mapply(function(ind1, ind2) {
      all(ind1[[vector.name]] == ind2[[vector.name]])
      }, list1, list2)
  expect_true(!all(not.newly.generated))
  }
  
  
  initials <- sampleValues(ps.simple, 100, discrete.names = TRUE)
  initials.new <- initSelector(initials)
  expect_true(all(unlist(lapply(initials.new, function(x) any(x$selector.selection)))))
  newly.generated(initials, initials.new, "selector.selection")
  
  # Condition NULL
  initials <- sampleValues(ps.simple, 100, discrete.names = TRUE)
  set.seed(100)
  initials.new <- initSelector(initials, reject.condition = NULL)
  expect_true(any(unlist(lapply(initials.new, function(x) !any(x$selector.selection)))))
  newly.generated(initials, initials.new, "selector.selection")
  
  # Other selector 
  ps.simple.new <- c(ps.simple, pSS(use.original: logical^5))
  initials <- sampleValues(ps.simple.new, 100, discrete.names = TRUE)
  init.use.original <- initSelector(initials, "use.original", reject.condition = all)
  # use.original newly generated? 
  newly.generated(initials, init.use.original, "use.original")
  expect_true(any(unlist(lapply(init.use.original, function(x) !all(x$use.original)))))
  
  # Condition NULL
  set.seed(100)
  init.use.original.wth <- initSelector(initials, "use.original", reject.condition = NULL)
  newly.generated(initials, init.use.original.wth, "use.original")
  # some elemnts all TRUE
  expect_true(any(unlist(lapply(init.use.original.wth, function(x) all(x$use.original)))))
})
