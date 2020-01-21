
deepclone <- function(obj) {
  unserialize(serialize(obj, NULL))
}

clonelog <- function(log) {
  otask <- log$env$task
  log$env$task <- NULL
  log <- deepclone(log)
  log$env$task <- otask
  log
}

#' @title Aggregate Population Results
#'
#' @description
#' Extract attributes saved for individuums in a
#' log object to a more accessible matrix or data.frame.
#'
#' @param log `[ecr_logger]` ecr log object.
#' @param extract `[character]` names of attributes to extract, currently 
#' "names", "fitness", "runtime", "fitness.holdout" and "fidelity" (if used) are supported.
#' @param simplify `[logical(1)]` whether to create a
#'   `matrix`/`data.frame` for each generation (default).
#'    Otherwise a list is returned for each generation containing
#'    the value (if `length(extract) == 1`) or a named list of values.
#' @param data.frame `[logical(1)]` whether to return a `data.frame`
#'   with rows for each individuum (if `TRUE`) or to return a `matrix`
#'   with columns for each individuum compatible as fitness matrix
#'   with various ecr tools (if `FALSE`, default). Only effective if
#'  `simplify` is `TRUE`.
#' @return `[matrix]` if `simplify` is `TRUE`, `[list]` otherwise.
#' @export
popAggregate <- function(log, extract, simplify = TRUE, data.frame = FALSE) {
  assertCharacter(extract, min.len = 1, any.missing = FALSE)
  assertFlag(simplify)
  assertFlag(data.frame)
  pop <- getPopulations(log)
  if (!(all(extract %in% availableAttributes(log)))) {
    stop("Assertion on 'extract' failed: Must be name(s) of attributes of population")
  }
  lapply(pop, function(generation) {
    aggr <- sapply(generation$population, function(individuum) {
      info <- attributes(individuum)[extract]
      if (length(extract) == 1) {
        info[[1]]
      } else if (simplify) {
        unlist(info, recursive = FALSE)
      } else {
        info
      }
    }, simplify = simplify)
    if (simplify && data.frame) {
      if (is.matrix(aggr)) {
        as.data.frame(t(aggr))
      } else {
        df <- data.frame(aggr)
        colnames(df) <- extract
        df
      }
    } else {
      aggr
    }
  })
}

#' @title  List Attributes that can be Aggregated
#'
#' @description
#' Looks at the first individuum in the first generation and
#' returns its attributes. If `check` is TRUE it checks that the
#' log object is consistent and throws an error if not. "Consistent"
#' here means that all individuals in all generations have the same
#' attributes.
#' @param log `[ecr_logger]` ecr log object.
#' @param check `[logical(1)]` whether to check consistency.
#' @return `[character]` attributes of individuals in population.
#' @export
availableAttributes <- function(log, check = FALSE) {
  pop <- getPopulations(log)
  if (length(pop) == 0) return(character(0))
  if (length(pop[[1]]$population) == 0) {
    stop("population of size 0 not allowed")
  }
  attrs <- names(attributes(pop[[1]]$population[[1]]))
  if (check) {
    lapply(pop, function(gen) lapply(gen$population, function(ind)
      assertSetEqual(names(attributes(ind)), attrs)))
  }
  attrs
}

#' @title Collect Result Information
#'
#' @description
#' Merge both `log` and `log.newinds` data for a complete `data.frame`
#' with information about progress (both on training data and on
#' holdout data) and ressource usage.
#'
#' @param ecr.object `[MosmafsResult]` `slickEcr()` result to analyse.
#' @param aggregate.perresult `[list]` list of functions
#'   to apply to fitness and holdout fitness. Every entry must either be
#'   a `character(1)` naming the function to use, or a function, in which
#'   that entry must have a name. Each function must return exactly one
#'   numeric value when fed a fitness matrix of one generation. This is
#'   ignored for single-objective runs.
#' @param aggregate.perobjective `[list]` list of functions
#'   to apply to fitness and holdout fitness matrix rows, formatted like
#'   `aggregate.perresult`. Each function must return exactly one numeric
#'   value when fed a fitness vector.
#' @param ref.point `[numeric]` reference point to use for HV computation.
#' @param cor.fun `[function]` function to use for calculation of
#'   correlation between objective and holdout. Must take two `numeric`
#'   arguments and return a `numeric(1)`.
#' @return `data.frame` 
#' @examples
#' library(mlrCPO)
#' 
#' # Setup of optimization problem 
#' ps.simple <- pSS(
#'  a: numeric [0, 10],
#'  selector.selection: logical^10)
#'  
#' mutator.simple <- combine.operators(ps.simple,
#'  a = mutGauss,
#'  selector.selection = mutBitflipCHW)
#'  
#' crossover.simple <- combine.operators(ps.simple,
#'  a = recSBX,
#'  selector.selection = recPCrossover)
#'
#' initials <- sampleValues(ps.simple, 30, discrete.names = TRUE)
#' 
#' fitness.fun <- smoof::makeMultiObjectiveFunction(
#'  sprintf("simple test"),
#'  has.simple.signature = FALSE, par.set = ps.simple, n.objectives = 2, 
#'  noisy = TRUE,
#'  ref.point = c(10, 1),
#'  fn = function(args, fidelity = NULL, holdout = FALSE) {
#'    pfeat <- mean(args$selector.selection)
#'    c(perform = args$a, pfeat = pfeat)
#'  })
#' fitness.fun.single <- smoof::makeMultiObjectiveFunction(
#'  sprintf("simple test"),
#'  has.simple.signature = FALSE, par.set = ps.simple, n.objectives = 1, 
#'  noisy = TRUE,
#'  ref.point = c(10),
#'  fn = function(args, fidelity = NULL, holdout = FALSE) {
#'    propfeat <- mean(args$selector.selection)
#'    c(propfeat = propfeat)
#'  })
#'
#' # Run NSGA-II
#' results <- slickEcr(fitness.fun = fitness.fun, lambda = 10, population = initials, 
#'  mutator = mutator.simple, recombinator = crossover.simple, generations = 10)
#'
#' # Collect results
#' colres <- collectResult(results)
#' print(colres)
#'  
#' @export
collectResult <- function(ecr.object, aggregate.perresult = list(domHV = function(x) computeHV(x, ref.point)), aggregate.perobjective = list("min", "mean", "max"), ref.point = smoof::getRefPoint(ecr.object$control$task$fitness.fun), cor.fun = cor) {
  assertClass(ecr.object, "MosmafsResult")

  nobj <- ecr.object$task$n.objectives
  normalize.funlist <- function(fl) {
    assertList(fl, any.missing = FALSE, types = c("function", "character"))
    
    charentries <- vlapply(fl, is.character)
    names(fl)[charentries] <- ifelse(is.na(names2(fl)[charentries]),
      unlist(fl[charentries], recursive = FALSE),
      names2(fl)[charentries])
    fl[charentries] <- lapply(fl[charentries], get,
      envir = .GlobalEnv, mode = "function")
    assertList(fl, any.missing = FALSE, types = "function", names = "unique")
  }
  
  aggregate.perresult <- normalize.funlist(aggregate.perresult)
  aggregate.perobjective <- normalize.funlist(aggregate.perobjective)

  if (nobj > 1) {
    assertNumeric(ref.point, any.missing = FALSE, finite = TRUE,
      len = ecr.object$task$n.objectives)
  }

  assertFunction(cor.fun)
  
  aggregate.fitness <- function(fitness) {
    resmat <- sapply(fitness, function(fit) {
      if (ecr.object$task$n.objectives == 1) {
        vnapply(aggregate.perobjective, function(f) f(fit))
      } else {
        if (is.null(rownames(fit))) {
          rownames(fit) <- paste0("obj.", seq_len(nrow(fit)))
        }
        c(
          unlist(apply(fit, 1, function(frow) {
            as.list(vnapply(aggregate.perobjective, function(f) f(frow)))
          })),
          vnapply(aggregate.perresult, function(f) f(fit))
        )
      }
    })
    as.data.frame(t(resmat))
  }
  
  
  fitnesses <- popAggregate(ecr.object$log, "fitness")
  
  stats <- getStatistics(ecr.object$log)
  stats.newinds <- getStatistics(ecr.object$log.newinds)
  
  no.fid <- is.null(stats.newinds$fidelity)
  if (no.fid) {
    stats.newinds$fidelity.sum <- 0
  } else {
    reevals <- stats.newinds$gen[stats.newinds$population == "fidelity.reeval"]
  }
  
  resdf <- with(stats.newinds, data.frame(
    gen,
    runtime = cumsum(runtime.sum),
    evals = cumsum(size),
    cum.fid = cumsum(fidelity.sum)))
  resdf <- resdf[!rev(duplicated(rev(resdf$gen))), ]
  assertTRUE(all.equal(resdf$gen, seq_len(nrow(resdf)) - 1))
  assertTRUE(all.equal(resdf$gen, stats$gen))
  if (no.fid) {
    resdf$cum.fid <- NULL
  } else {
    resdf$fid.reeval <- resdf$gen %in% reevals
  }
  resdf <- cbind(resdf,
    eval = aggregate.fitness(fitnesses))
  
  hofitnesses <- popAggregate(ecr.object$log, "fitness.holdout")
  
  if (any(vlapply(hofitnesses, function(x) any(is.finite(x))))) {
    
    if (nobj == 1) {
      corcols <- mapply(function(eval.fit, hout.fit) {
        suppressWarnings(cor.fun(eval.fit, hout.fit))
      }, fitnesses, hofitnesses)
      names(corcols) <- "obj.1"
      true.hout.domHV <- NA
      naive.hout.domHV <- NA
      resdf <- cbind(resdf, hout = aggregate.fitness(hofitnesses),
        cor = corcols)
    } else {
      corcols <- lapply(seq_len(ecr.object$task$n.objectives), function(idx) {
        mapply(function(eval.fit, hout.fit) {
          suppressWarnings(cor.fun(eval.fit[idx, ], hout.fit[idx, ]))
        }, fitnesses, hofitnesses)
      })
      names(corcols) <- rownames(fitnesses[[1]]) %??% paste0("obj.", seq_len(corcols))
      
      true.hout.domHV <- mapply(function(eval.fit, hout.fit) {
        unbiasedHoldoutDomHV(eval.fit, hout.fit, ref.point)
      }, fitnesses, hofitnesses)
      
      naive.hout.domHV <- mapply(function(eval.fit, hout.fit) {
        naiveHoldoutDomHV(eval.fit, hout.fit, ref.point)
      }, fitnesses, hofitnesses)
      resdf <- cbind(resdf, hout = aggregate.fitness(hofitnesses),
        true.hout.domHV, naive.hout.domHV,
        cor = corcols)
    }
  }
  resdf
}

#' @title Initialize Selector
#'
#' @description
#' Sample the `vector.name` variable such that the number of ones
#' has a given distribution.
#'
#' @param individuals `[list of named lists]` the individuals to initialize.
#' @param vector.name `[character(1)]` the variable name, whose entries are 
#' sampled.
#' @param distribution `[function]` function that returns a random integer
#'   from 0 to the length of each individual's `vector.name` slot.
#'   Defaults to the uniform distribution from 1 to `length()`.
#' @param soften.op `[ecr_mutator]` an optional mutator to apply to the
#'   `vector.name` variable.
#' @param soften.op.strategy `function` an optional function that can set
#'   the `soften.op`'s parameters. See [`combine.operators`] strategy parameters.
#'   Ignored if `soften.op` is not given.
#' @param soften.op.repeat `[integer(1)]` how often to repeat `soften.op`
#'   application. Ignored if `soften.op` is not given.
#' @param reject.condition `[function | NULL]` reject condition as a function applied
#' to newly generated values of `vector.name`. If set to NULL, no rejection is done. 
#' @return `list of named lists` the individuals with initialized
#'   `[[vector.name]]`.
#' @examples
#' library(mlrCPO)
#' 
#' # Initialize parameter set and sample candidates
#' ps <- pSS(
#'  maxdepth: integer[1, 30],
#'  minsplit: integer[2, 30],
#'  cp: numeric[0.001, 0.999], 
#'  selector.selection: logical^5)
#' 
#' initials <- sampleValues(ps, 15, discrete.names = TRUE)
#' 
#' # Resample logical vector selector.selection of initials 
#' # with binomial distribution 
#' initSelector(initials, distribution = function() rbinom(n = 5, size = 5, 
#'  prob = 0.5))
#' 
#' @export
initSelector <- function(individuals, vector.name = "selector.selection", distribution = function() floor(runif(1, 0, length(individuals[[1]][[vector.name]]) + 1)), soften.op = NULL, soften.op.strategy = NULL, soften.op.repeat = 1, reject.condition = function(x) !any(x)) {
  
  ilen <- length(individuals[[1]][[vector.name]])
  iint <- is.numeric(individuals[[1]][[vector.name]])
  assertList(individuals, types = "list", min.len = 1)
  assertTRUE(all(viapply(individuals, function(x) {
    length(x[[vector.name]])
  }) == ilen))
  
  if (!is.null(soften.op)) {
    assertClass(soften.op, "ecr_mutator")
    assertTRUE("binary" %in%
        getSupportedRepresentations(soften.op))
    assertFunction(soften.op.strategy, null.ok = TRUE)
    assertInt(soften.op.repeat, lower = 0)
  }
  assertFunction(reject.condition, null.ok = TRUE)
  
  lapply(individuals, function(ind) {
    repeat {  # repeat when rejecting 0s
      ind.new <- ind
      new.selection <- sample(ilen) <= distribution()
      if (!is.null(soften.op)) {
        new.selection <- as.numeric(new.selection)
        for (rp in seq_len(soften.op.repeat)) {
          opargs <- list(new.selection)
          if (!is.null(soften.op.strategy)) {
            strat.args <- soften.op.strategy(ind)
            assertList(strat.args, names = "unique")
            opargs <- c(opargs, strat.args)
          }
          new.selection <- do.call(soften.op, opargs)
          assertIntegerish(new.selection, len = ilen, any.missing = FALSE)
        }
        new.selection <- new.selection > 0.5
      }
      if (is.null(reject.condition) || !reject.condition(new.selection)) {
        if (iint) {
          new.selection <- as.integer(new.selection)
        }
        ind.new[[vector.name]] <- new.selection
        break
      }
    }
    ind.new
  })
}

#' @title Unbiased Dominated Hypervolume on Holdout Data
#'
#' @description
#' Calculate dominated hypervolume on holdout data. The result is
#' unbiased with respect to (uncorrelated with respect to objectives) noise in holdout data
#' performance, but it is *not* an estimate of real "dominated hypervolume".
#'
#' Only works on two-objective performance matrices.
#' @param fitness `[matrix]` fitness matrix of training data.
#' @param holdout `[matrix]` fitness matrix of holdout data.
#' @param refpoint `[numeric]` reference point.
#' @return `numeric`
#' @export
unbiasedHoldoutDomHV <- function(fitness, holdout, refpoint) {
  assertMatrix(fitness, nrows = 2)
  assertMatrix(holdout, nrows = 2, ncols = ncol(fitness))
  assertNumeric(refpoint, finite = TRUE, len = 2, any.missing = FALSE)
  ordering <- order(fitness[2, ], decreasing = TRUE)
  ordering <- intersect(ordering, which.nondominated(fitness))
  fitness <- fitness[, ordering, drop = FALSE]
  holdout <- holdout[, ordering, drop = FALSE]

  # only first objective of last point is interesting
  holdout <- cbind(holdout, c(refpoint[1], 0))

  last.point <- c(-Inf, refpoint[2])
  area <- 0
  for (idx in seq_len(ncol(holdout))) {
    this.point <- holdout[, idx]
    area <- area + (last.point[2] - this.point[2]) * (refpoint[1] - this.point[1])
    last.point <- this.point
  }
  area
}

#' @title Naive Hypervolume on Holdout Data
#'
#' @description
#' Calculate dominated hypervolume on holdout data. The result is
#' biased depending on noise in holdout data performance.
#'
#' @param fitness `[matrix]` fitness matrix of training data.
#' @param holdout `[matrix]` fitness matrix of holdout data.
#' @param refpoint `[numeric]` reference point.
#' @return `numeric`
#' @export
naiveHoldoutDomHV <- function(fitness, holdout, refpoint) {
  assertMatrix(holdout, nrows = nrow(fitness), ncols = ncol(fitness))
  assertNumeric(refpoint, finite = TRUE, len = nrow(fitness), any.missing = FALSE)
  computeHV(holdout[, nondominated(fitness), drop = FALSE], ref.point = refpoint)
}


#' @title Get Statistics 
#' @description
#' Get statistics from ecr_logger. Replaces [`ecr::getStatistics`] 
#' because original is buggy.
#' @param log `[ecr_logger]` ecr log object
#' @return `data.frame` of logged statistics.
#' @export
getStatistics <- function(log) {
  assertClass(log, "ecr_logger")
  log$env$stats[seq_len(log$env$cur.line - 1), , drop = FALSE]
}

#' @title Get Populations
#' @description 
#' Get populations from ecr_logger. Replaces ecr::getPopulation because 
#' original is buggy.
#' 
#' @param log `[ecr_logger]` ecr log object
#' @return `list` of populations. 
#' @export
getPopulations <- function(log) {
  assertClass(log, "ecr_logger")
  log$env$pop[seq_len(log$env$cur.line - 1)]
}


heuristicHasSimpleSig <- function(fn) { 
  identical(names(environment(fn)), "fn")
}


#' @title Set mosmafs.vectorize
#' @description 
#' Set or change attribute `mosmafs.vectorized` in fitness function. 
#' @param fn `smoof_multi_objective_function` fitness function. 
#' @param vectorize `[logical(1)]` whether to force `slickEvaluateFitness` to 
#' pass candidates to fitness function as `data.frame` or not.
#' @return `smoof_multi_objective_function`.
#' @export
setMosmafsVectorized <- function(fn, vectorize = TRUE) { 
  if (vectorize && heuristicHasSimpleSig(fn)) {
    warningf("attribute has.simple.signature of fn was set to TRUE, correct fitness evaluation can not be guaranteed")  ## schreiben wir sind uns nicht sicher weeil wir nur eine heuristik haben, aber wahrscheinlich macht der user was falsch
  }
  attr(fn, "mosmafs.vectorize") = vectorize
  fn
}


#' @title List to data.frame
#' 
#' @description 
#' Converts a list to a data.frame based on given parameter set.
#' 
#' List elements must have the correct type with respect to 
#' parameter set. Exceptions are discrete parameters, whose values should be 
#' factors, only characters are accepted and factors are returned.           
#' 
#' Returned `data.frame` has column names equal to parameter ids. In case 
#' of vector parameters column names will be numbered. 
#' 
#' @param list.object `[list]` list of individuals, each with elements named 
#' by parameter ids.
#' @param par.set `[ParamSet]` parameter set.
#' @return `[data.frame]`
#' @examples 
#' library(mlrCPO)
#' 
#' # Create parameter set
#' temp <- c("a", "b", "c")
#' ps.simple <- pSS(
#'  num: numeric [0, 10],
#'  int: integer[0, 10] [[trafo = function(x) x / 10]],
#'  char: discrete [temp], 
#'  selector.selection: logical^10)
#'  
#' # Sample values as list and convert list to data frame
#' init.list <- sampleValues(ps.simple, 5, discrete.names = TRUE)
#' result <- listToDf(init.list, ps.simple)
#' result
#' 
#' @export
listToDf = function(list.object, par.set) {
  assertList(list.object)
  checkClass <- function(l) {
    if(any(lapply(l, class) %in% "factor")) {
      stop("list elements are not allowed to be of type factor.")
    } 
  }
  lapply(list.object, checkClass)
  df = lapply(list.object, function(x) {
    Map(function(p, v) {
      if (isScalarNA(v)) 
        v = as.data.frame(t(rep(v, p$len)))
      else as.data.frame(t(v))
    }, par.set$pars, x)
  })
  df = lapply(df, do.call, what = cbind)
  df = as.data.frame(do.call(rbind, df))
  colnames(df) = getParamIds(par.set, repeated = TRUE, with.nr = TRUE)
  fixDesignFactors(df, par.set)
}

fixDesignFactors <- function(df, ps) {
  coltypes <- getParamTypes(ps, df.cols = TRUE)
  psvals <- rep(getValues(ps)[getParamIds(ps)], getParamLengths(ps))
  for (col in seq_along(df)) {
    if (coltypes[col] == "factor") {
      df[[col]] <- factor(df[[col]], levels = names(psvals[[col]]))
    }
  }
  return(df)
}


getNumberOfChildren <- function(recombinator) {
  attr(recombinator, "n.children")
}

getNumberOfParentsNeededForMating <- function(recombinator) {
  attr(recombinator, "n.parents")
}


makeFitnessMatrix <- function(fitness, control) {
  fitness <- addClasses(fitness, "ecr_fitness_matrix")
  attr(fitness, "minimize") <- control$task$minimize
  fitness
}


makeECRResult = function(control, log, population, fitness, stop.object, ...) {
  n.obj <- control$task$n.objectives
  if (n.obj == 1L) {
    return(
      makeS3Obj(
        task = control$task,
        best.x = log$env$best.x,
        best.y = log$env$best.y,
        log = log,
        last.population = population,
        last.fitness = as.numeric(fitness),
        message = stop.object$message,
        classes = c("ecr_single_objective_result", "ecr_result")
        )
    )
  }
  pareto.idx <- which.nondominated(fitness)
  pareto.front <- as.data.frame(t(fitness[, pareto.idx, drop = FALSE]))
  colnames(pareto.front) <- control$task$objective.names
  mo.res <- makeS3Obj(
    task = control$task,
    log = log,
    pareto.idx = pareto.idx,
    pareto.front = pareto.front,
    pareto.set = population[pareto.idx],
    last.population = population,
    message = stop.object$message,
    classes = c("ecr_multi_objective_result", "ecr_result")
  )
  mo.res <- filterDuplicated(mo.res)
}

filterDuplicated <- function(result, ...) {
  id.dup = duplicated(result$pareto.front)
  result$pareto.front = result$pareto.front[!id.dup, , drop = FALSE]
  result$pareto.set = result$pareto.set[!id.dup]
  return(result)
}


