
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
#' @param log `[ecr_logger]` ecr log object
#' @param extract `[character]` what to extract
#' @param simplify `[logical(1)]` whether to create a
#'   `matrix`/`data.frame` for each generation (default).
#'    Otherwise a list is returned for each generation containing
#'    the value (if `length(extract) == 1`) or a named list of values.
#' @param data.frame `[logical(1)]` Whether to return a `data.frame`
#'   with rows for each individuum (if `TRUE`) or to return a `matrix`
#'   with columns for each individuum compatible as fitness matrix
#'   with various ecr tools (if `FALSE`, default). Only effective if
#'  `simplify` is `TRUE`.
#' @export
popAggregate <- function(log, extract, simplify = TRUE, data.frame = FALSE) {
  assertCharacter(extract, min.len = 1, any.missing = FALSE)
  assertFlag(simplify)
  assertFlag(data.frame)
  pop <- getPopulations(log)
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
#' returns its attributes. if `check` is TRUE it checks that the
#' log object is consistent and throws an error if not. "consistent"
#' here means that all individuals in all generations have the same
#' attributes
#' @param log `[ecr_logger]` ecr log object
#' @param check `[logical(1)]` whether to check consistency.
#'   This should almost never be useful.
#' @return `[character]` attributes of individuals in population.
#' @export
availableAttributes <- function(log, check = FALSE) {
  pop <- getPopulations(log)
  if (length(pop) == 0) return(character(0))
  if (length(pop[[1]]$population) == 0) {
    if (check && any(vlapply(pop, function(gen) length(gen$population) != 0))) {
      stop("Some generations are empty.")
    }
    return(character(0))
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
#' @param ecr.object `[MosmafsResult]` `slickEcr()` result to analyse
#' @param aggregate.perresult `[list]` list of functions
#'   to apply to fitness and holdout fitness. Every entry must either be
#'   a `character(1)` naming the function to use, or a function, in which
#'   that entry must have a name. Each function must return exactly one
#'   numeric value when fed a fitness matrix of one generation. This i
#'   ignored for single-objective runs.
#' @param aggregate.perobjective `[list]` list of functions
#'   to apply to fitness and holdout fitness matrix rows, formatted like
#'   `aggregate.perresult`. Each function must return exactly one numeric
#'   value when fed a fitness vector.
#' @param ref.point `[numeric]` reference point to use for HV computation
#' @param cor.fun `[function]` function to use for calculation of
#'   correlation between objective and holdout. Must take two `numeric`
#'   arguments and return a `numeric(1)`.
#' @return `data.frame`
#' @export
collectResult <- function(ecr.object, aggregate.perresult = list(domHV = function(x) computeHV(x, ref.point)), aggregate.perobjective = list("min", "mean", "max"), ref.point = smoof::getRefPoint(ecr.object$control$task$fitness.fun), cor.fun = cor) {
  assertClass(ecr.object, "MosmafsResult")

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

  assertNumeric(ref.point, any.missing = FALSE, finite = TRUE,
    len = ecr.object$task$n.objectives)
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
  resdf
}

#' @title Initialize Selector
#'
#' @description
#' Sample the `selector.selection` variable such that the number of ones
#' has a given distribution.
#'
#' @param individuals `[list of named lists]` the individuals to initialize
#' @param distribution `[function]` function that returns a random integer
#'   from 0 to the length of each individual's `$selector.selection` slot.
#'   Defaults to the uniform distribution from 1 to `length()`.
#' @param soften.op `[ecr_mutator]` an optional mutator to apply to the
#'   `$selector.selection` variable.
#' @param soften.op.strategy `function` an optional function that can set
#'   the `soften.op`'s parameters. See [`combine.operators`] strategy parameters.
#'   Ignored if `soften.op` is not given.
#' @param soften.op.repeat `[integer(1)]` how often to repeat `soften.op`
#'   application. Ignored if `soften.op` is not given.
#' @param reject.zero `[logical(1)]` whether to reject (and sample anew)
#'   vectors with all zeroes. Default `TRUE`.
#' @return `list of named lists` The individuals with initialized
#'   `$selector.selection.
#' @export
initSelector <- function(individuals, distribution = function() floor(runif(1, 0, length(individuals[[1]]$selector.selection) + 1)), soften.op = NULL, soften.op.strategy = NULL, soften.op.repeat = 1, reject.zero = TRUE) {

  ilen <- length(individuals[[1]]$selector.selection)
  assertList(individuals, types = "list", min.len = 1)
  assertTRUE(all(viapply(individuals, function(x) {
    length(x$selector.selection)
  }) == ilen))

  if (!is.null(soften.op)) {
    assertClass(soften.op, "ecr_mutator")
    assertTRUE("binary" %in%
      ecr:::getSupportedRepresentations.ecr_operator(soften.op))
    assertFunction(soften.op.strategy, null.ok = TRUE)
    assertInt(soften.op.repeat, lower = 0)
  }
  assertFlag(reject.zero)

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
      if (any(new.selection) || !reject.zero) {
        ind.new$selector.selection <- new.selection
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
#' unbiased with respect to (uncorrelated w/r/t objectives) noise in holdout data
#' performance, but it is *not* an estimate of real "dominated hypervolume".
#'
#' Only works on 2-objective performance matrices
#' @param fitness `[matrix]` fitness matrix on training data
#' @param holdout `[matrix]` fitness matrix on holdout data
#' @param refpoint `numeric` reference point
#' @return `numeric`
#' @export
unbiasedHoldoutDomHV <- function(fitness, holdout, refpoint) {
  assertMatrix(fitness, nrow = 2)
  assertMatrix(holdout, nrow = 2, ncol = ncol(fitness))
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
#' @param fitness `[matrix]` fitness matrix on training data
#' @param holdout `[matrix]` fitness matrix on holdout data
#' @param refpoint `numeric` reference point
#' @return `numeric`
#' @export
naiveHoldoutDomHV <- function(fitness, holdout, refpoint) {
  assertMatrix(holdout, nrow = nrow(fitness), ncol = ncol(fitness))
  assertNumeric(refpoint, finite = TRUE, len = nrow(fitness), any.missing = FALSE)
  computeHV(holdout[, nondominated(fitness), drop = FALSE], ref.point = refpoint)
}


#' @title Replace ecr::getStatistics because original is buggy
#'
#' @export
getStatistics <- function(log) {
  assertClass(log, "ecr_logger")
  log$env$stats[seq_len(log$env$cur.line - 1), , drop = FALSE]
}

#' @title Replace ecr::getPopulation because original is buggy
#'
#' @export
getPopulations <- function(log) {
  assertClass(log, "ecr_logger")
  log$env$pop[seq_len(log$env$cur.line - 1)]
}


