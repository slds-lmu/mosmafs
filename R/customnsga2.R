#' @title Modified Interface to ECR
#'
#' @description
#' Mostly [`ecr::ecr`], with some simplifications and extensions.
#'
#' `slickEcr` does mostly what `ecr::ecr` does, with different default values at places.
#' Note that `fitness.fun` must be a "[`smoof`][smoof::smoof-package]" function.
#'
#' `initEcr` only evaluates fitness for the initial population and does not perform any
#' mutation or selection.
#'
#' `continueEcr` continues a run for another number of `generations`. Only `ecr.object`
#' (a result from a previous `initEcr`, `slickEcr`, or `continueEcr` call) and
#' `generations` must be given, the other arguments are optional *if* they were set
#' in a previous `slickEcr` or `continueEcr` call, in which case the values from the
#' previous run are used. Otherwise is possible to supply any combination of these values
#' to set them to new values.
#'
#' Note, for `fidelity`, that the generation continue counting from previous runs,
#' so if `initEcr` was ran for 5 generations and `continueEcr` is called with
#' a `fidelity` with first column values `c(1, 8)`, then the fidelity given in the
#' first row is applied for 2 generations, after which the fidelity given in the
#' second row applies.
#'
#' @param fitness.fun `[smoof_multi_objective_function]` fitness function, must be a
#'   "`smoof`" function.
#' @param lambda `[integer(1)]` number of individuals to add each generation
#' @param population `[list]` list of individuals to start off from.
#' @param mutator `[ecr_mutator]` mutation operator
#' @param recombinator `[ecr_recombinator]` recombination operator
#' @param generations `[integer(1)]` number of iterations to evaluate
#' @param parent.selector `[ecr_selector]` parent selection operator
#' @param survival.selector `[ecr_selector]` survival selection operator
#' @param p.recomb `[numeric(1)]` probability to apply a recombination operator
#' @param p.mut `[numeric(1)]` probability to apply mutation operator
#' @param survival.strategy `[character(1)]` one of `"plus"` or `"comma"`
#' @param n.elite `[integer(1)]` Number of elites to keep, only used if
#'   `survival.strategy` is `"comma"`
#' @param fidelity `[data.frame | NULL]` If this is given, it controls the
#'   fidelity of the function being evaluated, via its `fidelity` argument.
#'   It must then be a `data.frame` with two or three columns. The first column
#'   gives the generation at which the fidelity first applies; the second
#'   column controls the fidelity at that generation or later; the third column,
#'   if given, controls the additional fidelity whenever the result of the first
#'   evaluation is not dominated by any result of the previous generation. The
#'   entries in the first column must be strictly ascending. The first element
#'   of the first column must always be 1. Whenever fidelity changes, the whole
#'   population is re-evaluated, so it is recommended to use only few different
#'   fidelity jumps throughout all generations.
#' @param unbiased.fidelity `[logical(1)]` Whether generations do not have to be re-evaluated when fidelity jumps downward.
#' @param log.stats `[list]` information to log for each generation
#' @param log.stats.newinds `[list]` information to log for each newly evaluated individuals
#' @param ecr.object `[MosmafsResult]` an object retrieved from previous runs of
#'   `initEcr`, `slickEcr`, or `continueEcr`
#' @export
slickEcr <- function(fitness.fun, lambda, population, mutator, recombinator, generations = 100, parent.selector = selSimple, survival.selector = selNondom, p.recomb = 0.7, p.mut = 0.3, survival.strategy = "plus", n.elite = 0, fidelity = NULL, unbiased.fidelity = TRUE, log.stats = list(fitness = list("min", "mean", "max")), log.stats.newinds = c(list(runtime = list("mean", "sum")), if (!is.null(fidelity)) list(fidelity = list("sum")))) {
  if (!smoof::isSmoofFunction(fitness.fun)) {
    stop("fitness.fun must be a SMOOF function")
  }
  n.objectives <- smoof::getNumberOfObjectives(fitness.fun)

  checkEcrArgs(lambda, population, mutator, recombinator, generations, parent.selector, survival.selector, p.recomb, p.mut, survival.strategy, n.elite, n.objectives)

  ecr.object <- initEcr(fitness.fun, population, fidelity = fidelity, log.stats = log.stats, log.stats.newinds = log.stats.newinds, unbiased.fidelity = unbiased.fidelity)

  continueEcr(ecr.object, generations, lambda, mutator, recombinator, parent.selector, survival.selector, p.recomb, p.mut, survival.strategy, n.elite, fidelity)
}

#' @rdname slickEcr
#' @export
initEcr <- function(fitness.fun, population, fidelity = NULL, log.stats = list(fitness = list("min", "mean", "max")), log.stats.newinds = c(list(runtime = list("mean", "sum")), if (!is.null(fidelity)) list(fidelity = list("sum"))), unbiased.fidelity = TRUE) {
  if (!smoof::isSmoofFunction(fitness.fun)) {
    stop("fitness.fun must be a SMOOF function")
  }
  checkFidelity(fidelity)
  assertList(log.stats, names = "unique")

  assertList(log.stats.newinds, names = "unique")

  n.objectives <- smoof::getNumberOfObjectives(fitness.fun)

  ctrl <- initECRControl(fitness.fun)

  # log the state of each generation
  log <- initLogger(ctrl, log.stats = log.stats, log.pop = TRUE,
    log.extras = c(state = "character"),
    init.size = 1000)
  log$env$n.gens <- log$env$n.gens - 1

  # log newly created individuals
  log.newinds <- initLogger(ctrl, log.stats = log.stats.newinds, log.pop = TRUE,
    log.extras = c(size = "integer", population = "character"),
    init.size = 1000)
  log.newinds$env$n.gens <- log.newinds$env$n.gens - 1


  if (!is.null(fidelity)) {
    if (ncol(fidelity) < 3) {
      last.fidelity <- fidelity[[2]][1]
    } else {
      last.fidelity <- fidelity[[2]][1] + fidelity[[3]][1]
    }
  } else {
    last.fidelity <- NULL
  }

  ef <- slickEvaluateFitness(ctrl, population,
    fidelity = last.fidelity,  # high fidelity for first generation
    previous.points = matrix(Inf, nrow = n.objectives))
  fitness <- ef$fitness
  population <- ef$population
  updateLogger(log, population, fitness, n.evals = length(population),
    extras = list(state = "init"))
  updateLogger(log.newinds, population, fitness, n.evals = length(population),
    extras = list(size = length(population), population = "init"))

  result <- ecr:::makeECRResult(ctrl, log, population,  fitness, list(message = "out of generations"))
  result$log.newinds <- log.newinds
  result$control <- ctrl
  result$fidelity <- fidelity
  result$last.fidelity <- last.fidelity
  result$unbiased.fidelity <- unbiased.fidelity
  addClasses(result, "MosmafsResult")
}

#' @rdname slickEcr
#' @export
continueEcr <- function(ecr.object, generations, lambda = NULL, mutator = NULL, recombinator = NULL, parent.selector = NULL, survival.selector = NULL, p.recomb = NULL, p.mut = NULL, survival.strategy = NULL, n.elite = NULL, fidelity = NULL, unbiased.fidelity = NULL) {

  assertClass(ecr.object, "MosmafsResult")

  population <- ecr.object$last.population
  fitness <- utils::tail(getPopulations(ecr.object$log), 1)[[1]]$fitness
  ctrl <- ecr.object$control

  lambda <- lambda %??% ecr.object$lambda
  mutator <- mutator %??% ctrl$mutate
  recombinator <- recombinator %??% ctrl$recombine
  parent.selector <- parent.selector %??% ctrl$selectForMating
  survival.selector <- survival.selector %??% ctrl$selectForSurvival
  p.recomb <- p.recomb %??% ctrl$p.recomb
  p.mut <- p.mut %??% ctrl$p.mut
  survival.strategy <- survival.strategy %??% ecr.object$survival.strategy
  n.elite <- n.elite %??% ecr.object$n.elite %??% 0
  if (is.null(ecr.object$fidelity) && !is.null(fidelity)) {
    stop("Can't use multifidelity when ecr.object was initialized without multifidelity")
  }
  fidelity <- fidelity %??% ecr.object$fidelity
  unbiased.fidelity <- unbiased.fidelity %??% ecr.object$unbiased.fidelity
  last.fidelity <- ecr.object$last.fidelity
  if (!is.null(fidelity) && is.null(last.fidelity)) {
    stop("Inconsistent ecr.object: 'last.fidelity' not given, but 'fidelity' is.")
  }

  needed.args <- c("lambda", "mutator", "recombinator", "parent.selector", "survival.selector", "p.recomb", "p.mut", "survival.strategy", "unbiased.fidelity")
  for (na in needed.args) {
    if (is.null(get(na))) {
      stopf("%s is not given and could not be found in ecr.object", na)
    }
  }

  n.objectives <- ecr.object$task$n.objectives
  checkEcrArgs(lambda, population, mutator, recombinator, generations, parent.selector, survival.selector, p.recomb, p.mut, survival.strategy, n.elite, n.objectives)

  ctrl[c("mutate", "recombine", "selectForMating", "selectForSurvival")] <- NULL

  ctrl <- registerECROperator(ctrl, "mutate", mutator)
  ctrl <- registerECROperator(ctrl, "recombine", recombinator)
  ctrl <- registerECROperator(ctrl, "selectForMating", parent.selector)
  ctrl <- registerECROperator(ctrl, "selectForSurvival", survival.selector)

  log <- clonelog(ecr.object$log)
  log.newinds <- clonelog(ecr.object$log.newinds)

  gen <- log$env$n.gens + 1

  fidelity.row <- max(c(which(fidelity[[1]] <= gen) - 1, 1))

  if (!is.null(fidelity)) {
    if (ncol(fidelity) < 3) {
      fidelity.sum <- fidelity[[2]]
    } else {
      fidelity.sum <- fidelity[[2]] + fidelity[[3]]
    }
  }

  for (gen in seq(gen, gen + generations - 1)) {
    if (length(fidelity[[1]]) > fidelity.row && fidelity[[1]][fidelity.row + 1] <= gen) {
      fidelity.row <- fidelity.row + 1
      new.front.fidelity <- fidelity.sum[fidelity.row]
      if (unbiased.fidelity) {
        reeval <- new.front.fidelity > last.fidelity
      } else {
        reeval <- new.front.fidelity != last.fidelity
      }
      if (reeval) {
        # reset population sampled with new fidelity
        ef <- slickEvaluateFitness(ctrl, population,
          fidelity = fidelity.sum[fidelity.row],
          previous.points = matrix(Inf, nrow = n.objectives))
        fitness <- ef$fitness
        population <- ef$population
        updateLogger(log.newinds, population, fitness, n.evals = length(population),
          extras = list(size = length(population), population = "fidelity.reeval"))
        log.newinds$env$n.gens <- log.newinds$env$n.gens - 1
      }
      last.fidelity <- new.front.fidelity
    }
    assertTRUE(log.newinds$env$n.gens + 1 == gen)
    assertTRUE(log$env$n.gens + 1 == gen)

    offspring <- generateOffspring(ctrl, population,
      fitness, lambda = lambda, p.recomb = p.recomb, p.mut = p.mut)
    ef <- slickEvaluateFitness(ctrl, offspring,
      fidelity = c(fidelity[[2]][fidelity.row], if (length(fidelity) > 2) fidelity[[3]][fidelity.row]),
      previous.points = fitness)
    fitness.offspring <- ef$fitness
    offspring <- ef$population

    updateLogger(log.newinds, offspring, fitness.offspring, n.evals = length(offspring),
      extras = list(size = length(offspring), population = "offspring"))

    if (survival.strategy == "plus") {
      sel <- replaceMuPlusLambda(ctrl, population, offspring, fitness, fitness.offspring)
    } else {
      sel <- replaceMuCommaLambda(ctrl, population, offspring, fitness, fitness.offspring, n.elite = n.elite)
    }
    population <- sel$population
    fitness <- sel$fitness

    updateLogger(log, population, fitness, n.evals = length(offspring),
      extras = list(state = "generation"))
  }
  result <- ecr:::makeECRResult(ctrl, log, population,  fitness, list(message = "out of generations"))
  result$log.newinds <- log.newinds
  result$lambda <- lambda
  ctrl$p.recomb <- p.recomb
  ctrl$p.mut <- p.mut
  result$control <- ctrl
  result$survival.strategy <- survival.strategy
  result$n.elite <- n.elite
  result$fidelity <- fidelity
  result$unbiased.fidelity <- unbiased.fidelity
  result$last.fidelity <- last.fidelity
  addClasses(result, "MosmafsResult")
}

#' @title Compute the Fitness of Individuals
#'
#' @description
#' Evaluates fitness with varying fidelity. Fidelity parameter
#' is passed on to `fidelity` parameter of objective stored in
#' `ctrl`
#'
#' @param ctrl ecr control object
#' @param population `[list]` list of individuals to evaluate
#' @param fidelity `[numeric]` vector of fidelity, with one
#'   or two elements. If this has one element, it is directly
#'   passed on to the fitness function. If it has two elements,
#'   the fitness function is first evaluated with the first
#'   fidelity; if the resulting point dominates the population
#'   given in `population` it is again evaluated with the
#'   second fidelity given, and the result is averaged weighted
#'   by the fidelity parameter.
#' @param previous.points `[matrix]` population to compare points
#'   to if `fidelity` has two elements. Otherwise not used.
#' @return `list(population = list, fitness = matrix)`
#' @export
slickEvaluateFitness <- function(ctrl, population, fidelity = NULL, previous.points = NULL) {
  assertList(population)
  assertNumeric(fidelity, min.len = 1, max.len = 2, null.ok = TRUE)
  assertMatrix(previous.points, min.rows = 1, null.ok = length(fidelity) < 2)

  fitness.fun = ctrl$task$fitness
  ps <- getParamSet(fitness.fun)
  n.obj <- smoof::getNumberOfObjectives(fitness.fun)
  wrapped.fitness <- function(x, fidelity) {
    assertTRUE(isFeasible(ps, x))
    ret <- c(fitness.fun(x, fidelity = fidelity))
    assertNumeric(ret, any.missing = FALSE, len = n.obj)
    ret
  }
  if (is.null(fidelity)) {
    invocation <- function(x) {
      list(time = system.time(res <- wrapped.fitness(x), gcFirst = FALSE)[3], res = res)
    }
  } else if (length(fidelity) == 1) {
    invocation <- function(x) {
      list(time = system.time(res <- wrapped.fitness(x, fidelity = fidelity), gcFirst = FALSE)[3],
        res = res, fidelity = fidelity)
    }
  } else {
    invocation <- function(x) {
      time <- system.time(
        phyttniss <- wrapped.fitness(x, fidelity = fidelity[1]),
        gcFirst = FALSE)[3]
      is.dominated <- dominated(cbind(matrix(phyttniss, ncol = 1), previous.points))[1]
      if (is.dominated) {
        return(list(time = time, res = phyttniss, fidelity = fidelity[1]))
      }
      time <- time + system.time(
        phyttniss.addnl <- wrapped.fitness(x, fidelity = fidelity[2]),
        gcFirst = FALSE)[3]
      phyttniss <- (phyttniss * fidelity[1] + phyttniss.addnl * fidelity[2]) / sum(fidelity)
      list(time = time, res = phyttniss, fidelity = sum(fidelity))
    }
  }

  results <- parallelMap::parallelMap(invocation, population, level = "ecr.evaluateFitness")
  fitness <- extractSubList(results, "res", simplify = FALSE)
  list(
    population = mapply(function(ind, res) {
      attr(ind, "fitness") <- res$res
      attr(ind, "runtime") <- res$time
      attr(ind, "fidelity") <- res$fidelity
      ind
    }, population, results, SIMPLIFY = FALSE),
    fitness = ecr:::makeFitnessMatrix(do.call(cbind, fitness), ctrl))
}


checkFidelity <- function(fidelity) {
  assertDataFrame(fidelity, null.ok = TRUE, min.cols = 2,
    max.cols = 3, min.rows = 1)
  if (!is.null(fidelity)) {
    assertIntegerish(fidelity[[1]], lower = 0, sorted = TRUE,
      unique = TRUE, any.missing = FALSE)
    assertTRUE(fidelity[[1]][1] == 1)
    assertNumeric(fidelity[[2]], any.missing = FALSE)
    assertNumeric(fidelity[[length(fidelity)]], any.missing = FALSE)
  }
}

checkEcrArgs <- function(lambda, population, mutator, recombinator, generations, parent.selector, survival.selector, p.recomb, p.mut, survival.strategy, n.elite, n.objectives) {
  assertInt(lambda, lower = 1)
  assertList(population)
  assertClass(mutator, "ecr_mutator")
  assertClass(recombinator, "ecr_recombinator")
  assertInt(generations, lower = 0)
  assertClass(parent.selector, "ecr_selector")
  assertClass(survival.selector, "ecr_selector")
  assertNumber(p.recomb, lower = 0, upper = 1)
  assertNumber(p.mut, lower = 0, upper = 1)
  assertChoice(survival.strategy, c("plus", "comma"))
  assertInt(n.elite, lower = 0)

  if (n.objectives > 1) {
    obj.name <- "multi-objective"
  } else {
    obj.name <- "single-objective"
  }
  if (obj.name %nin% attr(parent.selector, "supported.objectives")) {
    stopf("parent.selector does not support %s fitness", obj.name)
  }
  if (obj.name %nin% attr(survival.selector, "supported.objectives")) {
    stopf("parent.selector does not support %s fitness", obj.name)
  }
}
