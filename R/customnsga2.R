

#' @title NSGA2 without Prejudice
#'
#' @description
#' Performs NSGA2 optimization as done by the ecr2-package, but with
#' arguments that may be overwritten.
#'
#' @param ... Same parameters as in [ecr::ecr]
#'
#' @export
mosmafs.nsga2 = function(...) {
  given <- list(...)
  args <- list(
    representation = "custom",
    parent.selector = selSimple,
    survival.selector = selNondom,
    mu = length(given$initial.solutions)
  )
  if (smoof::isSmoofFunction(given$fitness.fun) &&
      !smoof::isVectorized(given$fitness.fun)) {
    # workaround for for https://github.com/jakobbossek/ecr2/issues/107
    given$n.objectives <- smoof::getNumberOfObjectives(given$fitness.fun)
    given$n.dim <- smoof::getNumberOfParameters(given$fitness.fun)
    # given$par.set <- getParamSet(given$fitness.fun)
    attributes(given$fitness.fun) <- list()
  }

  do.call(ecr::ecr, insert(args, given))
}


#' @title Modified Interface to ECR
#'
#' @description
#' Mostly [`ecr::ecr`], with some simplifications and extensions.
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
#' @param log.status `[list]` information to log
#' @
slickEcr <- function(fitness.fun, lambda, population, mutator, recombinator, generations = 100, parent.selector = selSimple, survival.selector = selNondom, p.recomb = 0.7, p.mut = 0.3, survival.strategy = c("plus", "comma"), n.elite = 0, fidelity = NULL, log.stats = list(fitness = list("min", "mean", "max"))) {

  if (!smoof::isSmoofFunction(fitness.fun)) {
    stop("fitness.fun must be a SMOOF function")
  }

  assertInt(lambda, lower = 1)
  assertList(population)
  assertClass(mutator, "ecr_mutator")
  assertClass(recombinator, "ecr_recombinator")
  assertInt(generations, lower = 0)
  assertClass(parent.selector, "ecr_selector")
  assertClass(survival.selector, "ecr_selector")
  assertNumber(p.recomb, lower = 0, upper = 1)
  assertNumber(p.mut, lower = 0, upper = 1)
  survival.strategy <- match.arg(survival.strategy)
  assertInt(n.elite, lower = 0)
  assertDataFrame(fidelity, null.ok = TRUE, min.cols = 2,
    max.cols = 3, min.rows = 1)
  if (!is.null(fidelity)) {
    assertIntegerish(fidelity[[1]], lower = 0, sorted = TRUE,
      unique = TRUE, any.missing = FALSE)
    assertTRUE(fidelity[[1]][1] == 0)
    assertNumeric(fidelity[[2]], any.missing = FALSE)
    assertNumeric(fidelity[[3]], any.missing = FALSE, null.ok = TRUE)
  }
  assertList(log.stats, named = "unique")

  # workaround for for https://github.com/jakobbossek/ecr2/issues/107
  n.objectives <- smoof::getNumberOfObjectives(fitness.fun)
  ctrl <- initECRControl(fitness.fun)

  ctrl <- registerECROperator(ctrl, "mutate", mutator)
  ctrl <- registerECROperator(ctrl, "recombine", recombinator)
  ctrl <- registerECROperator(ctrl, "selectForMating", parent.selector)
  ctrl <- registerECROperator(ctrl, "selectForSurvival", survival.selector)

  log <- initLogger(ctrl, log.stats = log.stats, log.pop = TRUE,
    init.size = generations * lambda + length(population))


  ef <- slickEvaluateFitness(ctrl, population,
    fidelity = fidelity[[3]][1] %??% fidelity[[2]][1],  # high fidelity for first generation
    population = matrix(Inf, nrow = n.objectives))
  fitness <- ef$fitness
  population <- ef$population

  fidelity.row <- 1

  for (gen in seq_len(generations)) {
    if (length(fidelity[[1]]) > fidelity.row && fidelity[[1]][fidelity.row + 1] <= gen) {
      fidelity.row <- fidelity.row + 1
      # reset population sampled with new fidelity
      ef <- slickEvaluateFitness(ctrl, population,
        fidelity = fidelity[ncol(fidelity), fidelity.row],
        population = matrix(Inf, nrow = n.objectives))
      fitness <- ef$fitness
      population <- ef$population
    }

    offspring <- generateOffspring(ctrl, population,
      fitness, lambda = lambda, p.recomb = p.recomb, p.mut = p.mut)
    ef <- slickEvaluateFitness(ctrl, offspring,
      fidelity = c(fidelity[[2]][fidelity.row], fidelity[[3]][fidelity.row]),
      population = population)
    fitness.offspring <- ef$fitness
    offspring <- ef$population

    if (survival.strategy == "plus") {
      sel <- replaceMuPlusLambda(ctrl, population, offspring, fitness, fitness.offspring)
    } else {
      sel <- replaceMuCommaLambda(ctrl, population, offspring, fitness, fitness.offspring, n.elite = n.elite)
    }
    population <- sel$population
    fitness <- sel$fitness

    updateLogger(log, population, fitness, n.evals = lambda)
  }
  ecr:::makeECRResult(ctrl, log, population, fitness, stop.object)
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
#' @param population `[matrix]` population to compare points
#'   to if `fidelity` has two elements. Otherwise not used.
#' @return `list(population = list, fitness = matrix)`
slickEvaluateFitness <- function(ctrl, population, fidelity = NULL, population = NULL) {
  assertNumeric(fidelity, min.len = 1, max.len = 2, null.ok = TRUE)
  assertMatrix(population, min.rows = 1, null.ok = length(fidelity) < 2)

  fitness.fun = control$task$fitness
  ps <- getParamSet(fitness.fun)
  n.obj <- smoof::getNumberOfObjectives(fitness.fun)
  wrapped.fitness <- function(x, fidelity) {
    assertTRUE(isFeasible(ps, x))
    ret <- c(fitness.fun(x, fidelity = fidelity))
    assertNumeric(ret, any.missing = FALSE, len = n.obj)
    ret
  }
  if (is.null(fidelity)) {
    invocation <- function(x) wrapped.fitness(x)
  } else if (length(fidelity) == 1) {
    invocation <- function(x) wrapped.fitness(x, fidelity = fidelity)
  } else {
    invocation <- function(x) {
      phyttniss <- wrapped.fitness(x, fidelity = fidelity[1])
      is.dominated <- dominated(cbind(matrix(phyttniss, ncol = 1), population))[1]
      if (!is.dominated) {
        phyttniss.addnl <- wrapped.fitness(x, fidelity = fidelity[2])
        phyttniss <- (phyttniss * fidelity[1] + phyttniss.addnl * fidelity[2]) / sum(fidelity)
      }
      phyttniss
    }
  }

  fitness <- parallelMap(invocation, population, level = "ecr.evaluateFitness")
  list(
    population = mapply(function(ind, fit) {
      attr(ind, "fitness") <- fit
    }, population, fitness),
    fitness = ecr:::makeFitnessMatrix(do.call(cbind, fitness), ctrl))
}
