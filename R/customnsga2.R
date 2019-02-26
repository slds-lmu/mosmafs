

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
