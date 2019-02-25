

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

  do.call(ecr::ecr, insert(args, given))
}
