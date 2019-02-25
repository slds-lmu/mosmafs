

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
  args = list(
    mutator = ecr::setup(mutPolynomial, eta = 25, p = 0.2),
    recombinator = ecr::setup(recSBX, eta = 15, p = 0.7),
    representation = "float",
    parent.selector = selSimple,
    survival.selector = selNondom)

  do.call(ecr::ecr, insert(args, list(...)))
}
