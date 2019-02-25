
#' @title Turn Continuous-Space Operators into Integer-Space Operators
#'
#' @description
#' The input `operator` is wrapped: individuals are fed to it as-is, and
#' output is rounded. Upper and lower bounds are both shifted by 0.5 down or
#' up, respectively, to retain a fair distribution.
#'
#' @param operator `[ecr_operator]` [`ecr_operator`] that supports continuous
#'   variables.
#' @return [`ecr_operator`] operator that operates on integers.
#' @export
intifyMutator <- function(operator) makeMutator(function(ind, ..., lower, upper) {
  ind <- operator(ind, ..., lower = lower - 0.5, upper = upper + 0.5)
  as.integer(pmin(pmax(lower, round(ind)), upper))
}, supported = "custom")

#' @rdname intifyMutator
#' @export
intifyRecombinator <- function(operator) makeRecombinator(function(inds, ..., lower, upper) {
  children <- operator(list(inds[[1]], inds[[2]]), ..., lower = lower - 0.5, upper = upper + 0.5)
  if (attr(children, "multiple")) {
    return(do.call(wrapChildren, lapply(children, function(x) as.integer(pmin(pmax(lower, round(x)), upper)))))
  }
  wrapChildren(as.integer(pmin(pmax(lower, round(children)), upper)))
},
n.parents = ecr:::getNumberOfParentsNeededForMating.ecr_recombinator(operator),
n.children = ecr:::getNumberOfChildren.ecr_recombinator(operator))


#' @title Integer Gaussian Mutator
#'
#' @description
#' See [ecr::mutGauss]
#' @family operators
#' @export
mutGaussInt <- intifyMutator(mutGauss)

#' @title Integer Polynomial Mutator
#'
#' @description
#' See [ecr::mutPolynomial]
#' @family operators
#' @export
mutPolynomialInt <- intifyMutator(mutPolynomial)

#' @title Integer Uniform Mutator
#'
#' @description
#' See [ecr::mutUniform]
#' @family operators
#' @export
mutUniformInt <- intifyMutator(mutUniform)

#' @title Integer SBX Recombinator
#'
#' @description
#' See [ecr::recSBX]
#' @family operators
#' @export
recIntSBX <- intifyRecombinator(recSBX)

#' @title Random Choice Mutator
#'
#' @description
#' "Random Choice" mutation operator for discrete parameters: with prob. `p` chooses
#' one of the available categories at random (this *may* be the original value!)
#' @param ind `[character]` individuum to mutate
#' @param values `[list of character]` set of possible values for `ind` entries to take.
#'   May be a list of length 1, in which case it is recycled.
#' @param p `[numeric(1)]` probability with which to mutate
#' @return [`character`]
#' @family operators
#' @export
mutRandomChoice <- makeMutator(function(ind, values, p = 0.1) {
  mapply(function(i, v) {
    if (runif(1) < p) {
      sample(v, 1)
    } else {
      i
    }
  }, ind, values)
}, supported = "custom")


#' @title Double Geometric Distribution Mutator
#'
#' @description
#' "Double Geometric" mutation operator for integer parameters: with prob. `p` both
#' adds a random geometrically distributed value, and subtracts (a different) one.
#' @param ind `[integer]` individuum to mutate
#' @param p `[numeric(1)]` probability with which to mutate
#' @param geomp `[numeric(1)]` geometric distribution parameter
#' @param lower `[integer]` lower bounds on `ind` values. May have same length as
#'   `ind` or may be shorter, in which case it is recycled.
#' @param upper `[integer]` upper bounds on `ind` values. May have same length as
#'   `ind` or may be shorter, in which case it is recycled.
#' @return [`integer`]
#' @family operators
#' @export
mutDoubleGeom <- makeMutator(function(ind, p = 1, geomp = 0.9, lower, upper) {
  affect <- runif(length(ind)) < p
  naffect <- sum(affect)
  ind[affect] <- ind[affect] + rgeom(naffect, prob = geomp) - rgeom(naffect, prob = geomp)
  pmin(pmax(lower, ind), upper)
}, supported = "custom")

#' @title General Uniform Crossover
#'
#' @description
#' Crossover mutation operator that crosses over each position iid with prob. `p`
#' and can also be used for non-binary operators.
#' @param ind `[list of any]` list of two individuals to perform uniform crossover on
#' @param p `[numeric(1)]` per-entry probability to perform crossover
#' @return `[list of any]` The mutated individuals.
#' @export
recPCrossover <- makeRecombinator(function(ind, p = 0.1, ...) {
  crossovers = runif(length(ind[[1]])) < p
  tmp = ind[[1]][crossovers]
  ind[[1]][crossovers] = ind[[2]][crossovers]
  ind[[2]][crossovers] = tmp
  wrapChildren(ind[[1]], ind[[2]])
}, n.parents = 2, n.children = 2)

#' @title Uniform Reset for Binary Parameters
#'
#' @description
#' Uniformly with probability `p`, draw each bit again: 1 w/prob `reset.dist`, 0 otherwise.
#'
#' @param ind `[integer]` binary individuum with values 0 or 1
#' @param p `[numeric(1)]` entry-wise reset probability
#' @param reset.dist `[numeric]` probability to draw 1-bit per entry, if reset is performed.
#'   `reset.dist` can be length 1 or same length as `ind` (which uses a different distribution for each bit).
#' @return `[integer]` the mutated individuum.
#' @export
mutUniformReset <- makeMutator(function(ind, p = 0.1, reset.dist) {
  if (length(reset.dist) == 1) {
    reset.dist = rep(reset.dist, length(ind))
  }
  assertNumeric(reset.dist, lower = 0, upper = 1, len = length(ind), any.missing = FALSE)
  affect <- runif(length(ind)) < p
  naffect <- sum(affect)
  ind[affect] <- as.numeric(runif(naffect) < reset.dist[affect])
  ind
}, supported = "binary")

#' @title Parametrised Uniform Reset for Binary Parameters
#'
#' @description
#' perform [`mutUniformReset`], with `reset.dist = reset.dists %*% reset.dist.weights`.
#' @param ind `[integer]` binary individuum with values 0 or 1
#' @param p `[numeric(1)]` entry-wise reset probability
#' @param reset.dists `[matrix]` columns of probabilities to draw 1-bit per entry, if reset is performed.
#'   Must have `length(ind)` columns and `length(reset.dist.weights)` rows.
#' @param reset.dist.weights `[numeric]` weight vector to select among `reset.dists` columns.
#' @return `[integer]` the mutated individuum.
#' @export
mutUniformMetaReset <- makeMutator(function(ind, p = 0.1, reset.dists, reset.dist.weights) {
  assertNumeric(reset.dist.weights, lower = 0, upper = 1 - .Machine$double.eps, any.missing = FALSE)
  assertMatrix(reset.dists, nrows = length(ind), ncol = length(reset.dist.weights))
  reset.dist.weights <- -log(1 - reset.dist.weights)
  reset.dist.weights <- reset.dist.weights / sum(reset.dist.weights)  # TODO: this could go into trafo
  mutUniformReset(ind, p = p, reset.dist = reset.dists %*% reset.dist.weights)
}, supported = "binary")

#' @title Create a Filter Strategy Function
#'
#' @description
#' Creates a strategy function that uses the `weight.param.name` entry of
#' individuals as a weighting vector `reset.dist.weights and `reset.dists`
#' for [`mutUniformMetaReset`].
#'
#' @param reset.dists `[matrix]` see `reset.dists` in [mutUniformMetaReset]
#' @param weight.param.name `[character(1)]` name of parameter to use as
#'   `reset.dist.weights` in [mutUniformMetaReset].
#' @return `function`
#' @export
makeFilterStrategy <- function(reset.dists, weight.param.name) {
  function(ind) {
    list(reset.dists = reset.dists, reset.dist.weights = ind[[weight.param.name]])
  }
}
