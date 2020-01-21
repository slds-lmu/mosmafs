#' @include utils.R
#' recSBXInt <- intifyRecombinator(recSBX)
#'
#' combo.rec.int <- combine.operators(ps,
#' numeric = recSBX,
#' int = recSBXInt)
#'
#' combo.rec.int(list(list(numb = 1.5, int = 3), list(numb = 3, int = 0))) de utils.R

.tol <- sqrt(.Machine$double.eps) * 4

#' @title Turn Continuous-Space Operators into Integer-Space Operators
#'
#' @description
#' The input `operator` is wrapped: individuals are fed to it as-is, and
#' output is rounded. Upper and lower bounds are both shifted by 0.5 down or
#' up, respectively, to retain a fair distribution.
#'
#' @param operator `[ecr_operator]` [`ecr_operator`][ecr::makeOperator] that supports continuous
#'   variables.
#' @return [`ecr_operator`][ecr::makeOperator] operator that operates on integers.
#' @examples
#' library(mlrCPO)
#'
#' # Create parameter set
#' ps <- pSS(
#' numb: numeric[1, 10],
#' int: integer[0, 5])
#'
#' # Define mutator
#' # If Gaussian mutator is applied to integer parameter,
#' # it does not return an integer
#' combo.mut <- combine.operators(ps,
#' numeric = mutGauss,
#' int = mutGauss)
#' combo.mut(list(numb = 1.5, int = 3))
#'
#' # Turn continuous-space operator mutGauss into integer-space operator
#' mutGaussInt <- intifyMutator(mutGauss)
#' combo.mut.int <- combine.operators(ps,
#' numeric = mutGauss,
#' int = mutGaussInt)
#' combo.mut.int(list(numb = 1.5, int = 3))
#'
#' # Turn continuous-space operator recSBX into integer-space operator
#' recSBXInt <- intifyRecombinator(recSBX)
#' combo.rec.int <- combine.operators(ps,
#' numeric = recSBX,
#' int = recSBXInt)
#' combo.rec.int(list(list(numb = 1.5, int = 3), list(numb = 3, int = 0)))
#'
#' @export
intifyMutator <- function(operator) {
  assertClass(operator, c("ecr_mutator", "ecr_operator", "function"))
  makeMutator(function(ind, ..., lower, upper) { # nocov
    assertIntegerish(ind)
    assertIntegerish(lower, any.missing = FALSE)
    assertIntegerish(upper, any.missing = FALSE)
    n = length(ind)
    if (!((length(lower) %in% c(n, 1)) & (length(upper) %in% c(n, 1)))) {
      stopf("Length of lower and upper must have same length as individual or 1.")
    }
    if (!all(lower <= upper)) {
      stop("elements of 'lower' must be component-wise smaller or equal to elements of 'upper'")
    }
    opargs <- names(formals(args(operator)))
    if ("..." %in% opargs || all(c("lower", "upper") %in% opargs)) {
      indcall <- alist(as.numeric(ind), ..., lower = lower - 0.5, upper = upper + 0.5)
    } else {
      indcall <- alist(as.numeric(ind), ...)
    } # nocov
    ind <- do.call(operator, indcall)
    as.integer(pmin(pmax(lower, round(ind)), upper))
}, supported = "custom")} # nocov

#' @rdname intifyMutator
#' @export
intifyRecombinator <- function(operator) {
  assertClass(operator, c("ecr_recombinator", "ecr_operator", "function"))
  makeRecombinator(function(inds, ..., lower, upper) { # nocov
    assertList(inds, any.missing = FALSE, min.len = 2)
    lapply(inds, assertIntegerish)
    assertIntegerish(lower, any.missing = FALSE, null.ok = TRUE)
    assertIntegerish(upper, any.missing = FALSE, null.ok = TRUE)
    n = length(inds[[1]])
    if (length(unique(lapply(inds, length))) != 1) {
      stop("Length of components of individuals must be the same.")
    } # nocov
    if (!((length(lower) %in% c(n, 1)) & (length(upper) %in% c(n, 1)))) {
      stopf("Length of lower and upper must have same length as one individual or 1.")
    } # nocov
    if (!all(lower <= upper)) {
      stop("elements of 'lower' must be component-wise smaller or equal to elements of 'upper")
    } # nocov
    opargs <- names(formals(args(operator)))
    if ("..." %in% opargs || all(c("lower", "upper") %in% opargs)) {
      childrencall <- alist(list(as.numeric(inds[[1]]), as.numeric(inds[[2]])),
        ..., lower = lower - 0.5, upper = upper + 0.5)
    } else { # nocov
      childrencall <- alist(list(as.numeric(inds[[1]]), as.numeric(inds[[2]])),
        ...)
    } # nocov
    children <- do.call(operator, childrencall)
    if (isTRUE(attr(children, "multiple"))) {
      return(do.call(wrapChildren, lapply(children, function(x)
        as.integer(pmin(pmax(lower, round(x)), upper)))))
    } # nocov start
  wrapChildren(as.integer(pmin(pmax(lower, round(children)), upper)))
},
n.parents = getNumberOfParentsNeededForMating(operator),
n.children = getNumberOfChildren(operator))} # nocov end


#' @title Integer Gaussian Mutator
#'
#' @param ind `[integer]` integer vector/individual to mutate.
#' @param lower `[integer]` vector of minimal values for each parameter of the
#' decision space. Must have the same length as `ind`.
#' @param upper `[integer]` vector of maximal values for each parameter of the
#' decision space. Must have the same length as `ind`.
#' @param ...  further arguments passed on to the method.
#' @return `[integer]` mutated individual.
#' @description
#' See [ecr::mutGauss]
#' @family operators
#' @export
mutGaussInt <- intifyMutator(mutGauss)

#' @title Integer Polynomial Mutator
#'
#' @description
#' See [ecr::mutPolynomial]
#' @param ind `[integer]` integer vector/individual to mutate.
#' @param lower `[integer]` vector of minimal values for each parameter of the
#' decision space. Must have the same length as `ind`.
#' @param upper `[integer]` vector of maximal values for each parameter of the
#' decision space. Must have the same length as `ind`.
#' @param ...  further arguments passed on to the method.
#' @return `[integer]` mutated individual.
#' @family operators
#' @export
mutPolynomialInt <- intifyMutator(mutPolynomial)

#' @title Integer Uniform Mutator
#'
#' @description
#' See [ecr::mutUniform]
#' @param ind `[integer]` integer vector/individual to mutate.
#' @param lower `[integer]` vector of minimal values for each parameter of the
#' decision space. Must have the same length as `ind`.
#' @param upper `[integer]` vector of maximal values for each parameter of the
#' decision space. Must have the same length as `ind`.
#' @param ...  further arguments passed on to the method.
#' @return `[integer]` mutated individual.
#' @family operators
#' @export
mutUniformInt <- intifyMutator(mutUniform)

#' @title Integer SBX Recombinator
#'
#' @description
#' See [ecr::recSBX]
#' @param inds `[integer]` parents, i.e., list of exactly two numeric vectors
#' of equal length.
#' @param lower `[integer]` vector of minimal values for each parameter of the
#' decision space.
#' @param upper `[integer]` vector of maximal values for each parameter of the
#' decision space.
#' @param ...  further arguments passed on to the method.
#' @return `[integer]` mutated individual.
#' @family operators
#' @export
recIntSBX <- intifyRecombinator(recSBX)


#' @title Integer Intermediate Recombinator
#'
#' @description
#' See [ecr::recIntermediate]
#'
#' @param inds `[inds]` parents, i.e., list of exactly two integer vectors
#' of equal length.
#' @param lower `[integer]` vector of minimal values for each parameter of the
#' decision space.
#' @param upper `[integer]` vector of maximal values for each parameter of the
#' decision space.
#' @param ...  further arguments passed on to the method.
#' @return `[integer]` mutated individual.
#' @family operators
#' @export
recIntIntermediate <- intifyRecombinator(recIntermediate)


#' @title Gaussian Intermediate Recombinator
#'
#' @description
#' Gaussian intermediate recombinator samples component-wise from a normal
#' distribution with mean as the component-wise mean
#' and standard deviation as halved components-wise absolute distance
#' of the two given parents.
#' It is applicable only for numeric representations.
#'
#' See also [ecr::recIntermediate].
#' @param inds `[list of numeric]` list of two individuals to recombinate.
#' @param lower `[numeric]` lower bounds of `inds` values. May have same length as
#'   one individual or may be a single number, if the lower bounds are the same for all
#'   values.
#' @param upper `[numeric]` upper bounds of `inds` values. May have same length as
#'   one individual or may be a single number, if the upper bounds are the same for all
#'   values.
#' @return `[list of numeric]` recombined individuals.
#' @family operators
#' @export
recGaussian <- makeRecombinator(function(inds, lower, upper) {
  assertList(inds, len = 2, any.missing = FALSE)
  lapply(inds, assertNumeric)
  if (length(inds[[1]]) != length(inds[[2]])) {
    stop("Length of components of individuals must be the same.")
  }
  n = length(inds[[1]])
  if (!((length(lower) %in% c(n, 1)) & (length(upper) %in% c(n, 1)))) {
    stopf("Length of lower and upper must have same length as one individual or 1.")
  }
  assertNumeric(lower, any.missing = FALSE)
  assertNumeric(upper, any.missing = FALSE)
  do.call(wrapChildren, replicate(2, simplify = FALSE, {
    ind <- rnorm(length(inds[[1]]),
      (inds[[1]] + inds[[2]]) / 2,
      abs(inds[[2]] - inds[[1]]) / 2)
    pmin(pmax(lower, ind), upper)
  }))
}, supported = "float", n.parents = 2, n.children = 2)
recIntGaussian <- intifyRecombinator(recGaussian)



#' @title Random Choice Mutator
#'
#' @description
#' "Random Choice" mutation operator for discrete parameters: with probability
#'  `p` chooses one of the available categories at random (this *may* be
#'  the original value!)
#' @param ind `[character]` individual to mutate.
#' @param values `[list of character]` set of possible values for `ind` entries to take.
#'   May be a list of length 1, in which case it is recycled.
#' @param p `[numeric(1)]` per-entry probability to perform mutation.
#' @return `[character]`
#' @family operators
#' @export
mutRandomChoice <- makeMutator(function(ind, values, p = 0.1) {
  assertCharacter(ind, any.missing = FALSE)
  assertList(values, any.missing = FALSE)
  if (!(length(values) %in% c(1, length(ind)))) {
    stop("length of values must be equal to length of ind")
  }
  assertNumber(p, lower = 0 - .tol, upper = 1 + .tol)
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
#' "Double Geometric" mutation operator for integer parameters: with
#' probability `p` a random geometrically distributed value is added,
#' and another (different) one subtracted.
#'
#' `mutDoubleGeomScaled` scales `sdev` with each component's range and then uses
#' `geomp = (sqrt(2 * sdev^2 + 1) - 1) / sdev^2`.
#'
#'
#' @param ind `[integer]` individual to mutate.
#' @param p `[numeric(1)]` per-entry probability to perform mutation.
#' @param geomp `[numeric]` geometric distribution parameter.
#' @param sdev `[numeric]` standard deviation, relative to `upper - lower`.
#' @param lower `[integer]` lower bounds of `ind` values. May have same length as
#'   `ind` or may be a single number, if the lower bounds are the same for all
#'   values.
#' @param upper `[integer]` upper bounds of `ind` values. May have same length as
#'  `ind` or may be a single number, if the upper bounds are the same for all
#'   values.
#' @return `[integer]`
#' @family operators
#' @export
mutDoubleGeom <- makeMutator(function(ind, p = 1, geomp = 0.9, lower, upper) {
  assertIntegerish(ind)
  assertNumeric(p, lower = 0 - .tol, upper = 1 + .tol, any.missing = FALSE)
  assertNumeric(geomp, lower = 0 - .tol, upper = 1 + .tol, any.missing = FALSE)
  n = length(ind)
  assertIntegerish(lower, any.missing = FALSE)
  assertIntegerish(upper, any.missing = FALSE)
  n = length(ind)
  if (!((length(lower) %in% c(n, 1)) & (length(upper) %in% c(n, 1)))) {
    stopf("Length of lower and upper must have same length as individual or 1.")
  }
  affect <- runif(length(ind)) < p
  naffect <- sum(affect)
  ind[affect] <- ind[affect] + rgeom(naffect, prob = geomp) - rgeom(naffect, prob = geomp)
  as.integer(pmin(pmax(lower, ind), upper))
}, supported = "custom")

#' @rdname mutDoubleGeom
#' @export
mutDoubleGeomScaled <- makeMutator(function(ind, p = 1, sdev = 0.05, lower, upper) {
  assertIntegerish(lower, any.missing = FALSE)
  assertIntegerish(upper, any.missing = FALSE)
  n = length(ind)
  if (!((length(lower) %in% c(n, 1)) & (length(upper) %in% c(n, 1)))) {
    stopf("Length of lower and upper must have same length as individual or 1.")
  }
  assertNumeric(sdev, lower = 0 - .tol, any.missing = FALSE, finite = TRUE)
  sdev <- sdev * (upper - lower)
  mutDoubleGeom(ind, p = p, geomp = (sqrt(2 * sdev^2 + 1) - 1) / sdev^2,
    lower = lower, upper = upper)
}, supported = "custom")

#' @title Parametric Uniform  Mutation
#'
#' @description
#' Adds a variable `delta` to each component `ind[i]`
#' with probability `p`, where `delta` is uniformly
#' distributed between `pmax(lower - ind[x], -lx/2)` and
#' `pmin(upper - ind[i], lx/2)`.
#' @param ind `[numeric | integer]` individual to mutate.
#' @param p `[numeric]` per-entry probability to perform mutation.
#' @param lx `[numeric]` uniform distribution bandwidth.
#' @param sdev `[numeric]` standard deviation, will be scaled to `upper - lower`.
#' @param lower `[integer]` lower bounds of `ind` values. May have same length as
#'   `ind` or may be a single number, if the lower bounds are the same for all
#'   values.
#' @param upper `[integer]` upper bounds of `ind` values. May have same length as
#'  `ind` or may be a single number, if the upper bounds are the same for all
#'   values.
#' @param ...  further arguments passed on to the method.
#' @return `[numeric | integer]` mutated individual.
#' @export
mutUniformParametric <- makeMutator(function(ind, p, lx, lower, upper) {
  assertNumeric(ind)
  assertNumeric(lower, any.missing = FALSE)
  assertNumeric(upper, any.missing = FALSE)
  assertNumeric(lx, any.missing = FALSE, finite = TRUE)
  assertNumeric(p, lower = 0 - .tol, upper = 1 + .tol, any.missing = FALSE)
  n = length(ind)
  for (arg in c("lx", "lower", "upper", "p")) {
    if (!(length(get(arg)) %in% c(n, 1))) {
      stopf("%s must have same length as individual or 1.",
        arg)
    }
    if ((length(get(arg)) == 1) & (arg != "p")) {
      assign(arg, rep_len(get(arg), length(ind)))
    }
  }
  affect <- runif(length(ind)) < p
  naffect <- sum(affect)
  ind[affect] <- runif(naffect,
    pmax(lower[affect], ind[affect] - lx[affect]/2),
    pmin(upper[affect], ind[affect] + lx[affect]/2))
  ind
}, supported = "float")

#' @rdname mutUniformParametric
#' @export
mutUniformParametricScaled <- makeMutator(function(ind, p, sdev, lower, upper) {
  assertNumeric(lower, any.missing = FALSE, finite = TRUE)
  assertNumeric(upper, any.missing = FALSE, finite = TRUE)
  assertNumeric(sdev, any.missing = FALSE, finite = TRUE)
  mutUniformParametric(ind, p = p, lx = sqrt(12) * sdev, lower = lower, upper = upper)
}, supported = "float")

#' @rdname mutUniformParametric
#' @export
mutUniformParametricInt <- intifyMutator(mutUniformParametric)

#' @rdname mutUniformParametric
#' @export
mutUniformParametricIntScaled <- intifyMutator(mutUniformParametricScaled)

#' @title General Uniform Crossover
#'
#' @description
#' Crossover recombination operator that crosses over each position iid with prob. `p`
#' and can also be used for non-binary operators.
#' @param inds `[list of any]` list of two individuals to perform uniform crossover on
#' @param p `[numeric(1)]` per-entry probability to perform crossover.
#' @param ...  further arguments passed on to the method.
#' @return `[list of any]` The mutated individuals.
#' @export
recPCrossover <- makeRecombinator(function(inds, p = 0.1, ...) {
  assertList(inds, len = 2, any.missing = FALSE)
  if (length(inds[[1]]) != length(inds[[2]])) {
    stop("Length of components of individuals must be the same.")
  }
  n = length(inds[[1]])
  if (!(length(p) %in% c(n, 1))) {
    stopf("Argument p must have same length as individual or 1.")
  }
  crossovers = runif(length(inds[[1]])) < p
  tmp = inds[[1]][crossovers]
  inds[[1]][crossovers] = inds[[2]][crossovers]
  inds[[2]][crossovers] = tmp
  wrapChildren(inds[[1]], inds[[2]])
}, n.parents = 2, n.children = 2)

#' @title Uniform Reset for Binary Parameters
#'
#' @description
#' For each bit individually, decide with probability `p` to "reset" it to an
#' equilibrium distribution which is specified by `reset.dist`: a bit being
#' reset is set to 1 with probability `reset.dist` and set to 0 with probability
#' (1 - `reset.dist`).
#'
#' @param ind `[integer]` binary individual with values 0 or 1.
#' @param p `[numeric(1)]` entry-wise reset probability.
#' @param reset.dist `[numeric]` probability to draw 1-bit per entry, if reset is performed.
#'   `reset.dist` can be length 1 or same length as `ind` (which uses a different distribution for each bit).
#' @return `[integer]` the mutated individual.
#' @export
mutUniformReset <- makeMutator(function(ind, p = 0.1, reset.dist) {
  assertIntegerish(ind, lower = 0, upper = 1)
  if (length(reset.dist) == 1) {
    reset.dist = rep(reset.dist, length(ind))
  }
  assertNumeric(reset.dist, lower = 0 - .tol, upper = 1 + .tol,
    len = length(ind), any.missing = FALSE)
  assertNumeric(p, lower = 0 - .tol, upper = 1 + .tol)
  if (!(length(p) %in% c(length(ind), 1))) {
      stopf("Argument p must have same length as individual or 1.")
  }
  affect <- runif(length(ind)) < p
  naffect <- sum(affect)
  ind[affect] <- as.numeric(runif(naffect) < reset.dist[affect])
  ind
}, supported = "binary")

#' @title Parametrised Uniform Reset for Binary Parameters
#'
#' @description
#' Performs [`mutUniformReset`] with `reset.dist = reset.dists %*% reset.dist.weights`.
#' @param ind `[integer]` binary individual with values 0 or 1.
#' @param p `[numeric(1)]` entry-wise reset probability.
#' @param reset.dists `[matrix]` columns of probabilities to draw 1-bit per entry, if reset is performed.
#'   Must have `length(ind)` rows and `length(reset.dist.weights)` columns.
#' @param reset.dist.weights `[numeric]` weight vector to select among `reset.dists` columns.
#' @return `[integer]` the mutated individual
#' @export
mutUniformMetaReset <- makeMutator(function(ind, p = 0.1, reset.dists, reset.dist.weights) {
  assertNumeric(reset.dist.weights, lower = 0 - .tol, upper = 1 + .tol, any.missing = FALSE)
  reset.dist.weights <- pmin(reset.dist.weights, 1 - .Machine$double.eps)
  assertMatrix(reset.dists, mode = "numeric", nrows = length(ind), ncols = length(reset.dist.weights))
  reset.dist.weights <- -log1p(-reset.dist.weights)
  reset.dist.weights <- reset.dist.weights / max(sum(reset.dist.weights), .001)
  mutUniformReset(ind, p = p, reset.dist = reset.dists %*% reset.dist.weights)
}, supported = "binary")

#' @title Create a Filter Strategy Function
#'
#' @description
#' Creates a strategy function that uses the `weight.param.name` entry of
#' individuals as a weighting vector `reset.dist.weights` and `reset.dists`
#' for [`mutUniformMetaReset`] and [`mutUniformMetaResetSHW`].
#'
#' @param reset.dists `[matrix]` see `reset.dists` in [mutUniformMetaReset].
#' @param weight.param.name `[character(1)]` name of parameter to use as
#'   `reset.dist.weights` in [mutUniformMetaReset].
#' @return `function`
#' @export
makeFilterStrategy <- function(reset.dists, weight.param.name) {
  assertMatrix(reset.dists, mode = "numeric")
  assertCharacter(weight.param.name)
  function(ind) {
    list(reset.dists = reset.dists, reset.dist.weights = ind[[weight.param.name]])
  }
}


#' @title Scaled Gaussian Mutator
#'
#' @description
#' See [ecr::mutGauss]. Allows a vector of standard deviations. Scales
#' standard deviations to the range of `[lower, upper]`.
#' @inheritParams ecr::mutGauss
#' @param sdev `[numeric]` standard deviation(s) of the Gauss mutation.
#' @return `[numeric]` mutated individual.
#' @family operators
#' @export
mutGaussScaled <- makeMutator(function(ind, p = 1, sdev = 0.05, lower, upper) {
  assertNumeric(p, lower = 0 - .tol, upper = 1 + .tol, any.missing = FALSE)
  assertNumeric(sdev, lower = 0 - .tol, any.missing = FALSE, finite = TRUE)
  assertNumeric(lower, any.missing = FALSE, finite = TRUE)
  assertNumeric(upper, any.missing = FALSE, finite = TRUE)

  sdev.scaled <- sdev * (upper - lower)
  new.ind <- rnorm(length(ind), mean = ind, sd = sdev.scaled)
  which.step <- runif(length(ind)) < p
  ind[which.step] <- new.ind[which.step]
  pmin(pmax(lower, ind), upper)
}, supported = "float")

#' @title Integer Scaled Gaussian Mutator
#'
#' @description
#' See [mutGaussScaled].
#' @param ind `[integer]` integer vector/individual to mutate.
#' @param lower `[integer]` vector of minimal values for each parameter of the decision space. Must have the same length as `ind`.
#' @param upper `[integer]` vector of maximal values for each parameter of the decision space. Must have the same length as `ind`.
#' @param ...  further arguments passed on to the method.
#' @return `[integer]` mutated individual.
#' @family operators
#' @export
mutGaussIntScaled <- intifyMutator(mutGaussScaled)


#' @title Multi-Objective k-Tournament Selector
#'
#' @description
#' k individuals are chosen randomly and the best one
#' is chosen. This process is repeated `n.select` times.
#'
#' Choice is primarily by dominated sorting
#' and secondarily by either dominated hypervolume
#' or crowding distance, depending on `sorting`.
#'
#' Ties are broken randomly by adding random noise of relative magnitude
#' `.Machine$double.eps * 2^10` to points.
#'
#' @param fitness `[matrix]` fitness matrix, one column per individual.
#' @param n.select `[integer(1)]` number of individuals to select.
#' @param sorting `[character(1)]` one of `"domhv"` or `"crowding"` (default).
#' @param ref.point `[numeric]` reference point for hypervolume, must be given
#'   if `sorting` is `"domhv"`.
#' @param k `[integer(1)]` number of individuals to select at once.
#' @param return.unique `[logical(1)]` whether returned individual indices must be unique.
#' @return `[integer]` vector of selected individuals.
#' @family Selectors
#' @export
selTournamentMO <- makeSelector(function(fitness, n.select, sorting = "crowding", ref.point, k = 2, return.unique = FALSE) {
  assertMatrix(fitness, min.cols = 1, min.rows = 2)
  assertFlag(return.unique)
  assertInt(n.select, lower = 1, upper = if (return.unique) ncol(fitness) else Inf)
  assertInt(k, lower = 1)
  k <- min(k, ncol(fitness))
  rank.all <- overallRankMO(fitness, sorting, ref.point)

  pool <- seq_len(ncol(fitness))
  replicate(n.select, {
    if (k >= length(pool)) {
      competitors <- pool
    } else {
      competitors <- sample(pool, k, replace = FALSE)
    }
    choice <- competitors[which.min(rank.all[competitors])]
    if (return.unique) {
      pool <<- setdiff(pool, choice)
    }
    choice
  })
}, supported.objectives = "multi-objective")

#' @title Simple Selector without Replacement
#'
#' @param fitness `[matrix]` fitness matrix, one column per individual.
#' @param n.select `[integer(1)]` number of individuals to select.
#' @return `[matrix]` selected individuals.
#' @family Selectors
#' @export
selSimpleUnique <- makeSelector(function(fitness, n.select) {
  assertMatrix(fitness, mode = "numeric")
  assertIntegerish(n.select, lower = 1)
  sample(ncol(fitness), size = n.select, replace = FALSE)
}, supported.objectives = c("single-objective", "multi-objective"))

#' @title Rank by Nondominated Front and Crowding Distance or Hypervolume Contribution
#'
#' @description
#' Rank individuals by nondominating sorted front first and by hypervolume contribution
#' or crowding distance second.
#'
#' Ties are broken randomly by adding random noise of relative magnitude
#' `.Machine$double.eps * 2^10` to points.
#'
#' @param fitness `[matrix]` fitness matrix, one column per individual.
#' @param sorting `[character(1)]` one of `"domhv"` or `"crowding"` (default).
#' @param ref.point `[numeric]` reference point for hypervolume, must be given
#'   if `sorting` is `"domhv"`.
#' @return `[integer]` vector of ranks with length `ncol(fitness)`, lower ranks are
#'   associated with individuals that tend to dominate more points and that tend to
#'   have larger crowding distance or hypervolume contribution.
#' @export
overallRankMO <- function(fitness, sorting = "crowding", ref.point) {
  assertChoice(sorting, c("crowding", "domhv"))
  if (sorting == "domhv") {
    assertNumeric(ref.point, finite = TRUE, any.missing = FALSE, len = nrow(fitness))
  }

  assertMatrix(fitness, min.cols = 1, min.rows = 2)
  ranksort0 <- doNondominatedSorting(fitness)$ranks
  ranksort1 <- vector("numeric", length(ranksort0))
  for (rnk in unique(ranksort0)) {
    subfit <- fitness[, ranksort0 == rnk, drop = FALSE]
    subfit <- subfit + runif(length(subfit), -1, 1) * .Machine$double.eps * 2^9 * subfit
    if (sorting == "crowding") {
      secondary <- computeCrowdingDistance(subfit)
    } else {
      if (ncol(subfit) == 1) {
        # TODO: don't need this any more when https://github.com/jakobbossek/ecr2/issues/109 is fixed
        secondary <- prod(ref.point - subfit)
      } else {
        secondary <- computeHVContr(subfit, ref.point)
      }
    }
    ranksort1[ranksort0 == rnk] <- secondary
  }
  rankresult <- vector("integer", ncol(fitness))
  rankresult[order(ranksort0, -ranksort1)] <- seq_len(ncol(fitness))
  rankresult
}

#' @title Bitflip (Approximately, in Expectation) Conserving Hamming Weight
#'
#' @description
#' If a given bitvector has `m` 1s and `n` 0s, then a bit is flipped from
#' 0 to 1 with probability `2p(m+1)/(m+n+2)` and from
#' 1 to 0 with probability `2p(n+1)/(m+n+2)`. This is
#' equivalent with choosing bits uniformly at random with probability
#' 2 * `p` and drawing them from a bernoulli-distribution with parameter
#' `(m+1)/(m+n+2)`.
#'
#' @param ind `[integer]` binary individual.
#' @param p `[numeric]` average flip probability, must be between 0
#'   and 0.5.
#' @param ...  further arguments passed on to the method.
#' @return `[integer]` mutated binary individual.
#' @export
mutBitflipCHW <- makeMutator(function(ind, p = 0.1, ...) {
  assertNumeric(p, lower = 0 - .tol, upper = 0.5)
  assertIntegerish(ind, lower = 0, upper = 1, min.len = 1)
  m <- sum(ind)
  bern.p <- (m + 1) / (length(ind) + 2)

  affect <- runif(length(ind)) < 2 * p
  ind[affect] <- rbinom(sum(affect), 1, bern.p)
  ind
}, supported = "binary")

#' @title Uniform Reset Scaled by Hamming Weight
#'
#' @description
#' Combination of the idea of [`mutBitflipCHW`] with [`mutUniformReset`].
#'
#' If a given bitvector has `m` 1s and `n` 0s, then, with probability `p` for
#' each bit, it is drawn anew from the distribution
#' `((m + 1) * reset.dist) / (m * reset.dist + n * (1 - reset.dist) + 1)`.
#'
#' The reasoning behind this is that, without Laplace smoothing, drawing from
#' `m * reset.dist` / (m * reset.dist + n * (1 - reset.dist))` would lead to
#' probabilities of drawing a "0" or "1" such that
#' `mean(P("1") / P("0")) = m / n * mean(reset.dist / (1 - reset.dist))`.
#'
#' The `mutUniformMetaResetSHW` does reset with a weighted mean of distributions.
#'
#' @param ind `[integer]` binary individual.
#' @param p `[numeric]` average reset probability, must be between 0 and 1.
#' @param reset.dist `[numeric]` approximate probability to draw 1-bit per entry.
#' @param reset.dists `[matrix]` columns of probabilities, with `length(ind)` cols and
#'   `length(reset.dist.weights)` rows.
#' @param reset.dist.weights `[numeric]` weight vector to select among `reset.dist` columns.
#' @param ...  further arguments passed on to the method.
#' @return `[integer]` the mutated individual
#' @export
mutUniformResetSHW <- makeMutator(function(ind, p = 0.1, reset.dist, ...) {
  assertIntegerish(ind, lower = 0, upper = 1, min.len = 1)
  if (length(reset.dist) == 1) {
    reset.dist <- rep(reset.dist, length(ind))
  }
  assertNumeric(reset.dist, lower = 0 - .tol, upper = 1 + .tol, len = length(ind), any.missing = FALSE)
  assertNumeric(p, lower = 0 - .tol, upper = 1 + .tol)
  if (!(length(p) %in% c(length(ind), 1))) {
    stopf("Argument p must have same length as individual or 1.")
  }

  affect <- runif(length(ind)) < p

  m <- sum(ind)
  bern.p <- ((m + 1) * reset.dist) / (m * reset.dist + (length(ind) - m) * (1 - reset.dist) + 1)
  bern.p <- pmin(pmax(0, bern.p), 1)
  ind[affect] <- rbinom(sum(affect), 1, bern.p[affect])
  ind
}, supported = "binary")

#' @rdname mutUniformResetSHW
#' @export
mutUniformMetaResetSHW <- makeMutator(function(ind, p = 0.1, reset.dists, reset.dist.weights, ...) {
  assertNumeric(reset.dist.weights, lower = 0 - .tol, upper = 1 + .tol, any.missing = FALSE)
  reset.dist.weights <- pmin(reset.dist.weights, 1 - .Machine$double.eps)
  assertMatrix(reset.dists, nrows = length(ind), ncols = length(reset.dist.weights))
  reset.dist.weights <- -log(1 - reset.dist.weights)
  reset.dist.weights <- reset.dist.weights / max(sum(reset.dist.weights), .001)
  mutUniformResetSHW(ind, p = p, reset.dist = reset.dists %*% reset.dist.weights)
}, supported = "binary")


