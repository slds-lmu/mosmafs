
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
#' Crossover recombination operator that crosses over each position iid with prob. `p`
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
  assertNumeric(p, lower = 0, upper = 1)
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
  assertMatrix(reset.dists, nrows = length(ind), ncols = length(reset.dist.weights))
  reset.dist.weights <- -log(1 - reset.dist.weights)
  reset.dist.weights <- reset.dist.weights / sum(reset.dist.weights)
  mutUniformReset(ind, p = p, reset.dist = reset.dists %*% reset.dist.weights)
}, supported = "binary")

#' @title Create a Filter Strategy Function
#'
#' @description
#' Creates a strategy function that uses the `weight.param.name` entry of
#' individuals as a weighting vector `reset.dist.weights and `reset.dists`
#' for [`mutUniformMetaReset`] and [`mutUniformMetaResetSHW`].
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


#' @title Integer Scaled Gaussian Mutator
#'
#' @description
#' See [ecr::mutGauss]. Allows a vector of standard deviations. Scales
#' standard deviations to the range of `[lower, upper]`.
#' @family operators
#' @export
mutGaussScaled <- makeMutator(function(ind, p = 1, sdev = 0.05, lower, upper) {
  assertNumber(p, lower = 0, upper = 1, na.ok = FALSE)
  assertNumeric(sdev, lower = 0, any.missing = FALSE, finite = TRUE)
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
#' See [mutGaussScaled]
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
#' @param fitness `[matrix]` fitness matrix, one column per individuum
#' @param n.select `[integer(1)]` number of individuums to select
#' @param sorting `[character(1)]` one of `"domhv"` or `"crowding"` (default)
#' @param ref.point `[numeric]` reference point for hypervolume, must be given
#'   if `sorting` is `"domhv"`.
#' @param k `[integer(1)]` number of individuals to select at once.
#' @return `[integer]` vector of selected individuals
#' @param return.unique `[logical(1)]` whether returned individual indices must be unique
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

#' @title Rank by Nondominated Front and Crowding Distance or Hypervolume Contribution
#'
#' @description
#' Rank individuals by nondominating sorted front first and by hypervolume contribution
#' or crowding distance second.
#'
#' Ties are broken randomly by adding random noise of relative magnitude
#' `.Machine$double.eps * 2^10` to points.
#'
#' @param fitness `[matrix]` fitness matrix, one column per individuum
#' @param sorting `[character(1)]` one of `"domhv"` or `"crowding"` (default)
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
#' @param ind `[integer]` binary individual
#' @param p `[numeric]` average flip probability, must be between 0
#'   and 0.5.
#' @export
mutBitflipCHW <- makeMutator(function(ind, p = 0.1) {
  assertNumeric(p, lower = 0, upper = 0.5)
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
#' @param ind `[integer]` binary individual
#' @param p `[numeric]` average reset probability, must be between 0 and 1
#' @param reset.dist `[numeric]` approximate probability to draw 1-bit per entry
#' @param reset.dists `[matrix]` columns of probabilities, with `length(ind)` cols and
#'   `length(reset.dist.weights)` rows.
#' @param reset.dist.weights `[numeric]` weight vector to select among `reset.dist` columns.
#' @return `[integer]` the mutated individuum
#' @export
mutUniformResetSHW <- makeMutator(function(ind, p = 0.1, reset.dist) {
  if (length(reset.dist) == 1) {
    reset.dist <- rep(reset.dist, length(ind))
  }
  assertNumeric(reset.dist, lower = 0, upper = 1, len = length(ind), any.missing = FALSE)
  assertNumeric(p, lower = 0, upper = 1)
  affect <- runif(length(ind)) < p

  m <- sum(ind)
  bern.p <- ((m + 1) * reset.dist) / (m * reset.dist + (length(ind) - m) * (1 - reset.dist) + 1)

  ind[affect] <- rbinom(sum(affect), 1, bern.p[affect])
  ind
}, supported = "binary")

#' @rdname mutUniformResetSHW
#' @export
mutUniformMetaResetSHW <- makeMutator(function(ind, p = 0.1, reset.dists, reset.dist.weights) {
  assertNumeric(reset.dist.weights, lower = 0, upper = 1 - .Machine$double.eps, any.missing = FALSE)
  assertMatrix(reset.dists, nrows = length(ind), ncols = length(reset.dist.weights))
  reset.dist.weights <- -log(1 - reset.dist.weights)
  reset.dist.weights <- reset.dist.weights / sum(reset.dist.weights)
  mutUniformResetSHW(ind, p = p, reset.dist = reset.dists %*% reset.dist.weights)
}, supported = "binary")
