
intifyMutator <- function(operator) makeMutator(function(ind, ..., lower, upper) {
  ind <- operator(ind + 0.5, ..., lower = lower, upper = upper + 1)
  pmin(pmax(lower, floor(ind)), upper)
}, supported = "custom")

intifyRecombinator <- function(operator) makeRecombinator(function(inds, ..., lower, upper) {
  children <- operator(list(inds[[1]] + 0.5, inds[[2]] + 0.5), ..., lower = lower, upper = upper + 1)
  if (attr(children, "multiple")) {
    return(do.call(wrapChildren, lapply(children, function(x) pmin(pmax(lower, floor(x)), upper))))
  }
  wrapChildren(pmin(pmax(lower, children), upper))
},
n.parents = ecr:::getNumberOfParentsNeededForMating.ecr_recombinator(operator),
n.children = ecr:::getNumberOfChildren.ecr_recombinator(operator))

mutGaussInt <- intifyMutator(mutGauss)
mutPolynomialInt <- intifyMutator(mutPolynomial)
mutUniformInt <- intifyMutator(mutUniform)
recIntSBX <- intifyRecombinator(recSBX)

# "random choice" mutation operator for discrete parameters: with prob. p chooses
# one of the available categories at random (this /may/ be the original value!)
mutRandomChoice <- makeMutator(function(ind, values, p = 0.1) {
  mapply(function(i, v) {
    if (runif(1) < p) {
      sample(v, 1)
    } else {
      i
    }
  }, ind, values)
}, supported = "custom")


mutDoubleGeom <- makeMutator(function(ind, p = 1, geomp = 0.9, lower, upper) {
  affect <- runif(length(ind)) < p
  naffect <- sum(affect)
  ind[affect] <- ind[affect] + rgeom(naffect, prob = geomp) - rgeom(naffect, prob = geomp)
  pmin(pmax(lower, ind), upper)
}, supported = "custom")

# crossover mutation operator that crosses over each position iid with prob. p
# and can also be used for non-binary operators.
recPCrossover <- makeRecombinator(function(ind, p = 0.1, ...) {
  crossovers = runif(length(ind[[1]])) < p
  tmp = ind[[1]][crossovers]
  ind[[1]][crossovers] = ind[[2]][crossovers]
  ind[[2]][crossovers] = tmp
  wrapChildren(ind[[1]], ind[[2]])
}, n.parents = 2, n.children = 2)

# uniformly with probability `p`, draw each bit again: 1 w/prob `reset.dist`, 0 otherwise
# reset.dist can be length 1 or same length as `ind` (which uses a different distribution for each bit).
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

# perform `mutUniformReset`, with reset.dist = reset.dists %*% reset.dist.weights.
# reset.dists must be a matrix with `length(ind)` rows and `length(reset.dist.weights)` rows
# reset.dist.weights must be a numeric greater 0 and smaller than 1
mutUniformMetaReset <- makeMutator(function(ind, p = 0.1, reset.dists, reset.dist.weights) {
  assertNumeric(reset.dist.weights, lower = 0, upper = 1 - .Machine$double.eps, any.missing = FALSE)
  assertMatrix(reset.dists, nrows = length(ind), ncol = length(reset.dist.weights))
  reset.dist.weights <- -log(1 - reset.dist.weights)
  reset.dist.weights <- reset.dist.weights / sum(reset.dist.weights)  # TODO: this could go into trafo
  mutUniformReset(ind, p = p, reset.dist = reset.dists %*% reset.dist.weights)
}, supported = "binary")

makeFilterStrategy <- function(filtermat, weight.param.name, output.name) {
  function(ind) {
    weights <- ind[[weight.param.name]]
    assertNumeric(weights, lower = 0, upper = 1 - .Machine$double.eps, any.missing = FALSE)
    assertMatrix(filtermat, ncol = length(weights))
    weights <- -log(1 - weights)
    weights <- weights / sum(weights)
    namedList(output.name, filtermat %*% weights)
  }
}

# --------------- tests / experiments -------------------

testps <- pSS(x: discrete[a, b, c], y: discrete[m, n, o], z: discrete[x, y, z]^3,
  one: logical, two: numeric[1, 10], three: numeric[0, 1])


eco <- combine.operators(testps,
  discrete = mutRandomChoice,
  x = mutRandomChoice,
  logical = mutBitflip,
  numeric = mutGauss)


initials <- sampleValues(testps, 1, discrete.names = TRUE)

initials
resdf = do.call(rbind, replicate(1000, as.data.frame(unlist(lapply(eco(initials[[1]]), as.list), recursive = FALSE)), simplify = FALSE))


# debug(mutRandomChoice)

eco(initials[[1]])

resdf$two <- NULL
resdf$three <- NULL
lapply(resdf, table)



initials <- sampleValues(testps, 2, discrete.names = TRUE)

reco <- combine.operators(testps,
  discrete = recPCrossover,
  x = ecr::setup(recPCrossover, p = .5),
  logical = recCrossover,
  numeric = recSBX)





