


my.nsga2 = function(fitness.fun, ...) {
  args = list(
    mutator = setup(mutPolynomial, eta = 25, p = 0.2),
    recombinator = setup(recSBX, eta = 15, p = 0.7),
    representation = "float",
    parent.selector = selSimple,
    survival.selector = selNondom)

  do.call(ecr::ecr, insert(args, list(...)))
}



nsga2 = function(
  fitness.fun,
  n.objectives = NULL,
  n.dim = NULL,
  minimize = NULL,
  lower = NULL,
  upper = NULL,
  mu = 100L,
  lambda = mu,
  mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = lower, upper = upper),
  recombinator = setup(recSBX, eta = 15, p = 0.7, lower = lower, upper = upper),
  terminators = list(stopOnIters(100L)), representation = "float",
  ...) {

  res = ecr(fitness.fun = fitness.fun, n.objectives = n.objectives,
    n.dim = n.dim, minimize = minimize, lower = lower, upper = upper,
    mu = mu, lambda = lambda, representation = representation, survival.strategy = "plus",
    parent.selector = selSimple,
    mutator = mutator,
    recombinator = recombinator,
    survival.selector = selNondom,
    terminators = terminators, ...)
  return(res)
}
