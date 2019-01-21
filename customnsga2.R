


my.nsga2 = function(fitness.fun, ...) {
  args = list(
    mutator = setup(mutPolynomial, eta = 25, p = 0.2),
    recombinator = setup(recSBX, eta = 15, p = 0.7),
    representation = "float",
    parent.selector = selSimple,
    survival.selector = selNondom)

  do.call(ecr::ecr, insert(args, list(...)))
}
