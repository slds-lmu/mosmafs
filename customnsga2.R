


my.nsga2 = function(...) {
  args = list(
    mutator = ecr::setup(mutPolynomial, eta = 25, p = 0.2),
    recombinator = ecr::setup(recSBX, eta = 15, p = 0.7),
    representation = "float",
    parent.selector = selSimple,
    survival.selector = selNondom)

  do.call(ecr::ecr, insert(args, list(...)))
}
