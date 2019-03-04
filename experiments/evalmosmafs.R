

constructEvalSetting <- function(task, learner, ps, cpo) {

  nfeat <- getTaskNFeats(task)

  mosmafs.params <- pSSLrn(
    init.distribution.constructor: discrete [list(
      binomial = function(param) function() rbinom(1, nfeat, param),
      geometric = function(param) function() min(rgeom(1, 1 / (nfeat * param)), nfeat),
      uniform = function(param) function() floor(runif(1, 0, length(nfeat) + 1)))],
    init.distribution.param: numeric[0.001, 0.999] [[requires =
      quote(init.distribution.constructor %in% c("binomial", "geometric"))]]
    init.soften.iters: integer[0, 2],
    use.SHW: logical,
    filters: discrete [list(
      none = character(0),
      filters = c("auc", "praznik_JMI",
        "FSelectorRcpp_information.gain",
        "chi.squared", "DUMMY"))],
    ops.parentsel: discrete[list(
      selSimple = selSimple,
      selTrounament = selTrounamentMO)],
    ops.parentsel.param.k: numeric[1, 5] [[trafo = function(x) round(2^x),
      requires = quote(ops.parentsel == "selTournament")]],
    ops.parentsel.param.sorting: discrete[crowding, domHV],

    ops.mut.int: discrete[list(
      mutGaussIntScaled = mutGaussIntScaled,
      mutDoubleGeomScaled = mutDoubleGeom,
      mutPolynomialInt = makeMutator(function(ind, p, sdev, lower, upper) {
        mutPolynomialInt(ind, p = p, eta = max(1, (sqrt(8 + sdev^2) / sdev - 5) / 2), lower = lower, upper = upper)
      }, supported = "custom")
      mutUniformParametricIntScaled = mutUniformParametricIntScaled)],
    ops.mut.mumeric: discrete[list(
      mutGaussScaled = mutGaussScaled,
      mutPolynomialInt = makeMutator(function(ind, p, sdev, lower, upper) {
        mutPolynomial(ind, p = p, eta = max(1, (sqrt(8 + sdev^2) / sdev - 5) / 2), lower = lower, upper = upper)
      }, supported = "float"),
      mutUniformParametricScaled = mutUniformParametricIntScaled)],
    ops.mut.strategy: logical,
    ops.mut.sdev: numeric[log(0.005), 0] [[trafo: function(x) exp(x), requires = quote(!ops.mut.strategy)]],
    ops.mut.p: numeric[0, 1] [[requires = quote(!ops.mut.strategy)]],
    # crossover: intermediate, sbx(eta, p), unifcrossover(p)
    ops.rec.nums: discrete[list(
      recSBX = list(numeric = recSBX, integer = recIntSBX),
      recIntermediate = list(numeric = recIntermediate, integer = recIntIntermediate)





