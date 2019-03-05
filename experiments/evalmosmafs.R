

#' @title Parameter Configuration of Mosmafs
#'
#' @description
#' Create a SMOOF function for parameter configuration of mosmafs, with parameter set.
#'
#' The resulting function takes a list of values according to its `getParamSet()`.
#' Additionally the list can contain an `$INSTANCE`, an integer between 1 and 1000.
#' If it is not given, the instance will be chosen randomly. It corresponds to the
#' resampling instance to use if `fixed.ri` is `TRUE`.
#'
#' @param task `[Task]` the task to optimize
#' @param learner `[Learner]` the learner to optimize
#' @param ps `[ParamSet]` the parameter set of the learner (and `cpo`) alone
#' @param measure `[Measure]` measure to optimize
#' @param cpo `[CPO]` cpo to prepend feature selection
#' @param nfeat `[integer(1)]` number of features
#' @param evals `[integer(1)]` number of evals to perform. Note this concerns fidelity
#'   evaluations (i.e. single CV folds). When not using multifid the number of points
#'   evaluated is 1/10th the `evals` value.
#' @param outer.resampling: outer resampling to use.
#' @param savedir `[character(1) | NULL]` the directory to save every trace to.
#'   If this is `NULL` (the default) evaluations are not saved.
#' @return `function` a smoof function.
constructEvalSetting <- function(task, learner, ps, measure = getDefaultMeasure(task), worst.measure = NULL, cpo = NULLCPO, nfeat = getTaskNFeats(task %>>% cpo), evals = 1e5, outer.resampling = makeResampleDesc("CV", iters = 10, stratify = TRUE), savedir = NULL) {

  assertClass(task, "Task")
  assertClass(learner, "Learner")
  assertClass(ps, "ParamSet")
  assertClass(measure, "Measure")
  assertClass(cpo, "CPO")
  assertInt(nfeat, lower = 1)
  assertInt(evals, lower = 1)
  if (!is.null(savedir)) {
    assertString(savedir)
    assertDirectoryExists(savedir, access = "w")
  }
  if (is.null(worst.measure)) {
    worst.measure <- measure$worst
  }
  assertNumber(worst.measure, finite = TRUE)
  obj.factor <- if (measure$minimize) 1 else -1
  ref.point = c(worst.measure * obj.factor, 1)

  mosmafs.params <- pSS(

    init.distribution.constructor: discrete [list(
      binomial = function(param) function() rbinom(1, nfeat, param),
      geometric = function(param) function() {
        res <- 1 + rgeom(1000, 1 / max(1, (nfeat * param)))
        c(res[res <= nfeat], nfeat)[1]
      },
      uniform = function(param) function() floor(runif(1, 0, length(nfeat) + 1)))],
    init.distribution.param: numeric[0.001, 0.999] [[requires =
      quote(init.distribution.constructor %in% c("binomial", "geometric"))]],

    init.soften.iters: integer[0, 2],
    use.SHW.init: logical,
    use.SHW: logical,

    filters: discrete [list(
      none = character(0),
      many.filters = c("auc", "praznik_JMI",
        "FSelectorRcpp_information.gain",
        "chi.squared", "DUMMY"),
      few.filters = c("praznik_JMI", "FSelectorRcpp_information.gain"))],
    filter.strategy: logical [[requires = quote(filters != "none")]],

    ops.parentsel: discrete[list(
      selSimple = selSimple,
      selNondom = selNondom,
      selTournamentMO = selTournamentMO)],
    ops.survsel: discrete[list(
      selSimple = selSimpleUnique,
      selNondom = selNondom,
      selTournamentMO = selTournamentMO)],
    ops.tournament.k: numeric[1, 5] [[trafo = function(x) round(2^x),
      requires = quote(ops.parentsel == "selTournamentMO" || ops.survsel == "selTournamentMO")]],
    ops.tournament.sorting: discrete[crowding, domHV]
      [[requires = quote(ops.parentsel == "selTournamentMO")]],

    ops.mut.int: discrete[list(
      mutGaussIntScaled = mutGaussIntScaled,
      mutDoubleGeomScaled = mutDoubleGeom,
      mutPolynomialInt = makeMutator(function(ind, p, sdev, lower, upper) {
        mutPolynomialInt(ind, p = p, eta = max(1, (sqrt(8 + sdev^2) / sdev - 5) / 2),
          lower = lower, upper = upper)
      }, supported = "custom"),
      mutUniformParametricIntScaled = mutUniformParametricIntScaled)],
    ops.mut.numeric: discrete[list(
      mutGaussScaled = mutGaussScaled,
      mutPolynomialInt = makeMutator(function(ind, p, sdev, lower, upper) {
        mutPolynomial(ind, p = p, eta = max(1, (sqrt(8 + sdev^2) / sdev - 5) / 2),
          lower = lower, upper = upper)
      }, supported = "float"),
      mutUniformParametricScaled = mutUniformParametricIntScaled)],
    ops.mut.strategy: logical,
    ops.mut.sdev: numeric[log(0.005), 0]
      [[trafo = function(x) exp(x), requires = quote(!ops.mut.strategy)]],
    ops.mut.p: numeric[0, 1] [[requires = quote(!ops.mut.strategy)]],

    ops.rec.nums: discrete[list(
      recSBX = recSBX,
      recGaussian = recGaussian,
      recPCrossover = recPCrossover)],
    ops.rec.strategy: logical,
    ops.rec.crossover.p: numeric[0, 1]
      [[requires = quote(!ops.rec.strategy)]],
    ops.rec.sbx.eta: numeric[1, 10]
      [[requires = quote(!ops.rec.strategy && ops.rec.nums == "recSBX")]],

    mu: numeric[3, 8] [[trafo = function(x) round(2^x)]],
    lambda: numeric[3, 8] [[trafo = function(x) round(2^x)]],

    generation.fid: logical,
    generation.fid.point: numeric[0, 1] [[requires = quote(generation.fid == TRUE)]],

    dominance.fid: logical,

    fixed.ri: logical,

    p.recomb: numeric[0, 1],
    p.mut: numeric[0, 1]
  )

  outer.res.inst <- makeResampleInstance(outer.resampling, task)
  getTrainTask <- function(i) {
    subsetTask(task, outer.res.inst$train.inds[[i]])
  }
  getHoutTask <- function(i) {
    subsetTask(task, outer.res.inst$test.inds[[i]])
  }

  stratcv10 <- makeResampleDesc("CV", iters = 10, stratify = TRUE)
  res.insts <- lapply(seq_len(outer.res.inst$desc$iters), function(iter) {
    replicate(1000, makeResampleInstance(stratcv10, getTrainTask(iter)))
  })

  reduceResult <- function(x) {  # remove task from result when saving
    if (is.list(x)) {
      for (i in seq_along(x)) {
        x[i] <- list(reduceResult(x[[i]]))
      }
    } else if (is.environment(x)) {
      if (environmentName(x) != "") {
        return(x)
      }
      for (i in names(x)) {
        x[[i]] <- reduceResult(x[[i]])
      }
    } else if (is.function(x)) {
      if (environmentName(environment(x)) != "") {
        return(x)
      }
      environment(x) <- globalenv()
    }
    if (length(setdiff(names(attributes(x)), "names"))) {
      attributes(x) <- reduceResult(attributes(x))
    }
    x
  }

  smoof::makeSingleObjectiveFunction("mosmafs",
    has.simple.signature = FALSE, noisy = TRUE, par.set = mosmafs.params,
    minimize = FALSE, fn = function(x) {
      for (n in names(x)) {
        assign(n, x[[n]])
      }

      # Construct ParamSet
      eval.ps <- c(ps, pSS(selector.selection: logical^nfeat))

      if (length(filters)) {
        fima <- makeFilterMat(task, filters = filters)
        assign.op <- function(shw) if (shw) mutUniformMetaResetSHW else mutUniformMetaReset
        selector.mutator <- assign.op(use.SHW)
        selector.mutator.init <- assign.op(use.SHW.init)
        if (filter.strategy) {
          filterstrat <- makeFilterStrategy(reset.dists = fima,
            weight.param.name = "filterweights")
          eval.ps <- c(eval.ps, pSS(filterweights: numeric[0, ~1]^length(filters)))
          init.strategy <- makeFilterStrategy(reset.dists = fima,
            weight.param.name = "filterweights")
        } else {
          selector.mutator <- ecr::setup(selector.mutator,
            reset.dists = fima, reset.dist.weights = rep(0.5, length(filters)))
          selector.mutator.init <- ecr::setup(selector.mutator.init,
            reset.dists = fima, reset.dist.weights = rep(0.5, length(filters)))
          init.strategy <- function(ind) list()
        }
      } else {
        assign.op <- function(shw) {
          if (shw) {
            makeMutator(function(ind, p) mutBitflipCHW(ind, p / 2), supported = "binary")
          } else {
            makeMutator(function(ind, p) mutBitflip(ind, p / 2), supported = "binary")
          }
        }
        selector.mutator <- assign.op(use.SHW)
        selector.mutator.init <- assign.op(use.SHW.init)
        init.strategy = function(ind) list()
      }

      # Construct mutator I
      if (ops.mut.strategy) {
        destrategize.num.mut <- identity
        destrategize.disc.mut <- identity
        strategy.num.mut <- function(ind) list(p = ind$strategy.p, sdev = ind$strategy.sdev)
        strategy.disc.mut <- function(ind) list(p = ind$strategy.p)
        eval.ps <- c(eval.ps, pSS(
          strategy.p: numeric[0, 1],
          strategy.sdev: numeric[log(0.005), 0] [[trafo = function(x) exp(x)]]))
      } else {
        destrategize.num.mut <- function(x) ecr::setup(x, p = ops.mut.p, sdev = ops.mut.sdev)
        destrategize.disc.mut <- function(x) ecr::setup(x, p = ops.mut.p)
        strategy.num.mut <- function(ind) list()
        strategy.disc.mut <- function(ind) list()
      }
      # Construct recombinator I
      num.needs.eta <- identical(ops.rec.nums, recSBX)
      num.needs.p <- identical(ops.rec.nums, recPCrossover)
      if (ops.rec.strategy) {
        destrategize.num.rec <- identity
        destrategize.disc.rec <- identity
        strategy.num.rec <- function(ind)
          if (num.needs.p) list(p = ind$strategy.rec.p)
          else if (num.needs.eta) list(eta = ind$strategy.rec.eta)
          else list()
        strategy.disc.rec <- function(ind) c(list(p = ind$strategy.rec.p))
        eval.ps <- c(eval.ps, pSS(
          strategy.rec.p: numeric[0, 1],
          strategy.rec.eta: numeric[1, 10]))
      } else {
        destrategize.num.rec <- function(x)
          if (num.needs.p) ecr::setup(x, p = ops.rec.crossover.p)
          else if (num.needs.eta) ecr::setup(x, eta = ops.rec.sbx.eta)
          else x
        destrategize.disc.rec <- function(x) ecr::setup(x, p = ops.rec.crossover.p)
        strategy.num.rec <- function(ind) list()
        strategy.disc.rec <- function(ind) list()
      }


      # needs to go after eval.ps is fully constructed, so
      # after first part of recombinator construction
      mutator <- combine.operators(eval.ps,
        integer = destrategize.num.mut(ops.mut.int),
        .strategy.integer = strategy.num.mut,
        numeric = destrategize.num.mut(ops.mut.numeric),
        .strategy.numeric = strategy.num.mut,
        discrete = destrategize.disc.mut(mutRandomChoice),
        .strategy.discrete = strategy.disc.mut,
        logical = destrategize.disc.mut(mutBitflip),
        .strategy.logical = strategy.disc.mut,
        selector.selection = selector.mutator,
        .strategy.selector.selection = function(ind) {
          if (length(filters) && filter.strategy) {
            filterstrat(ind)
          } else {
            list()
          }
        })

      recombinator <- combine.operators(eval.ps,
        integer = destrategize.num.rec(intifyRecombinator(ops.rec.nums)),
        .strategy.integer = strategy.num.rec,
        numeric = destrategize.num.rec(ops.rec.nums),
        .strategy.numeric = strategy.num.rec,
        discrete = destrategize.disc.rec(recPCrossover),
        .strategy.discrete = strategy.disc.rec
        logical = destrategize.disc.rec(recUnifCrossover),
        .strategy.logical = strategy.disc.rec)

      if (identical(ops.parentsel, selTournamentMO)) {
        ops.parentsel <- ecr::setup(ops.parentsel,
          k = min(ops.tournament.k, lambda),
          sorting = ops.tournament.sorting,
          ref.point = ref.point)
      }

      if (identical(ops.survsel, selTournamentMO)) {
        ops.survsel <- ecr::setup(ops.survsel,
          k = min(ops.tournament.k, lambda),
          sorting = ops.tournament.sorting,
          ref.point = ref.point, return.unique = TRUE)
      }

      initials <- sampleValues(eval.ps, mu, discrete.names = TRUE)
      initials <- initSelector(initials,
        distribution = init.distribution.constructor(x$init.distribution.param),
        soften.op = ecr::setup(selector.mutator.init, p = 1),
        soften.op.strategy = init.strategy,
        soften.op.repeat = init.soften.iters)

      if (!generation.fid) {
        if (dominance.fid) {
          fidelity <- data.frame(1, 1, 9)
        } else {
          fidelity <- data.frame(1, 10)
        }
      } else {
        jumpgen <- round((evals - mu) / lambda * generation.fid.point)
        jumpgen <- max(jumpgen, 2)
        if (dominance.fid) {
          fidelity <- data.frame(c(1, jumpgen), c(1, 1), c(1, 9))
        } else {
          fidelity <- data.frame(c(1, jumpgen), c(1, 10))
        }
      }

      if (is.null(x$INSTANCE)) {
        x$INSTANCE <- sample(length(res.insts), size = 1)
        filesuffix <- paste0("_", gsub(":", "-", gsub(" ", "_", as.character(Sys.time()))))
      } else {
        filesuffix <- ""
      }
      assertInt(x$INSTANCE, lower = 1, upper = length(res.insts))

      hiters <- seq_len(outer.res.inst$desc$iters)
      iterresults <- parallelMap::parallelSapply(hiters, function(houtiter) {
        set.seed(houtiter)
        nRes <- function(n) {
          if (fixed.ri) {
            inst <- res.insts[[houtiter]][[x$INSTANCE]]
          } else {
            inst <- makeResampleInstance(stratcv10, task)
          }
          if (n == 1) {
            inst$desc$iters <- 1
            inst$desc$train.inds <- inst$desc$train.inds[1]
            inst$desc$test.inds <- inst$desc$test.inds[1]
          } else if (n == 9) {
            inst$desc$iters <- 9
            inst$desc$train.inds <- inst$desc$train.inds[2:10]
            inst$desc$test.inds <- inst$desc$test.inds[2:10]
          } else if (n != 10) {
            stop(sprintf("Invalid fidelity %s", n))
          }
          inst
        }

        fitness.fun <- makeObjective(
          learner = learner,
          task = getTrainTask(houtiter),
          ps = eval.ps,
          resampling = nRes,
          measure = measure,
          holdout.data = getHoutTask(houtiter),
          worst.measure = worst.measure, cpo = cpo)

        run <- slickEcr(
          fitness.fun = fitness.fun,
          lambda = lambda,
          population = initials,
          mutator = mutator,
          recombinator = recombinator,
          generations = list(mosmafsTermFidelity(evals)),
          parent.selector = ops.parentsel,
          survival.selector = ops.survsel,
          p.recomb = p.recomb,
          p.mut = p.mut,
          fidelity = fidelity)

        if (!is.null(savedir)) {
          saveRDS(list(params = x, run = reduceResult(run)),
            file = file.path(savedir,
              paste0("MOSMAFS_RUN_",
                digest::digest(x),
                filesuffix,
                sprintf("_%s.rds", houtiter))))
        }
        res <- collectResult(run)
        res$cum.fid <- pmin(res$cum.fid, evals)
        intbase <- c(res$cum.fid[1], diff(res$cum.fid))
        sum(intbase * res$true.hout.domHV)

      })
      mean(iterresults)
    })
}



