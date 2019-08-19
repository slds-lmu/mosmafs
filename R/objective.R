
#' @title Create ecr Objective Function
#'
#' @description
#' Creates an objective function that resamples `learner` on `task`
#' with `resampling` and measures `measure` (optional), together
#' with the number of features selected.
#' If measure needs to be maximized, it is multiplied by -1 to make it
#' a minimization task.
#'
#' The `ParamSet` used to generate individuals for the ecr must include
#' parameters for `learner`, not a `logical` parameter with length equal
#' to `getTaskNFeats(task)` for feature selection, as it is automatically added
#' named as `selector.selection`.
#' It can be accessed via `getParamSet()` with the object created by
#' `makeObjective()` as input.
#'
#' `learner` must *not* include a `cpoSelector()` applied to it, this
#' happens automatically within `makeObjective`.
#'
#' @param learner `[Learner]` A [`Learner`][mlr::makeLearner] object to optimize.
#' @param task `[Task]` The [`mlr::Task`] object to optimize on.
#' @param ps `[ParamSet]` The [`ParamSet`][ParamHelpers::makeParamSet] to optimize over, only parameters of the
#' actual learner.
#' @param resampling `[ResampleDesc | ResampleInst | function]` The [`ResampleDesc`][mlr::makeResampleDesc] or
#'   [`ResampleInst`][mlr::makeResampleInstance] object to use. This may be a function
#'   `numeric(1)` -> `ResampleDesc`/`ResampleInst` which maps fidelity to the resampling to use.
#'   If this is used, then the resampling should be chosen such that an average value, weighted by fidelity,
#'   makes sense. For example, the function could map an integer to a corresponding number of resampling folds
#'   or repetitions.
#' @param measure `[Measure | NULL]` The [`Measure`][mlr::makeMeasure] to optimize for.
#'   The default is `NULL`, which uses the `task`'s default `Measure`.
#'   If measure needs to be maximized, the measure is multiplied
#'   by -1, to make it a minimization task.
#' @param holdout.data `[Task]` Additional data on which to predict each
#'   configuration after training on `task`.
#' @param worst.measure `[numeric(1)]` worst value for measure to consider,
#'   for dominated hypervolume calculation. Will be extracted from the
#'   given measure if not given, but will raise an error if the extracted
#'   (or given) value is infinite. Measure is multiplied by -1, if measure needs
#'   to be maximized.
#' @param cpo `[CPO]` CPO pipeline to apply before feature selection.
#'   (A CPO that should be applied *after* feature selection should already be
#'   part of `learner` when given). Care should be taken that the
#'   `selector.selection` parameter in `ps` has the appropriate length of
#'   the data that `cpo` emits.
#' @return `function` an objective function for [`ecr::ecr`].
#' @examples
#' library("mlr")
#' library("rpart")
#'
#' task.whole <- bh.task
#' rows.whole <- sample(nrow(getTaskData(task.whole)))
#' task <- subsetTask(task.whole, rows.whole[1:250])
#' task.hout <- subsetTask(task.whole, rows.whole[251])
#' lrn <- makeLearner("regr.rpart")
#'
#' ps.simple <- mlrCPO::pSS(
#'   maxdepth: integer[1, 30],
#'   minsplit: integer[2, 30],
#'   cp: numeric[0.001, 0.999])
#'   nRes <- function(n) {
#'   makeResampleDesc("Subsample", split = 0.9, iters = n)
#' }
#'
#' fitness.fun.mos <- makeObjective(lrn, task, ps.simple, nRes,
#'   measure = mse,
#'   holdout.data = task.hout, worst.measure = 100)
#'
#' # extract param set from objective
#' ps.obj  <- getParamSet(fitness.fun.mos)
#' getParamIds(ps.obj) # automatically added parameter ' for selecting features
#'
#' exp <- sampleValue(ps.obj)
#' res <- fitness.fun.mos(exp, fidelity = 2, holdout = FALSE)
#'
#'
#' @export
makeObjective <- function(learner, task, ps, resampling, measure = NULL, holdout.data = NULL, worst.measure = NULL, cpo = NULLCPO) {
  if (is.null(measure)) {
    measure <- getDefaultMeasure(task)
  }
  assertClass(learner, "Learner")
  assertClass(cpo, "CPO")
  assertClass(task, "Task")
  assertClass(holdout.data, "Task", null.ok = TRUE)
  assertClass(ps, "ParamSet")
  assert(
      checkClass(resampling, "ResampleInstance"),
      checkClass(resampling, "ResampleDesc"),
      checkFunction(resampling, nargs = 1)
  )
  assertClass(measure, "Measure")
  if (is.null(worst.measure)) {
    worst.measure <- measure$worst
  }
  assertNumber(worst.measure, finite = TRUE)

  obj.factor <- if (measure$minimize) 1 else -1

  worst.measure <- worst.measure * obj.factor

  # error if selector.selection already in ps, will be automatically added
  if ("selector.selection" %in% ParamHelpers::getParamIds(ps)) {
    stop("selector.selection is not allowed to be part of 'ps' as it is automatically added")
  }
  ps = c(ps, pSS(selector.selection = NA: logical^getTaskNFeats(task)))

  learner <- cpoSelector() %>>% checkLearner(learner, type = getTaskType(task))
  learner %<<<% cpo

  argnames <- getParamIds(getParamSet(learner))
  smoof::makeMultiObjectiveFunction(
    sprintf("mosmafs_%s_%s", learner$id, task$task.desc$id),
    has.simple.signature = FALSE, par.set = ps, n.objectives = 2, noisy = TRUE,
    ref.point = c(worst.measure, 1),
    fn = function(args, fidelity = NULL, holdout = FALSE) {
      if (holdout && is.null(holdout.data)) {
        return(c(perf = Inf, propfeat = Inf))
      }
      if (!missing(fidelity) && identical(fidelity, 0)) {
        return(c(perf = 0, propfeat = 0))
      }
      # filter out strategy parameters
      args <- args[intersect(names(args), argnames)]
      learner <- setHyperPars(learner, par.vals = args)
      if (holdout) {
        model <- train(learner, task)
        prd <- predict(model, holdout.data)
        val <- performance(prd, list(measure), task, model)[1]
      } else {
        if (is.function(resampling)) {
          assertNumber(fidelity)
          res <- resampling(fidelity)
        } else {
          res <- resampling
        }

        val <- resample(learner, task, res,
          list(measure), show.info = FALSE)$aggr
      }
      if (is.na(val)) {
        val <- worst.measure
      }
      propfeat <- mean(args$selector.selection)
      c(perf = unname(val * obj.factor), propfeat = propfeat)
  })
}

#' @title Convert Discrete Parameters from Names to Values
#'
#' @description
#' Convert parameter values sampled with [ParamHelpers::sampleValue()] and
#' `discrete.names = TRUE` to true parameter values.
#'
#' @param paramset `[ParamSet]` The [`ParamSet`][ParamHelpers::makeParamSet] used to generate the value.
#' @param value `[named list]` Names list of parameters sampled from `paramset`.
#' @return `named list` of parameter values, with `character` entries representing names
#'   of values of discrete params converted to the actual values.
#' @export
valuesFromNames <- function(paramset, value) {
  assertClass(paramset, "ParamSet")
  assertList(value, len = length(paramset$pars))
  assertSubset(names(value), getParamIds(paramset), empty.ok = FALSE)
  adapt <- getParamIds(paramset)[getParamTypes(paramset) %in% c("discrete", "discretevector")]
  adapt <- intersect(adapt, names(value))
  for (pname in adapt) {
    param <- paramset$pars[[pname]]
    if (isVector(param)) {
      value[[pname]] <- param$values[value[[pname]]]
    } else {
      value[[pname]] <- param$values[[value[[pname]]]]
    }
  }
  value
}



#' @title Create mlrMBO Objective Function
#'
#' @description
#' "Baseline" performance measure: Creates an objective function that performs
#' normal parameter optimization by evaluating filters with additional parameters:
#' `mosmafs.nselect` (how many features to select),
#' `mosmafs.iselect` (vector integer parameter that selects explicit features
#' that are not necessary the best according to filter values)
#' and `mosmafs.select.weights` (numeric parameter vector that does
#' weighting between filter values to use.
#' @param learner `[Learner]` the base learner to use.
#' @param task `[Task]` the task to optimize.
#' @param filters `[character]` filter values to evaluate and use.
#' @param ps `[ParamSet]` the ParamSet of the learner to evaluate. Should
#'   not include `selector.selection` etc., only parameters of the actual
#'   learner.
#' @param resampling `[ResampleDesc | ResampleInstance]` the resampling strategy to use.
#' @param measure `[Measure]` the measure to evaluate.
#' If measure needs to be maximized, the measure is multiplied by -1,
#' to make it a minimization task.
#' @param num.explicit.featsel `[integer(1)]` additional number of parameters
#'   to add for explicit feature selection.
#' @param holdout.data `[Task | NULL]` the holdout data to consider.
#' @param worst.measure `[numeric(1)]` worst value to impute for failed evals.
#' @param cpo `[CPO]` CPO pipeline to apply before feature selection.
#' @param numfeats `[integer(1)]` number of features to consider. Is extracted
#'   from the `task` but should be given if `cpo` changes the number of features.
#' @return `function` that can be used for mlrMBO; irace possibly needs some
#'   adjustments.
#' @export
makeBaselineObjective <- function(learner, task, filters, ps, resampling, measure = NULL, num.explicit.featsel = 0, holdout.data = NULL, worst.measure = NULL, cpo = NULLCPO, numfeats = getTaskNFeats(task)) {
  if (is.null(measure)) {
    measure <- getDefaultMeasure(task)
  }
  assertClass(learner, "Learner")
  assertClass(cpo, "CPO")
  assertClass(task, "Task")
  assertClass(holdout.data, "Task", null.ok = TRUE)
  assertCharacter(filters, any.missing = FALSE, min.len = 1)
  assertSubset(filters, names(get(".FilterRegister", envir = getNamespace("mlr"))))
  assertInt(num.explicit.featsel, lower =  0)
  assertClass(ps, "ParamSet")
  assert(
      checkClass(resampling, "ResampleInstance"),
      checkClass(resampling, "ResampleDesc")
  )
  assertClass(measure, "Measure")
  if (is.null(worst.measure)) {
    worst.measure <- measure$worst
  }
  assertNumber(worst.measure, finite = TRUE)
  assertInt(numfeats, lower = 1)

  obj.factor <- if (measure$minimize) 1 else -1

  worst.measure <- worst.measure * obj.factor

  learner <- cpoSelector() %>>% checkLearner(learner, type = getTaskType(task))
  learner %<<<% cpo

  argnames <- getParamIds(getParamSet(learner))

  assertSubset(getParamIds(ps), argnames)
  ps <- c(ps, pSS(mosmafs.nselect = NA: integer[0L, numfeats]),
    makeParamSet(params = lapply(seq_len(num.explicit.featsel), function(idx) {
      # not using vector parameters here because mlrMBO probably
      # sucks at handling them.
      makeIntegerParam(sprintf("mosmafs.iselect.%s", idx),
        lower = 1L, upper = numfeats)
    })),
    if (length(filters) > 1) {
      makeParamSet(params = lapply(seq_along(filters), function(idx) {
        # not using vector parameters here because mlrMBO probably
        # sucks at handling them.
        makeNumericParam(sprintf("mosmafs.select.weights.%s", idx),
          lower = 0, upper = 1 - .Machine$double.eps)
      }))
    }
  )

  fmat <- makeFilterMat(task %>>% cpo, filters)
  assertMatrix(fmat, nrows = numfeats)

  smoof::makeMultiObjectiveFunction(
    sprintf("mosmafs_baseline_%s_%s", learner$id, task$task.desc$id),
    has.simple.signature = FALSE, par.set = ps, n.objectives = 2, noisy = TRUE,
    ref.point = c(worst.measure, 1),
    fn = function(x) {
      # mlrMBO is the platonic ideal of awful design.
      # The function parameter actually must be named 'x'.
      args <- x
      dif.names <- getParamIds(ps)[!getParamIds(ps) %in% names(args)]
      if (length(dif.names) > 0) {
        stop(sprintf("%s must be an element in list 'x'", dif.names))
      }

      # trafo not necessary in mlrMBO

      # set up `selector.selection` from nselect, iselect, select.weights and fmat
      nselect <- args$mosmafs.nselect
      iselect <- args[sprintf("mosmafs.iselect.%s", seq_len(num.explicit.featsel))]
      if (length(filters) > 1) {
        select.weights <- unlist(args[sprintf("mosmafs.select.weights.%s",
          seq_along(filters))])
        select.weights <- pmin(select.weights, 1 - .Machine$double.eps)
        select.weights <- -log1p(-select.weights)
        select.weights <- select.weights / max(sum(select.weights), .001)
        fvals <- c(fmat %*% select.weights)
      } else {
        fvals <- c(fmat)
      }
      selections <- order(fvals, decreasing = TRUE)
      selections <- selections[unique(c(unlist(iselect), seq_along(selections)))]
      args$selector.selection <- rep(FALSE, numfeats)
      args$selector.selection[selections[seq_len(nselect)]] <- TRUE

      # filter out mosmafs.* parameters we don't need any more
      args <- args[intersect(names(args), argnames)]
      learner <- setHyperPars(learner, par.vals = args)

      propfeat <- mean(args$selector.selection)

      net.time <- system.time(
        val <- resample(learner, task, resampling,
          list(measure), show.info = FALSE)$aggr,
        gcFirst = FALSE)[3]
      if (is.na(val)) {
        val <- worst.measure
      }
      userextra <- list(net.time = net.time)

      if (!is.null(holdout.data)) {
        model <- train(learner, task)
        prd <- predict(model, holdout.data)
        val.holdout <- performance(prd, list(measure), task, model)[1]
        if (is.na(val.holdout)) {
          val.holdout <- worst.measure
        }
        userextra <- c(userextra, list(
          fitness.holdout.perf = unname(val.holdout * obj.factor),
          fitness.holdout.propfeat = propfeat))
      }

      result <- c(perf = unname(val * obj.factor), propfeat = propfeat)
      attr(result, "extras") <- userextra
      result
    })
}


