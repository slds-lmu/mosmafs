
#' @title Create ecr Objective Function
#'
#' @description
#' Create an objective function that resamples `learner` on `task`
#' with `resampling` and measures `measure` (optional), together
#' with the number of features selected.
#'
#' The `ParamSet` used to generate individuals for the ecr must include,
#' besides parameters for `learner`, a parameter `selector.selection`,
#' a `logical` parameter with length equal `getTaskNFeats(task)`.
#'
#' `learner` must *not* include a `cpoSelector()` applied to it, this
#' happens automatically within `makeObjective`.
#'
#' @param learner `[Learner]` A [`Learner`][mlr::makeLearner] object to optimize.
#' @param task `[Task]` The [`mlr::Task`] object to optimize on.
#' @param ps `[ParamSet]` The [`ParamSet`][ParamHelpers::makeParamSet] to optimize over.
#' @param resampling `[ResampleDesc | ResampleInst | function]` The [`ResampleDesc`][mlr::makeResampleDesc] or
#'   [`ResampleInst`][mlr::makeResampleInstance] object to use. This may be a function
#'   `numeric(1)` -> `ResampleDesc`/`ResampleInst` which maps fidelity to the resampling to use.
#'   If this is used, then the resampling should be chosen such that an average value, weighted by fidelity,
#'   makes sense. For example, the function could map an integer to a corresponding number of resampling folds
#'   or repetitions.
#' @param measure `[Measure | NULL]` The [`Measure`][mlr::makeMeasure] to optimize for.
#'   The default is `NULL`, which uses the `task`'s default `Measure`.
#' @param holdout.data `[Task]` Additional data on which to predict each
#'   configuration after training on `task`.
#' @param worst.measure `[numeric(1)]` worst value for measure to consider,
#'   for dominated hypervolume calculation. Will be extracted from the
#'   given measure if not given, but will raise an error if the extracted
#'   (or given) value is infinite.
#' @param cpo `[CPO]` CPO pipeline to apply before feature selection.
#'   (A CPO that should be applied *after* feature selection should already be
#'   part of `learner` when given). Care should be taken that the
#'   `selector.selection` parameter in `ps` has the appropriate length of
#'   the data that `cpo` emits.
#' @return `function` an objective function for [`ecr::ecr`].
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
      if (identical(fidelity, 0)) {
        return(c(0, 0))
      }
      args <- valuesFromNames(ps, args)
      args <- trafoValue(ps, args)
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
  adapt <- getParamIds(paramset)[getParamTypes(paramset) %in% c("discrete", "discretevector")]
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
#' "Baseline" performance measure: We just do normal parameter optimization
#' with additional parameters: mosmafs.nselect (how many features to select),
#' mosmafs.iselect (vector integer parameter that selects explicit features out
#' of order) and mosmafs.select.weights (numeric parameter vector that does
#' weighting between filter values to use.
#' @param learner `[Learner]` the base learner to use
#' @param task `[Task]` the task to optimize
#' @param filters `[character]` filter values to evaluate and use
#' @param ps `[ParamSet]` the ParamSet of the learner to evaluate. Should
#'   not include `selector.selection` etc., only parameters of the actual
#'   learner.
#' @param resampling `[ResampleDesc | ResampleInstance]` the resampling to use
#' @param measure `[Measure]` the measure to evaluate
#' @param num.explicit.featsel `[integer(1)]` additional number of parameters
#'   to add for explicit feature selection.
#' @param holdout.data `[Task | NULL]` the holdout data to consider
#' @param worst.measure `[numeric(1)]` worst value to impute for failed evals
#' @param cpo `[CPO]` CPO pipeline to apply before feature selection
#' @param numfeats `[integer(1)]` number of features to consider. Is extracted
#'   from the `task` but should be given if `cpo` changes the number of features.
#' @return `function` that can be used for mlrMBO; irace possibly needs some
#'   adjustmens
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
  assertSubset(filters, names(mlr:::.FilterRegister))
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
  ps <- c(ps, pSS(mosmafs.nselect: integer[0, numfeats]),
    makeParamSet(params = lapply(seq_len(num.explicit.featsel), function(idx) {
      # not using vector parameters here because mlrMBO probably
      # sucks at handling them.
      makeIntegerParam(sprintf("mosmafs.iselect.%s", idx),
        lower = 1, upper = numfeats)
    })),
    if (length(filters) > 1) {
      makeParamSet(params = lapply(seq_along(filters), function(idx) {
        # not using vector parameters here because mlrMBO probably
        # sucks at handling them.
        makeIntegerParam(sprintf("mosmafs.select.weights.%s", idx),
          lower = 1, upper = numfeats)
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

      # trafo not necessary in mlrMBO

      # set up `selector.selection` from nselect, iselect, select.weights and fmat
      nselect <- args$mosmafs.nselect
      iselect <- args[sprintf("mosmafs.iselect.%s", seq_len(num.explicit.featsel))]
      if (length(filters) > 1) {
        select.weights <- unlist(args[sprintf("mosmafs.select.weights.%s",
          seq_along(filters))])
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


