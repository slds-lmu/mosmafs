
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
makeObjective <- function(learner, task, ps, resampling, measure = NULL, holdout.data = NULL, worst.measure = NULL, cpo = NULL) {
  if (is.null(measure)) {
    measure <- getDefaultMeasure(task)
  }
  assertClass(learner, "Learner")
  assertClass(cpo, "CPO", null.ok = TRUE)
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
  if (!is.null(cpo)) {
    learner %<<<% cpo
  }
  argnames <- getParamIds(getParamSet(learner))
  smoof::makeMultiObjectiveFunction(sprintf("mosmafs_%s", learner$id),
    has.simple.signature = FALSE, par.set = ps, n.objectives = 2, noisy = TRUE,
    ref.point = c(worst.measure, 1),
    fn = function(args, fidelity = NULL, holdout = FALSE) {
      if (holdout && is.null(holdout.data)) {
        return(c(perf = Inf, propfeat = Inf))
      }
      args <- valuesFromNames(ps, args)
      args <- trafoValue(ps, args)
      args <- args[intersect(names(args), argnames)]  # filter out strategy parameters
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
