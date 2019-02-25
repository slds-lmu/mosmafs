
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
#' @param learner `[Learner]` An [`mlr::Learner`] object to optimize.
#' @param task `[Task]` The [`mlr::Task`] object to optimize on.
#' @param ps `[ParamSet]` The [`ParamHelpers::ParamSet`] to optimize over.
#' @param resampling `[ResampleDesc | ResampleInst]` The [`mlr::ResampleDesc`] or
#'   [`mlr::ResampleInst`] object to use.
#' @param measure `[Measure | NULL]` The [`mlr::Measure`] to optimize for.
#'   The default is `NULL`, which uses the `task`'s default `Measure`.
#' @return `function` an objective function for [`ecr::ecr`].
#' @export
makeObjective <- function(learner, task, ps, resampling, measure = NULL) {
  if (is.null(measure)) {
    measure <- getDefaultMeasure(measure)
  }
  obj.factor <- if (measure$minimize) 1 else -1
  learner <- cpoSelector() %>>% checkLearner(learner, type = getTaskType(task))
  smoof::makeMultiObjectiveFunction(sprintf("mosmafs_%s", lrn$id),
    has.simple.signature = FALSE, par.set = ps, n.objectives = 2, noisy = TRUE,
    fn = function(args) {
    args <- args[intersect(names(args), getParamIds(getParamSet(lrn)))]
    val <- resample(setHyperPars(lrn, par.vals = args), task, resampling,
      list(measure), show.info = FALSE)$aggr
    propfeat <- mean(args$selector.selection)
    c(val * obj.factor, propfeat)
  })
}
