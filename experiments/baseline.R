
# "Baseline" performance measure: We just do normal parameter optimization
# with additional parameters: nselect (how many features to select),
# iselect (vector discrete parameter that selects explicit features out of order)
# and select.weights (numeric parameter vector that does weighting between
# filter values to use.
# @param learner `[Learner}` the base learner to use
# @param task `[Task]` the task to optimize
# @param filters `[character]` filter values to evaluate and use
# @param ps `[ParamSet]` the ParamSet of the learner to evaluate. Should
#   not include `selector.selection` etc., only parameters of the actual
#   learner.
# @param resampling `[ResampleDesc | ResampleInstance]` the resampling to use
# @param measure `[Measure]` the measure to evaluate
# @param num.explicit.featsel `[integer(1)]` additional number of parameters
#   to add for explicit feature selection.
# @param holdout.data `[Task | NULL]` the holdout data to consider
# @param worst.measure `[numeric(1)]` worst value to impute for failed evals
# @param cpo `[CPO]` CPO pipeline to apply before feature selection
# @param numfeats `[integer(1)]` number of features to consider. Is extracted
#   from the `task` but should be given if `cpo` changes the number of features.
# @return `function` that can be used for mlrMBO; irace possibly needs some
#   adjustmens
makeBaselineObjective <- function(learner, task, filters, ps, resampling, measure = NULL, holdout.data = NULL, worst.measure = NULL, cpo = NULL, numfeats = getTaskNFeats(task)) {
  if (is.null(measure)) {
    measure <- getDefaultMeasure(task)
  }
  assertClass(learner, "Learner")
  assertClass(cpo, "CPO", null.ok = TRUE)
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

  obj.factor <- if (measure$minimize) 1 else -1

  worst.measure <- worst.measure * obj.factor

  learner <- cpoSelector() %>>% checkLearner(learner, type = getTaskType(task))
  if (!is.null(cpo)) {
    learner %<<<% cpo
  }
  argnames <- getParamIds(getParamSet(learner))

  assertSubset(getParamIds(ps), argnames)
  ps <- c(ps, pSS(
    nselect: integer[0, numfeats],
    iselect: discrete[sapply(seq_len(numfeats), identity, simplify = FALSE)],
    select.weights: numeric[~0, ~1]^length(filters)
  ))


}
