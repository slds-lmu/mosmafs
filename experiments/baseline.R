
# "Baseline" performance measure: We just do normal parameter optimization
# with additional parameters: mosmafs.nselect (how many features to select),
# mosmafs.iselect (vector discrete parameter that selects explicit features out
# of order) and mosmafs.select.weights (numeric parameter vector that does
# weighting between filter values to use.
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
  ps <- c(ps, pSS(mosmafsnselect: integer[0, numfeats]),
    if (num.explicit.featsel > 0) {
      pSS(mosmafsiselect: discrete[
          sapply(seq_len(numfeats), identity, simplify = FALSE)
        ]^num.explicit.featsel)
    },
    if (length(filters) > 1) {
      pSS(mosmafsselect.weights: numeric[0, ~1]^length(filters))
    }
  )

  fmat <- makeFilterMat(task %>>% cpo, filters)
  assertMatrix(fmat, nrows = numfeats)

  smoof::makeMultiObjectiveFunction(sprintf("mosmafs_baseline_%s_%s",
    sprintf("mosmafs_%s_%s", learner$id, task$task.desc$id),
    has.simple.signature = FALSE, par.set = ps, n.objectives = 2, noisy = TRUE,
    ref.point = c(worst.measure, 1),
    fn = function(args) {
      # trafo not necessary in mlrMBO

      # set up `selector.selection` from nselect, iselect, select.weights and fmat
      nselect <- args$mosmafs.nselect
      select.weights <- args$mosmsafs.select.weights
      iselect <- args$mosmafs.iselect
      if (is.null(select.weights)) {
        fvals <- c(fmat)
      } else {
        fvals <- c(fmat %*% select.weights)
      }
      selections <- order(fvals, decreasing = TRUE)
      selections <- c(iselect, selections)
      args$selector.selection <- rep(FALSE, numfeats)
      args$selector.selection[selections[seq_len(nselect)]] <- TRUE

      # filter out mosmafs.* parameters we don't need any more
      args <- args[intersect(names(args), argnames)]
      learner <- setHyperPars(learner, par.vals = args)

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
        userextra <- c(userextra, list(fitness.holdout = c(
          perf = unname(val.holdout * obj.factor),
          propfeat = propfeat)))

      }

      propfeat <- mean(args$selector.selection)

      result <- c(perf = unname(val * obj.factor), propfeat = propfeat)
      attr(result, "extras") <- userextra
      result
    })
}
