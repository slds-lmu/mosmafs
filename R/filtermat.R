
#' @title Create a Filter-Matrix
#'
#' @description
#' A Filter-Matrix can be used in combination with [`mutUniformMetaReset`] for
#' heuristic-supported biased mutation.
#'
#' @param task `[Task]` The task to generate filter information for.
#' @param filters `[character]` The filters to use. Special vilter `"DUMMY"`
#'   gives a constant column of `expectfeatfrac`.
#' @param expectfeatfrac `[numeric(1)]` The expected fraction of features to have in equilibrium. Ignored if `expectfeats` is given.
#' @param expectfeats `[numeric(1)]` The expected number of features to have in equilibrium.
#' @param minprob `[numeric(1)]` The minimum probability for each feature.
#' @param maxprob `[numeric(1)]` The maximum probability for each feature.
#' @return `matrix`
#' @examples 
#' library("mlr")
#' 
#' # Example for iris task
#' filters <- c("praznik_JMI", "anova.test", "variance", "DUMMY")
#' fima <- makeFilterMat(iris.task, filters = filters)
#' print(fima)
#' 
#' @export
makeFilterMat <- function(task, filters, expectfeatfrac = 0.5, expectfeats = getTaskNFeats(task) * expectfeatfrac, minprob = 0, maxprob = 1) {
  assertNumber(expectfeats / getTaskNFeats(task), lower = minprob, upper = maxprob)
  do.dummy <- "DUMMY" %in% filters
  filters <- setdiff(filters, "DUMMY")
  filtervals <- generateFilterValuesData(task, method = filters)
  if (!is.numeric(filtervals$data[[3]])) {
    colnam <- colnames(filtervals$data)
    filtervals <- reshape(filtervals$data, direction = "wide", idvar = colnam[1], timevar = colnam[3], drop = colnam[2])
    filtervals <- filtervals[match(getTaskFeatureNames(task), filtervals[[1]]), ]
    filtervals <- filtervals[-1]
  } else {
    filtervals <- filtervals$data[-(1:2)]
  }
  if (do.dummy) {
    filtervals$DUMMY <- 0.5
  }

  apply(filtervals, 2, function(col) {
    nas <- is.na(col)
    col[!nas] <- rank(col[!nas], ties.method = "average")
    col[nas] <- mean(col[!nas])
    col <- col - mean(col)
    meanprob <- expectfeats / getTaskNFeats(task)
    if (0 %in% range(col)) {
      return(col + meanprob)
    }
    shrinkage <- max(range(col) / (c(minprob, maxprob) - meanprob))
    col / shrinkage + meanprob
  })
}
