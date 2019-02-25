
#' @title Create a Filter-Matrix
#'
#' @description
#' A Filter-Matrix can be used in combination with [`mutUniformMetaReset`] for
#' heuristic-supported biased mutation.
#'
#' @param task `[Task]` The task to generate filter information for
#' @param filters `[character]` The filters to use
#' @param expectfeats `[integer(1)]` The expected number of features to have in equilibrium.
#' @param minprob `[numeric(1)]` The minimum probability for each feature
#' @param maxprob `[numeric(1)]` The maximum probability for each feature
#' @return `matrix`
#' @export
makeFilterMat <- function(task, filters, expectfeats = getTaskNFeats(task) / 2, minprob = 0, maxprob = 1) {
  assertNumber(expectfeats / getTaskNFeats(task), lower = minprob, upper = maxprob)
  filtervals <- generateFilterValuesData(task, method = filters)
  filtervals <- filtervals$data[-(1:2)]

  apply(filtervals, 2, function(col) {
    col <- col - mean(col)
    meanprob <- expectfeats / getTaskNFeats(task)
    if (0 %in% range(col)) {
      return(col + meanprob)
    }
    shrinkage <- max(range(col) / (c(minprob, maxprob) - meanprob))
    col / shrinkage + meanprob
  })
}
