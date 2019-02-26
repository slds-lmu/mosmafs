

#' @title Get Pareto Front Edges from Fitness Matrix
#'
#' @description
#' Get the edges defining a 2D pareto front for plotting
#'
#' @param fitness `[matrix | data.frame]` Matrix or (numeric) `data.frame`
#'   with two columns and rows for each individuum
#' @param refpoint `[numeric]` Reference point
#' @return `data.frame` with three columns: The points on the pareto front,
#'   and a `logical` column `point` indicating whether the point is on the pareto front
#'   (`TRUE`) or an auxiliary point for plotting (`FALSE`)
#' @family Utility Functions
#' @export
paretoEdges <- function(fitness, refpoint) {
  fitness <- as.matrix(t(fitness))
  front <- fitness[, nondominated(fitness), drop = FALSE]
  front <- front[, order(front[1, ]), drop = FALSE]
  frontpoints <- sapply(seq_len(ncol(front) * 2 - 1) + 1, function(twicecol) {
    pmax(front[, floor(twicecol / 2)],
      front[, ceiling(twicecol / 2)])
  })

  resmat <- cbind(c(front[1, 1], refpoint[2]),
    frontpoints,
    c(refpoint[1], front[2, ncol(front)]))
  indicator <- rep_len(c(FALSE, TRUE), ncol(resmat))
  data.frame(t(resmat), point = indicator)
}

#' @title Extract Fitnesses from ECR Log
#'
#' @description
#' Extract fitnesses for each generation from ECR log
#'
#' @param results `[ecr_multi_objective_result]` ecr run log
#' @param trafo `[function]` function `matrix`|`data.frame` -> `matrix`|`data.frame`
#'   to transforms individual generation matrices.
#' @return `data.frame` of fitnesses from ecr run log, with extra column `iter`.
#' @family Utility Functions
#' @export
fitnesses <- function(results, trafo = identity) {
  pops <- getPopulations(results$log)
  do.call(rbind, lapply(seq_along(pops), function(idx) {
    pop <- pops[[idx]]
    df <- as.data.frame(trafo(t(pop$fitness)))
    colnames(df)[1:2] <- c("perf", "propfeat")
    df$iter <- idx
    df
  }))
}

