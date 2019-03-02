

#' @title Termination Function Creator
#'
#' @description
#' These create functions that can be given to `slickEcr`'s `generations` argument
#'
#' The stagnation terminators only count stagnation from the last time
#' the fidelity was changed in a way that led to population re-evaluation.
#'
#' @param generations `[integer(1)]` limit generations. Initial population does
#'   not count.
#' @param evals `[integer(1)]` limit evals
#' @param time  `[numeric(1)]` limit evaluation time (which does not count
#'   holdout fitting time
#' @param fidelity `[numeric(1)]` total fidelity evaluation to limit
#' @param stag `[integer(1)]` number of generations (or other measures)
#'   without progress in hypervolume or mean objective value
#' @param stag.index `[character(1)]` one of `"generations"` (default),
#'   `"evals"`, `"time"`, `"fidelity"`: What index to count `stag` against when
#'   aborting after stagnation.
#' @return `function` a terminator function
#' @export
mosmafsTermEvals <- function(evals) {
  assertInt(evals)
  function(result) {
    state <- max(c(result$evals, 0))
    if (state >= evals) {
      sprintf("Number of total evaluations %s reached limit %s", state, evals)
    }
  }
}

mosmafsTermGenerations <- function(generations) {
  assertInt(generations)
  function(result) {
    state <- max(c(result$gen, 0))
    if (state >= generations) {
      sprintf("Number of total generations %s reached limit %s", state, generations)
    }
  }
}

#' @export
#' @rdname mosmafsTermEvals
mosmafsTermTime <- function(time) {
  assertNumber(time)
  function(result) {
    state <- max(c(result$runtime, 0))
    if (state >= time) {
      sprintf("Total runtime %s reached limit %s", state, evals)
    }
  }
}

#' @export
#' @rdname mosmafsTermEvals
mosmafsTermFidelity <- function(fidelity) {
  assertNumber(fidelity)
  function(result) {
    state <- max(c(result$cum.fid, 0))
    if (state >= fidelity) {
      sprintf("Total fidelity %s reached limit %s", state, evals)
    }
  }
}

#' @export
#' @rdname mosmafsTermEvals
mosmafsTermStagnationHV <- function(stag, stag.index = "generations") {
  assertInt(stag)
  assertChoice(stag.index, c("generations", "evals", "time", "fidelity"))
  stag.access <- switch(stag.index,
    generations = "gen",
    evals = "evals",
    time = "runtime",
    fidelity = "cum.fid")
  stag.name <- switch(stag.index,
    generations = "generations",
    evals = "evaluations",
    time = "seconds",
    fidelity = "evaluated fidelity steps")
  function(result) {
    if (nrow(result) < 1) return(NULL)
    # drop everything up to the last fid.reeval
    if ("fid.reeval" %in% colnames(result) && any(result$fid.reeval)) {
      drop <- seq_len(max(which(result$fid.reeval)))
      result <- result[-drop, ]
    }
    stagnation <- max(result[[stag.access]]) -
      result[[stag.access]][which.max(result$eval.domH)]
    if (stagnation >= stag) {
      sprintf("HV performance did not increase for %s %s (limit %s)",
        stagnation, stag.name, stag)
    }
  }
}

#' @export
#' @rdname mosmafsTermEvals
mosmafsTermStagnationObjMean <- function(stag, stag.index = "generations") {
  assertInt(stag)
  assertChoice(stag.index, c("generations", "evals", "time", "fidelity"))
  stag.access <- switch(stag.index,
    generations = "gen",
    evals = "evals",
    time = "runtime",
    fidelity = "cum.fid")
  stag.name <- switch(stag.index,
    generations = "generations",
    evals = "evaluations",
    time = "seconds",
    fidelity = "evaluated fidelity steps")
  function(result) {
    if (nrow(result) < 1) return(NULL)
    # drop everything up to the last fid.reeval
    if ("fid.reeval" %in% colnames(result) && any(results$fid.reeval)) {
      drop <- seq_len(max(which(result$fid.reeval)))
      result <- result[-drop, ]
    }

    obj.colnames <- grep("^eval\\..*\\.mean$", colnames(result), value = TRUE)
    stags <- lapply(result[obj.colnames], function(col) {
      stagnation <- max(result[[stag.access]]) -
        result[[stag.access]][which.min(col)]
    })
    stagnation <- min(stags)
    if (stagnation >= stag) {
      sprintf("Mean objective performance did not increase for %s %s (limit %s)",
        stagnation, stag.name, stag)
    }
  }
}
