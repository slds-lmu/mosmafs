

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
#' @param staggens `[integer(1)]` number of generations without progress in
#'   hypervolume or mean objective value
#' @return `function` a terminator function
#' @export
mosmafsTermEvals <- function(evals) {
  assertInt(fidelity)
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
  assertNumber(fidelity)
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
    state
    if (state >= fidelity) {
      sprintf("Total fidelity %s reached limit %s", state, evals)
    }
  }
}

#' @export
#' @rdname mosmafsTermEvals
mosmafsTermStagnationHV <- function(staggen) {
  assertInt(staggen)
  function(result) {
    if (nrow(result) < 1) return(NULL)
    # drop everything up to the last fid.reeval
    if ("fid.reeval" %in% colnames(result) && any(result$fid.reeval)) {
      drop <- seq_len(max(which(result$fid.reeval)))
      result <- result[-drop, ]
    }
    stagnation <- nrow(result) - which.max(result$eval.domH)
    if (stagnation >= staggen) {
      sprintf("HV performance did not increase for %s generations (limit %s)",
        stagnation, staggen)
    }
  }
}

#' @export
#' @rdname mosmafsTermEvals
mosmafsTermStagnationObjMean <- function(staggen) {
  assertInt(staggen)
  function(result) {
    if (nrow(result) < 1) return(NULL)
    # drop everything up to the last fid.reeval
    if ("fid.reeval" %in% colnames(result) && any(results$fid.reeval)) {
      drop <- seq_len(max(which(result$fid.reeval)))
      result <- result[-drop, ]
    }

    obj.colnames <- grep("^eval\\..*\\.mean$", colnames(result), value = TRUE)
    stags <- lapply(result[obj.colnames], function(col) {
      nrow(result) - which.min(col)
    })
    stagnation <- min(stags)
    if (stagnation >= staggen) {
      sprintf("Mean objective performance did not increase for %s generations (limit %s)",
        stagnation, staggen)
    }
  }
}
