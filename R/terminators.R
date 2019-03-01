

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
    if (max(result$evals) >= evals) {
      sprintf("Number of total evaluations %s reached limit %s",
        max(result$evals), evals)
    }
  }
}

mosmafsTermGenerations <- function(generations) {
  assertInt(generations)
  function(result) {
    if (max(result$gen) >= generations) {
      sprintf("Number of total generations %s reached limit %s",
        max(result$generations), generations)
    }
  }
}

#' @export
#' @rdname mosmafsTermEvals
mosmafsTermTime <- function(time) {
  assertNumber(fidelity)
  function(result) {
    if (max(result$runtime) >= time) {
      sprintf("Total runtime %s reached limit %s",
        max(result$runtime), evals)
    }
  }
}

#' @export
#' @rdname mosmafsTermEvals
mosmafsTermFidelity <- function(fidelity) {
  assertNumber(fidelity)
  function(result) {
    if (max(result$cum.fid) >= fidelity) {
      sprintf("Total fidelity %s reached limit %s",
        max(result$cum.fid), evals)
    }
  }
}

#' @export
#' @rdname mosmafsTermEvals
mosmafsTermStagnationHV <- function(staggen) {
  assertInt(staggen)
  function(result) {
    # drop everything up to the last fid.reeval
    if ("fid.reeval" %in% colnames(result)) {
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
    # drop everything up to the last fid.reeval
    if ("fid.reeval" %in% colnames(result)) {
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
