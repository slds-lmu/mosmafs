
deepclone <- function(obj) {
  unserialize(serialize(obj, NULL))
}

clonelog <- function(log) {
  otask <- log$env$task
  log$env$task <- NULL
  log <- deepclone(log)
  log$env$task <- otask
  log
}

#' @title Aggregate Population Results
#'
#' @description
#' Extract attributes saved for individuums in a
#' log object to a more accessible matrix or data.frame.
#'
#' @param log `[ecr_logger]` ecr log object
#' @param extract `[character]` what to extract
#' @param simplify `[logical(1)]` whether to create a
#'   `matrix`/`data.frame` for each generation (default).
#'    Otherwise a list is returned for each generation containing
#'    the value (if `length(extract) == 1`) or a named list of values.
#' @param data.frame `[logical(1)]` Whether to return a `data.frame`
#'   with rows for each individuum (if `TRUE`) or to return a `matrix`
#'   with columns for each individuum compatible as fitness matrix
#'   with various ecr tools (if `FALSE`, default). Only effective if
#'  `simplify` is `TRUE`.
#' @export
popAggregate <- function(log, extract, simplify = TRUE, data.frame = FALSE) {
  assertCharacter(extract, min.len = 1, any.missing = FALSE)
  assertFlag(simplify)
  assertFlag(data.frame)
  pop <- getPopulations(log)
  lapply(pop, function(generation) {
    aggr <- sapply(generation$population, function(individuum) {
      info <- attributes(individuum)[extract]
      if (length(extract) == 1) {
        info[[1]]
      } else if (simplify) {
        unlist(info, recursive = FALSE)
      } else {
        info
      }
    }, simplify = simplify)
    if (simplify && data.frame) {
      if (is.matrix(aggr)) {
        as.data.frame(t(aggr))
      } else {
        df <- data.frame(aggr)
        colnames(df) <- extract
        df
      }
    } else {
      aggr
    }
  })
}

#' @title  List Attributes that can be Aggregated
#'
#' @description
#' Looks at the first individuum in the first generation and
#' returns its attributes. if `check` is TRUE it checks that the
#' log object is consistent and throws an error if not. "consistent"
#' here means that all individuals in all generations have the same
#' attributes
#' @param log `[ecr_logger]` ecr log object
#' @param check `[logical(1)]` whether to check consistency.
#'   This should almost never be useful.
#' @return `[character]` attributes of individuals in population.
#' @export
availableAttributes <- function(log, check = FALSE) {
  pop <- getPopulations(log)
  if (length(pop) == 0) return(character(0))
  if (length(pop[[1]]$population) == 0) {
    if (check && any(vlapply(pop, function(gen) length(gen$population) != 0))) {
      stop("Some generations are empty.")
    }
    return(character(0))
  }
  attrs <- names(attributes(pop[[1]]$population[[1]]))
  if (check) {
    lapply(pop, function(gen) lapply(gen$population, function(ind)
      assertSetEqual(names(attributes(ind)), attrs)))
  }
  attrs
}
