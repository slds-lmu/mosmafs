

#' @title CPO that Selects Features
#' @param selection `[logical]`\cr
#'   Logical vector indicating if a features 
#'   was selected or not. Must have the same length as number of features.
#' @inheritParams mlrCPO::cpoTemplate
#' @return `[CPO]`
#' @examples 
#' library("mlr")
#' library("mlrCPO")
#' 
#' # Dataset has originally four features
#' iris.task$task.desc$n.feat
#' 
#' iris.task.subset = iris.task %>>% cpoSelector(c(TRUE, TRUE, FALSE, FALSE))
#' 
#' # Now only two were selected
#' iris.task.subset$task.desc$n.feat
#' @export
cpoSelector <- makeCPO("selector", pSSLrn(selection: logical^NA),
  cpo.train = NULL,
  cpo.retrafo = { data[selection] })

