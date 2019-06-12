

#' @title CPO that Selects Features
#' @param selection `[logical]`\cr
#' Logical vector indicating if a features 
#' was selected or not. Must have the same length as number of features.
#' @inheritParams mlrCPO::CPOConstructor	
#' @export
cpoSelector <- makeCPO("selector", pSSLrn(selection: logical^NA),
  cpo.train = NULL,
  cpo.retrafo = { data[selection] })

