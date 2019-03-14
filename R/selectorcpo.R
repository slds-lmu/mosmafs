

#' @title CPO that Selects Features
#' @export
cpoSelector <- makeCPO("selector", pSSLrn(selection: logical^NA),
  cpo.train = NULL,
  cpo.retrafo = { data[selection] })

