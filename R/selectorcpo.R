

#' @title CPO that Selects Features
#' @param selector [`NULL`]
#' @inheritParams mlrCPO::CPOConstructor	
#' @export
cpoSelector <- makeCPO("selector", pSSLrn(selection: logical^NA),
  cpo.train = NULL,
  cpo.retrafo = { data[selection] })

