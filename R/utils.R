
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
