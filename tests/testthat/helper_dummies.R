

mkdb <- function(isrec) {
  dbfun <- function(x, lower = 0, upper = 0, values = "", extra = 0) {
    catf("x: %s\nlower: %s\nupper: %s\nvalues: %s\nextra: %s",
      collapse(x), collapse(lower), collapse(upper), collapse(values), extra)
    if (isrec) do.call(wrapChildren, x) else x
  }
}
debugrec <- makeRecombinator(mkdb(TRUE), "custom", n.parents = 2, n.children = 2)
debugmut <- makeMutator(mkdb(FALSE), "custom")
