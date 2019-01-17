


library("mlrCPO")
library("ecr")
library("checkmate")

# combine operators to be applied to individuals that conform to parameter set param.set.
# Parameters are the param.set, and the names / types of params with the operator to use.
# Parameter groups that use a single operator can be defined using `.params.<groupname>` = [character]`
#
# say param.set has three logical params 'l1', 'l2', 'l3' and two numeric params 'n1', 'n2'.
# We want to use operatorA for 'l1' and 'l2', operatorB for 'l3', and operatorC for all numeric
# params. Call as
# combineOperator(param.set, .params.group1 = c("l1", "l2"), group1 = operatorA, l3 = operatorB, numeric = operatorC)
#
# Use arguments by types, names of parameters, or group name. Valid types are 'numeric', 'logical', 'integer', 'discrete'.
# Operators given for groups or individual parameters supercede operators given for types.
#
# If .binary.discrete.as.logical is TRUE, then binary discrete params are handled as logical params.
#
# operators for logical parameters must have only one argument. operators for discreten parameters
# must have an additional argument 'values', operators for continuous parameters must have
# an additional argument 'lower', 'upper'.
#
# use the ecr::setup function to set parameters for operators ("currying")
combine.operator(param.set, ..., .binary.discrete.as.logical = TRUE) {

  typenames <- c("numeric", "logical", "integer", "discrete")
  param.ids <- getParamIds(param.set)

  nameclash <- intersect(typenames, param.ids)
  if (length(nameclash)) {
    stopf("Parameter(s) %s have nameclash with special type names", collapse(nameclash))
  }

  args <- list(...)

  assertList(args, names = "unique", any.missing = FALSE)

  groupdefidx = grep("^\\.params\\.", names(args))

  groups <- args[groupdefidx]
  args[groupdefidx] <- NULL
  names(groups) <- gsub("^\\.params\\.", "", names(groups))

  nameclash <- intersect(names(groups), c(param.ids, typenames))
  if (length(nameclash)) {
    stopf("Group(s) %s have nameclash with param.set or type names", collapse(nameclash))
  }

  groupsnotfound <- setdiff(names(groups), names(args))
  if (length(groupsnotfound)) {
    stopf("Group(s) %s defined but without operator", collapse(groupsnotfound))
  }

  assertList(groups, types = "character", any.missing = FALSE, names = "unique")
  lapply(groups, assertCharacter, min.chars = 1, any.missing = FALSE, unique = TRUE, .var.name = "Group Definitions")
  assertList(args, types = "ecr_operator", any.missing = FALSE, names = "unique")

  for (g in names(groups)) {
    nopar <- setdiff(groups[[g]], param.ids)
    if (length(nopar)) {
      stopf("Group %s contains name(s) %s that are not a parameter.", g, collapse(nopar))
    }
  }

  nopar <- setdiff(names(args), c(param.ids, typenames))
  if (length(nopar)) {
    stopf("Argument(s) %s neither a special type nor a parameter name.", collapse(nopar))
  }

  dups <- c(unlist(groups), names(args))
  dups <- dups[duplicated(dups)]
  if (length(dups)) {
    stopf("Parameter(s) %s with more than one assigned operator", collapse(dups))
  }

  namedparams <- setdiff(c(unlist(groups), names(args)), typenames)

  paramtypes <- gsub("vector$", "", getParamTypes(param.set))
  assertSubset(paramtypes, typenames)

  unnamed <- param.ids %nin% namedparams
  typegroupnames <- unique(paramtypes[unnamed])

  unusedtypes <- setdiff(intersect(names(args), typenames), typegroupnames)
  if (length(unusedtypes)) {
    warningf("Function defined for type(s) %s, but no parameters of that type present or uncovered by other group/function.",
      collapse(unusedtypes))
  }


}

discretize.params <- function(param.set, param.vals) {


}


