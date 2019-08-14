

#' @title Combine ECR-Operators
#'
#' @description
#' Combine operators to be applied to individuals that conform to parameter set `param.set`.
#' Parameters are the `param.set`, and the names / types of params with the operator to use.
#' Parameter groups that use a single operator can be defined using `.params.<groupname> = [character]`.
#'
#' Say `param.set` has three logical params 'l1', 'l2', 'l3' and two numeric params 'n1', 'n2'.
#' To use operatorA for 'l1' and 'l2', operatorB for 'l3', and operatorC for all numeric
#' params, call          
#' `combineOperator(param.set, .params.group1 = c("l1", "l2"), group1 = operatorA, l3 = operatorB, numeric = operatorC)`.
#'
#' Use arguments by types, names of parameters, or group name. Valid types are 'numeric', 'logical', 'integer', 'discrete'.
#' Operators given for groups or individual parameters supercede operators given for types.
#'
#' Strategy parameters can be created by using `.strategy.<groupname|parametername|type>`. They must be a *function*
#' taking a named list of parameter values (i.e. an individuum) as input and return a named list of parameter values
#' to be given to the respective group's / parameter's or type's operator. If, in the example above, `operatorA` has
#' a parameter `sigma` that should also be treated as a parameter under evolution (and in fact be equal to `l3`), then
#' the above call would become
#' `combineOperator(param.set, .params.group1 = c("l1", "l2"), group1 = operatorA, .strategy.group1 = function(x) list(sigma = x$l3),
#'   l3 = operatorB, numeric = operatorC)`.
#'
#' If .binary.discrete.as.logical is TRUE, then binary discrete params are handled as logical params.
#'
#' Operators for logical parameters must have only one argument. Operators for discrete parameters
#' must have an additional argument 'values'. Operators for continuous or integer parameters must have
#' an additional argument 'lower', 'upper'.
#'
#' Use the ecr::setup function to set parameters for operators ("currying").
#'
#' @param param.set `[ParamSet]` [`ParamSet`][ParamHelpers::makeParamSet] that defines the search space.
#' @param ... additional parameters. See description.
#' @param .binary.discrete.as.logical `[logical(1)]` whether to treat binary discrete parameters as `logical` parameters and
#'   use bitwise operators.
#' @return [`ecr_operator`][ecr::makeOperator] ecr operator.
#' @examples 
#' library(mlrCPO)
#' 
#' # Create parameter set 
#' ps <- pSS(
#' logi: logical, 
#' disc: discrete[yes, no], 
#' discvec: discrete[letters]^3, 
#' numer: numeric[0, 10])
#' 
#' # Define mutators for groups of parameters 
#' combo.mut <- combine.operators(ps, 
#' .params.group1 = c("logi", "disc"), # define group for which same mutator is used
#' group1 = ecr::setup(mutBitflip, p = 1), # set probability for mutation to 1
#' discrete = mutRandomChoice, # define operator for all other discrete parameters
#' numer = mutGauss) # specific operator for parameter numer
#' 
#' combo.mut(list(logi = FALSE, disc = "yes", discvec = c("a", "x", "y"), 
#' numer = 2.5))
#' 
#' # Define mutator with strategy parameter
#' combo.strategy <- combine.operators(ps,
#' logical = ecr::setup(mutBitflip, p = 0),
#' discrete = mutRandomChoice,
#' numeric = mutGauss, 
#' .strategy.numeric = function(ind) {
#' if (ind$disc == "yes") {
#' return(list(p = 1L))
#' } else {
#' return(list(p = 0L))
#' }
#' })
#' 
#' combo.strategy(list(logi = FALSE, disc = "no", discvec = c("a", "x", "y"), 
#' numer = 2.5))
#' 
#' # Define recombinators for groups of parameters
#' combo.rec <- combine.operators(ps, 
#' .params.group1 = c("logi", "disc"), # define group for which same mutator is used
#' group1 = recPCrossover, 
#' discrete = recPCrossover,
#' numer = recGaussian)
#' 
#' combo.rec(list(list(logi = FALSE, disc = "no", discvec = c("a", "x", "y"), 
#' numer = 2.5), list(logi = TRUE, disc = "yes", discvec = c("c", "e", "g"), 
#' numer = 7.5)))
#' 
#' @export
combine.operators <- function(param.set, ..., .binary.discrete.as.logical = TRUE) {

  args <- list(...)
  assertList(args, names = "unique", any.missing = FALSE, .var.name = "...")
  assertFlag(.binary.discrete.as.logical)

  typenames <- c("numeric", "logical", "integer", "discrete")  # supported types
  required.opargs <- list(numeric = c("lower", "upper"), integer = c("lower", "upper"), discrete = "values", logical = character(0))
  param.ids <- getParamIds(param.set)

  operatorclasses <- c("ecr_recombinator", "ecr_mutator")

  if (hasRequires(param.set)) stop("Parameters with requirements not currently supported.")

  groupdefidx <- grep("^\\.params\\.", names(args))
  groups <- args[groupdefidx]
  stratdefidx <- grep("^\\.strategy\\.", names(args))
  strats <- args[stratdefidx]
  args[c(groupdefidx, stratdefidx)] <- NULL
  names(groups) <- gsub("^\\.params\\.", "", names(groups))
  names(strats) <- gsub("^\\.strategy\\.", "", names(strats))

  # at this point *groups* should be a named list of character vectors, and
  # *args* should be a named list of 'ecr_operator' functions

  namedparams <- setdiff(c(unlist(groups), names(args)), typenames)  # params named in groups or explicitly
  unnamed <- param.ids %nin% namedparams                             # params not named (should be accessible by type)
  typeargnames <- intersect(names(args), typenames)                  # types for which operators are  given explicitly or in a group
  paramtypes <- gsub("vector$", "", getParamTypes(param.set))        # parameter types
  if (.binary.discrete.as.logical) {                                 #   [possibly adapted to treat binary categories as logical]
    is.discrete.binary = vnapply(extractSubList(param.set$pars, "values", simplify = FALSE), length) == 2
    paramtypes[paramtypes == "discrete" & is.discrete.binary] = "logical"
  }
  names(paramtypes) <- param.ids
  requiredtypegroupnames <- unique(paramtypes[unnamed])              # types of parameters that have no explicit operators given

  whichop <- lapply(args, function(x) {
    intersect(class(x), operatorclasses)
  })
  whichop <- Reduce(intersect, whichop)                              # operator class of all args


  # ------------------------------------------------------------------------- #
  # input sanity checks                                                       #
  # ------------------------------------------------------------------------- #

  #   - param.ids must not have special type names
  nameclash <- intersect(typenames, param.ids)
  if (length(nameclash)) {
    stopf("Parameter(s) %s have nameclash with special type names", collapse(nameclash))
  }

  #   - types present in par.set must be supported
  assertSubset(paramtypes, typenames, .var.name = "types of parameters in param.set")

  #   - group names must not overlap with param names
  nameclash <- intersect(names(groups), c(param.ids, typenames))
  if (length(nameclash)) {
    stopf("Group(s) %s have nameclash with param.set or type names", collapse(nameclash))
  }

  #   - all groups must have a corresponding operator
  groupsnotfound <- setdiff(names(groups), names(args))
  if (length(groupsnotfound)) {
    stopf("Group(s) %s defined but without operator", collapse(groupsnotfound))
  }

  #   - groups must be character vectors with no duplicates
  assertList(groups, types = "character", any.missing = FALSE, names = "unique")
  lapply(groups, assertCharacter, min.chars = 1, any.missing = FALSE, unique = TRUE, .var.name = "Group Definitions")

  #   - all group elements must be parameter IDs
  for (g in names(groups)) {
    nopar <- setdiff(groups[[g]], param.ids)
    if (length(nopar)) {
      stopf("Group %s contains name(s) %s that are not a parameter.", g, collapse(nopar))
    }
  }

  #  - parameters in all groups must only be of one type each
  for (g in names(groups)) {
    partype <- unique(paramtypes[groups[[g]]])
    if (length(partype) != 1) {
      stopf("Group %s contains parameters of differing types %s.", g, collapse(partype))
    }
  }

  #   - arguments that are not a group must name a parameter ID or special type
  nopar <- setdiff(names(args), c(names(groups), param.ids, typenames))
  if (length(nopar)) {
    stopf("Argument(s) %s neither a special type nor a parameter name.", collapse(nopar))
  }

  #   - parameters must not have duplicate operator assignments (through groups or explicit arguments)
  dups <- c(unlist(groups), names(args))
  dups <- dups[duplicated(dups)]
  if (length(dups)) {
    stopf("Parameter(s) %s with more than one assigned operator", collapse(dups))
  }

  #  - all parameters must have at least one assigned operator
  uncovered <- param.ids[unnamed & paramtypes %nin% typeargnames]
  if (length(uncovered)) {
    stopf("Parameter(s) %s have neither an explicit operator given, nor are they in a group or a fallback type.",
      collapse(uncovered))
  }

  #   - operators must be of type 'ecr_operator'
  assertList(args, types = "ecr_operator", min.len = 1, any.missing = FALSE, names = "unique", .var.name = "list of operator arguments")

  #   - operator subclass must be one of the possible operatorclasses
  if (!length(whichop)) {
    stopf("All operators given must have at least one of the types %s in common.", collapse(operatorclasses))
  }

  #   - make sure operators agree in number of parents / children
  if ("ecr_recombinator" %in% whichop) {
    if (length(unique(vnapply(args, getNumberOfChildren))) != 1) {
      stop("Recombinator operators have differing number of children.")
    }
    if (length(unique(vnapply(args, getNumberOfParentsNeededForMating))) != 1) {
      stop("Recombinator operators need differing number of parents.")
    }
  }

  if (length(whichop) != 1) {
    stop("Only one type of operator currently supported")
  }

  #  - check that 'strats' is a list of functions
  assertList(strats, types = "function", any.missing = FALSE, names = "unique")

  #  - check that strats are only defined for groups / parameters / types that have functions named
  stratnotfound <- setdiff(names(strats), names(args))
  if (length(stratnotfound)) {
    stopf("Strategy/ies defined for %s without corresponding function.", stratnotfound)
  }

  #  - warn if an operator is given for a type that does not occur or
  #    if all parameters of that type have overriding definitions
  unusedtypes <- setdiff(typeargnames, requiredtypegroupnames)
  if (length(unusedtypes)) {
    messagef("Function defined for type(s) %s, but no parameters of that type present or non-covered by other group/function.",
      collapse(unusedtypes))
  }

  # ------------------------------------------------------------------------- #
  # Collect parameters into groups                                            #
  # ------------------------------------------------------------------------- #

  typegroups <- sapply(requiredtypegroupnames, function(type) {
    param.ids[paramtypes == type & unnamed]
  }, simplify = FALSE)

  singletongroups <- sapply(setdiff(names(args), c(names(groups), typenames)), identity, simplify = FALSE)

  allgroups <- c(groups, typegroups, singletongroups)
  operators <- args[names(allgroups)]  # this drops operators for types that are not needed
  strats[unusedtypes] <- NULL
  assertSubset(names(strats), names(allgroups))  # debug assertion

  fullstrats <- namedList(names(allgroups))  # want to have correct ordering
  fullstrats[names(strats)] <- strats

  paramsets <- lapply(allgroups, function(content) {
    ps <- param.set
    ps$pars <- ps$pars[content]
    ps
  })
  optypes <- vcapply(allgroups, function(content) unique(paramtypes[content]))

  # one last sanity check: operators need to have the specified parameters
  # Note: we don't always need the lower / upper argument
  mapply(optypes, operators, names(allgroups), fullstrats, SIMPLIFY = FALSE, FUN = function(type, op, opname, strategy) {
    if (!is.null(strategy)) return(NULL)  # if there is a strategy it will hopefully fill the gap.
    nondefaults <- names(Filter(function(x) identical(x, substitute()), formals(args(op))))
    required <- required.opargs[[type]]
    leftargs <- setdiff(nondefaults, c(required, "..."))
    if (length(leftargs) > 1) {
      stopf("Operator %s must have only 1 parameter%s, but has parameters %s",
        opname, if (length(required)) sprintf(" (additionally to required parameters %s)", collapse(required)) else "",
        collapse(leftargs))
    }
  })

  unify.operators(param.set, operators, paramsets, optypes, whichop, fullstrats)
}

# at this point we have
# - orig.param.set: original paramset, used to preserve order
# - operators: named list of operators to use for that group
# - paramsets: named list of paramsets for each group
# - paramtypes: named character 'param name' -> 'effective type' ('effective' in the sense that a binary discrete can be a logical)
# - optype: character(1) operator type (currently one of "ecr_recombinator", "ecr_mutator")
# - strats: named list of functions that extract extra parameters ("strategies") from individuals for operators.
unify.operators <- function(orig.param.set, operators, paramsets, paramtypes, optype, strats) {
  assertChoice(optype, c("ecr_recombinator", "ecr_mutator"))
  param.ids <- getParamIds(orig.param.set)
  group.param.ids <- lapply(paramsets, getParamIds)
  group.truetypes <- lapply(paramsets, function(x) gsub("vector$", "", getParamTypes(x)))
  group.effectivetypes <- paramtypes
  group.param.lengths <- lapply(paramsets, getParamLengths)
  group.param.startidx <- lapply(group.param.lengths, function(x) c(0, cumsum(x)[-length(x)]))
  group.param.vals <- lapply(paramsets, function(ps) rep(lapply(getValues(ps), names), getParamLengths(ps)))  # TODO: test this!


  curried.operators <- mapply(operators, group.effectivetypes, paramsets, group.param.vals, strats,
    SIMPLIFY = FALSE, FUN = function(op, type, parset, vals, strategy) {
    vallist <- switch(type,
      logical = list(),
      discrete = list(values = vals),
      numeric = list(lower = getLower(parset), upper = getUpper(parset)),
      integer = list(lower = getLower(parset), upper = getUpper(parset))
    )
    if (is.null(strategy)) {
      function(ind, fullind) {
        do.call(op, c(list(ind), vallist))
      }
    } else {
      function(ind, fullind) {
        do.call(op, c(list(ind), vallist, strategy(fullind)))
      }
    }
  })

  # input: list of parameter values
  # returns: list of simple vectors to feed to operators
  input.breakdown <- function(input) {
    assertSetEqual(names(input), param.ids)
    mapply(paramsets, group.param.ids, group.effectivetypes, group.truetypes, SIMPLIFY = FALSE,
      FUN = function(curps, groupmembers, type, truetypes) {
        curvals <- mapply(input[groupmembers], curps$pars, truetypes, SIMPLIFY = FALSE,
          FUN = function(curval, par, truet) {
            if (type == "logical") {
              if (truet == "discrete") {
                curval <- curval == names(par$values)[2]
              }
              curval <- as.numeric(curval)
            }
            if (isScalarNA(curval)) {
              curval <- rep(curval, par$len)
            }
            names(curval) <- par$cnames
            curval
          })
        unlist(curvals, recursive = FALSE)
      })
  }

  # output: list of vectors that came out of operators
  # returns: list of parameter values as seen from outside.
  output.buildup <- function(output.list) {
    splitup <- mapply(output.list, paramsets, group.effectivetypes, group.truetypes,
      group.param.lengths, group.param.startidx, SIMPLIFY = FALSE, USE.NAMES = FALSE,
      FUN = function(curval, curps, type, truetypes, parlen, paridx) {
        mapply(curps$pars, truetypes, parlen, paridx, SIMPLIFY = FALSE,
          FUN = function(par, truet, len, idx) {
            listelt <- curval[idx + seq_len(len)]
            if (type == "logical") {
              if (truet == "discrete") {
                listelt <- names(par$values)[listelt + 1]
              } else {
                listelt <- as.logical(listelt)
              }
            }
            if (all(is.na(listelt))) {
              listelt <- listelt[1]
            }
            names(listelt) <- par$cnames
            listelt
          })
      })
    splitup <- unlist(splitup, recursive = FALSE)
    assertSetEqual(names(splitup), param.ids)
    splitup[param.ids]
  }

  switch(optype,
    ecr_mutator = makeMutator(supported = "custom", function(input) {
      input.list <- input.breakdown(input)
      output.list <- mapply(curried.operators, input.list, FUN = function(x, y) x(y, input), SIMPLIFY = FALSE)
      output.buildup(output.list)
    }),
    ecr_recombinator = makeRecombinator(supported = "custom",
      n.parents = getNumberOfParentsNeededForMating(operators[[1]]),
      n.children = getNumberOfChildren(operators[[1]]),
      function(input) {
        input.list <- transpose.list(lapply(input, input.breakdown))
        output.list <- mapply(curried.operators, input.list, SIMPLIFY = FALSE, FUN = function(x, y) {
          val <- x(y, input)
          if (!isTRUE(attr(val, "multiple"))) {
            val <- list(val)
          }
          val
        })
        do.call(wrapChildren, lapply(transpose.list(output.list), output.buildup))
      }),
    stop("This should never happen."))
}

transpose.list <- function(inlist) {
  do.call(mapply, c(list(FUN = base::list, SIMPLIFY = FALSE), inlist))
}

set.defaults = function(infun, ...) {
  formals(infun) <- insert(formals(infun), list(...))
  infun
}
