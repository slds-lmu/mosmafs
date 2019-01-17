



source("datagen.R")

ldata <- create.linear.data(1000, 1000, rho = .9)
hdata <- create.hypersphere.data(2, 1000)

ltask <- create.classif.task("linear", ldata)
htask <- create.classif.task("hypersphere", hdata)

htaskplus <- task.add.random.cols(htask, 5)
htaskperm <- task.add.permuted.cols(htask, 4)

htaskperm$orig.features

str(getTaskData(htaskplus))

resample("classif.cvglmnet", htask, cv10)

resample("classif.logreg", task, cv10)

resample("classif.randomForest", task, cv10)



# ------ experimenting with glmnet
library("glmnet")
library("parallel")
library("BBmisc")

## what is the sweet spot of selected number features?
besties <- order(-ldata$beta)[1:10]
perfs <- mclapply(seq_along(besties), function(selectfeats) {
  subdata <- ldata
  subdata$X <- subdata$X[, besties[seq_len(selectfeats)]]
  subtask <- create.classif.task("sublinear", subdata)
  replicate(1000, resample("classif.logreg", subtask, cv10), simplify = FALSE)
}, mc.cores = 4)
#save(perfs, ldata, file = "ldata_and_logreg_performances.Rsv")

plot(sapply(perfs, function(perf) {
  mean(extractSubList(perf, "aggr"))
}))
lines(sapply(perfs, function(perf) {
  perfs <- extractSubList(perf, "aggr")
  mean(perfs) + sd(perfs)
}))
# compare:
sum(ldata$orig.features)



## What features does cvglmnet select?
cvmodel <- cv.glmnet(ldata$X, ldata$Y)
plot(cvmodel)

names(ldata)
plot(ldata$beta)

names(cvmodel)

names(cvmodel$glmnet.fit)
plot(cvmodel$glmnet.fit$a0)

names(cvmodel$glmnet.fit)
log(cvmodel$glmnet.fit$lambda)
plot(cvmodel)

selecteds = apply(cvmodel$glmnet.fit$beta, 2,
  function(x) which(x > 0))
selorder = integer(0)
for (sel in selecteds) {
  selorder = c(selorder, setdiff(sel, selorder))
}

order(-ldata$beta)[1:20]
selorder[1:20]




# --------- experimenting with ECR2

# devtools::install_github("jakobbossek/ecr2")

source("ecrshims.R")
library("testthat")

param.set.numeric = pSS(a: numeric[1, 3]^2, b: numeric[0, 1])
param.set.integer = pSS(ai: integer[1, 3]^2, bi: integer[0, 1])
param.set.logical = pSS(al: logical^2, bl: logical)
param.set.logical.extended = pSS(ale: logical^2, ble: logical, cle: discrete[l="m", n=10], dle: discrete[a=exp, b=identity]^2)
param.set.discrete = pSS(cd: discrete[l="m", n=10, o=NULL], d: discrete[a=exp, b=identity, c=`[`]^2)
fullps <- c(param.set.numeric, param.set.integer, param.set.logical.extended, param.set.discrete)

expect_error(combine.operators(param.set.numeric, .params.x = c("a", "b"), x = mutGauss, x = mutGauss), "uniquely named")
expect_error(combine.operators(pSS(logical: logical), logical = mutBitflip), "nameclash with special type names")
expect_error(combine.operators(pSS(x: character), x = mutBitflip), "types of parameters in param.set.*subset")
expect_error(combine.operators(param.set.numeric, .params.x = c("a", "b")), " x defined but without operator")
expect_error(combine.operators(param.set.numeric, .params.x = c("a", "b", "b"), x = mutGauss), "Group Definitions.*duplicate")
expect_error(combine.operators(param.set.numeric, .params.x = c("a", "b", "c"), x = mutGauss), " x contains.* c that are not")
expect_error(combine.operators(c(param.set.numeric, param.set.integer), .params.x = c("a", "b", "ai"), x = mutGauss, ai = mutGauss),
  " x contains parameters of differing types numeric,integer")
expect_error(combine.operators(param.set.numeric, .params.x = c("a", "b"), x = mutGauss, y = mutGauss),
  " y neither a special type nor a parameter name")
expect_error(combine.operators(param.set.numeric, .params.x = c("a", "b"), x = mutGauss, b = mutGauss),
  " b with more than one assigned operator")
expect_error(combine.operators(param.set.numeric), " a,b have neither an explicit operator given")
expect_error(combine.operators(param.set.numeric, .params.x = c("a", "b"), x = identity),
  "list of operator arguments.*ecr_operator")
expect_error(combine.operators(param.set.numeric, a = mutGauss, b = recSBX),
  "operators given must have at least one of the types ecr_")
expect_error(combine.operators(param.set.numeric, a = recIntermediate, b = recSBX),
  "differing number of children")
expect_error(combine.operators(param.set.numeric, a = recIntermediate,
  b = makeRecombinator(identity, supported = "custom", n.parents = 3, n.children = 1)),
  "differing number of parents")
specrec <- makeRecombinator(identity, supported = "custom", n.parents = 2, n.children = 1)
class(specrec) <- c(class(specrec), "ecr_mutator")
expect_error(combine.operators(param.set.numeric, .params.x = c("a", "b"), x = specrec),
  "Only one type of operator")
expect_warning(combine.operators(param.set.numeric, .params.x = c("a", "b"), x = recSBX, numeric = recPMX),
  " numeric, but no parameters of that type present")

expect_error(combop <- combine.operators(param.set.logical, al = mutGauss, bl = mutBitflip),
  " al must have only .* but has parameters ind,lower,upper")

combop <- combine.operators(param.set.numeric, a = recSBX, b = recOX)
expect_class(combop, "ecr_operator")
expect_class(combop, "ecr_recombinator")
expect_equal(ecr:::getNumberOfChildren.ecr_recombinator(combop), 2)
expect_equal(ecr:::getNumberOfParentsNeededForMating.ecr_recombinator(combop), 2)

combop <- combine.operators(param.set.numeric, a = recIntermediate, b = recIntermediate)
expect_class(combop, "ecr_operator")
expect_class(combop, "ecr_recombinator")
expect_equal(ecr:::getNumberOfChildren.ecr_recombinator(combop), 1)
expect_equal(ecr:::getNumberOfParentsNeededForMating.ecr_recombinator(combop), 2)

combop <- combine.operators(param.set.numeric, a = mutGauss, b = mutScramble)
expect_class(combop, "ecr_operator")
expect_class(combop, "ecr_mutator")

mkdb <- function(isrec) {
  dbfun <- function(x, lower = 0, upper = 0, values = "", extra = 0) {
    catf("x: %s\nlower: %s\nupper: %s\nvalues: %s\nextra: %s", collapse(x), collapse(lower), collapse(upper), collapse(values), extra)
    if (isrec) do.call(wrapChildren, x) else x
  }
}
debugrec <- makeRecombinator(mkdb(TRUE), "custom", n.parents = 2, n.children = 2)
debugmut <- makeMutator(mkdb(FALSE), "custom")

op <- combine.operators(param.set.numeric, a = ecr::setup(debugrec, extra = 1), b = ecr::setup(debugrec, extra = 2))
op(sampleValues(param.set.numeric, 2, discrete.names = TRUE))

op <- combine.operators(param.set.numeric, numeric = ecr::setup(debugrec, extra = 1))
op(sampleValues(param.set.numeric, 2, discrete.names = TRUE))


op <- combine.operators(fullps,
  numeric = ecr::setup(debugrec, extra = "numeric"),
  logical = ecr::setup(debugrec, extra = "logical"),
  integer = ecr::setup(debugrec, extra = "integer"),
  discrete = ecr::setup(debugrec, extra = "discrete"))

op(sampleValues(fullps, 2, discrete.names = TRUE))[[1]]

op <- combine.operators(fullps,
  numeric = ecr::setup(debugmut, extra = "numeric"),
  logical = ecr::setup(debugmut, extra = "logical"),
  integer = ecr::setup(debugmut, extra = "integer"),
  discrete = ecr::setup(debugmut, extra = "discrete"))

op(sampleValue(fullps, discrete.names = TRUE))



op(sampleValue(param.set.numeric, discrete.names = TRUE))



param.set = pSS(a: integer[1, 3]^2, b: logical, c: numeric[-1, 1], e: discrete[a=1, b=2]^2 [[requires=quote(c > 0)]])

generateRandomDesign(par.set = param.set)
samp <- sampleValues(param.set, 1, discrete.names = TRUE)

samp <- sampleValues(param.set, 1, discrete.names = FALSE)
samp

samp

dfRowToList(samp[[1]], param.set)

extractSubList(param.set$pars, "cnames")

getParamTypes(param.set)
getValues(param.set)

sampleValues(param.set, 4)[[1]]



getParamIds(filterParams(param.set, type = "numeric"))






sampleValue(param.set.discrete, discrete.names = TRUE)

names(param.set.logical.extended$pars[[3]]$values)
extractSubList(param.set.logical.extended$pars, "len")

inlists <- list(
    list(a=1, b=list(123, 456), c=letters[1:3]),
    list(a=10, b=list(3, 6), c=letters[4:6]))

do.call(mapply, c(list(FUN = base::list, SIMPLIFY = FALSE), inlists))$b


mapply
purrr::transpose(inlists)$b

# ----------------------------

source("selectorcpo.R")

head(iris %>>% cpoSelector(c(TRUE, TRUE, FALSE, FALSE, TRUE)))
head(getTaskData(iris.task %>>% cpoSelector(c(TRUE, TRUE, FALSE, FALSE))))

