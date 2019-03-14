



source("datagen.R")


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



indiv1 <- list(one = c(0, 0, 0, 0), two = 0, three = .01)
indiv2 <- list(one = c(0, 0, 0, 0), two = 0, three = 100)

op(indiv1)
op(indiv2)


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

