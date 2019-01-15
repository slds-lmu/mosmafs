



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

cvmodel <- cv.glmnet(ldata$X, ldata$Y)


besties <- order(-ldata$beta)
perfs <- mclapply(seq_along(besties), function(selectfeats) {
  subdata <- ldata
  subdata$X <- subdata$X[, besties[seq_len(selectfeats)]]
  subtask <- create.classif.task("sublinear", subdata)
  replicate(10, resample("classif.logreg", subtask, cv10))
}, mc.cores = 4)

save(perfs, ldata, file = "ldata_and_logreg_performances.Rsv")

lapply(perfs, function(perf) {
  mean(extractSubList(perf, "aggr"))
})





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




