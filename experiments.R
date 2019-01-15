



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




