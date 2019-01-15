



source("datagen.R")

ldata <- create.linear.data(10000, 100)
hdata <- create.hypersphere.data(2, 10000)

ltask <- create.classif.task("linear", ldata)
htask <- create.classif.task("linear", ldata)

htaskplus <- task.add.random.cols(htask, 5)
str(getTaskData(htaskplus))

resample("classif.cvglmnet", htask, cv10)

resample("classif.logreg", task, cv10)

resample("classif.randomForest", task, cv10)

library("glmnet")

plot(cv.glmnet(ldata$X, ldata$Y))

