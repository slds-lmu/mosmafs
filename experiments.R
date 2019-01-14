



source("datagen.R")


task <- create.linear.classif.task("test", n = 1000, p = 100)$task

resample("classif.cvglmnet", task, cv10)

resample("classif.logreg", task, cv10)

resample("classif.randomForest", task, cv10)

