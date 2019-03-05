


lrn <- makeLearner("classif.rpart")


ps <- pSS(
  maxdepth: integer[1, 30],
  minsplit: integer[2, 30],
  cp: numeric[0.001, 0.999])

obj <- makeBaselineObjective(lrn, pid.task,
  filters = c("praznik_JMI", "auc", "anova.test"),
  ps = ps, resampling = cv5, num.explicit.featsel = 2)

debugonce(obj)



getParamSet(obj)




res <- obj(list(maxdepth = 3, minsplit = 2, cp = 0.5, mosmafs.nselect = 4,
  mosmafs.iselect.1 = 2,
  mosmafs.iselect.2 = 4,
  mosmafs.select.weights.1 = 0.2,
  mosmafs.select.weights.2 = 0.4,
  mosmafs.select.weights.3 = 0.6))



install.packages(c("DiceKriging", "emoa", "rgenoud"))

library("mlrMBO")

ctrl <- makeMBOControl(n.objectives = 2) %>%
  setMBOControlInfill(makeMBOInfillCritDIB())


attributes(obj)$noisy <- FALSE
mbores <- mbo(obj, control = ctrl)



obj2 <- makeBaselineObjective(lrn, task,
  filters = c("praznik_JMI", "auc", "anova.test"),
  ps = ps, resampling = cv5, num.explicit.featsel = 2,
  holdout.data = task.hout)

ctrl <- makeMBOControl(n.objectives = 2) %>%
  setMBOControlInfill(makeMBOInfillCritDIB()) %>%
  setMBOControlTermination(iters = 100)


attributes(obj2)$noisy <- FALSE
mbores <- mbo(obj2, control = ctrl,
  learner = cpoImputeConstant("__MISSING__") %>>%
    makeLearner("regr.randomForest", se.method = "jackknife", keep.inbag = TRUE,
      predict.type = "se"))
mbores.nolrn <- mbo(obj2, control = ctrl)

plot(as.data.frame(mbores.nolrn$opt.path)$mosmafs.nselect)
plot(as.data.frame(mbores$opt.path)$mosmafs.nselect)

plot(as.data.frame(mbores.nolrn$opt.path)[c("y_1", "y_2")],
  xlim = c(0, 1), ylim = c(0, 1))
plot(as.data.frame(mbores$opt.path)[c("y_1", "y_2")],
  xlim = c(0, 1), ylim = c(0, 1))

plot(as.data.frame(mbores$opt.path)[c("fitness.holdout.perf", "fitness.holdout.propfeat")],
  xlim = c(0, 1), ylim = c(0, 1))

plot(as.data.frame(mbores.nolrn$opt.path)[c("fitness.holdout.perf", "fitness.holdout.propfeat")],
  xlim = c(0, 1), ylim = c(0, 1))


head(as.data.frame(mbores$opt.path))

str(mbores$opt.path$env$extra)

samples <- sampleValues(getParamSet(obj2), 3000, trafo = TRUE)

sres <- parallelMap::parallelMap(obj2, samples)

mat <- sapply(sres, identity)
plot(t(mat), xlim = c(0, 1), ylim = c(0, 1))

