
library("mlr")

processArff <- function(root.data) {
  datadirs <- list.dirs(root.data, recursive = FALSE, full.names = FALSE)

  proto.rinst <- makeResampleInstance(makeResampleDesc("Subsample", iter = 1), size = 100)

  tasks <- sapply(datadirs, function(d) {
    fulld <- file.path(root.data, d)
    train <- RWeka::read.arff(file.path(fulld, "train.arff"))
    test <- RWeka::read.arff(file.path(fulld, "test.arff"))
    fulltask <- rbind(train, test)
    rinst <- proto.rinst
    rinst$desc$split <- nrow(train) / (nrow(fulltask))
    rinst$desc$id <- paste0(rinst$desc$id, ".", d)
    rinst$size <- nrow(fulltask)
    rinst$train.inds[[1]] <- seq_len(nrow(train))
    rinst$test.inds[[1]] <- seq_len(nrow(test)) + nrow(train)
  #
    list(task = makeClassifTask(d, data = fulltask, target = "class"),
      rinst = rinst)
  }, simplify = FALSE)


  testtask <- function(tname) {
    train <- readARFF(sprintf("../data/%s/train.arff", tname))
    test <- readARFF(sprintf("../data/%s/test.arff", tname))
    model <- train("classif.glmnet", makeClassifTask(data = train, target = "class"))
    pred <- predict(model, makeClassifTask(data = test, target = "class"))
    res <- resample("classif.glmnet", tasks[[tname]]$task, tasks[[tname]]$rinst)
    rownames(pred$data) <- NULL
    rownames(res$pred$data) <- NULL
    all.equal(pred$data[c("truth", "response")], res$pred$data[c("truth", "response")])
  }

  stopifnot(testtask("sonar"))
  stopifnot(testtask("ionosphere"))

  for (tname in names(tasks)) {
    saveRDS(tasks[[tname]], file.path(root.data, sprintf("TASK_%s.rds", tname)))
  }
}

splitArff <- function(root.data) {
  datadirs <- list.dirs(root.data, recursive = FALSE, full.names = FALSE)
  for (tname in datadirs) {
    fulld <- file.path(root.data, tname)
    train <- RWeka::read.arff(file.path(fulld, "train.arff"))
    test <- RWeka::read.arff(file.path(fulld, "test.arff"))
    traintask <- makeClassifTask(paste0(tname, ".train"), data = train, target = "class")
    houttask <- makeClassifTask(paste0(tname, ".hout"), data = test, target = "class")
    saveRDS(list(task = traintask, hout = houttask), file.path(root.data, sprintf("SPLIT_%s.rds", tname)))
  }
}


# processArff("../data")

library("mlr")
library("data.table")
library("parallel")

tasks <- sapply(dir("../data", "\\.rds$", full.names = TRUE), readRDS, simplify = FALSE)
names(tasks) <- gsub(".*/TASK_(.*)\\.rds", "\\1", names(tasks))

filters <- listFilterMethods(desc = FALSE, tasks = TRUE)
filters <- as.character(subset(filters, task.classif)$id)


# tasks <- tasks[setdiff(names(tasks), c("convex", grep("hypersphere", names(tasks), value = TRUE)))]
# filters <- setdiff(filters, "permutation.importance")
# filters <- setdiff(filters, broken.all)

todo <- transpose(CJ(filters, names(tasks)))

filterresults2 <- mclapply(todo, function(info) {
  filter <- info[1]
  task <- info[2]
  cat(sprintf("Doing %s on %s.\n", filter, task))
  result <- generateFilterValuesData(tasks[[task]]$task, method = filter)
  cat(sprintf("Done %s on %s.\n", filter, task))
  result
}, mc.preschedule = FALSE, mc.cores = 32)

sort(sapply(filterresults, function(f) {
  if (!is.null(f) && class(f) != "try-error") {
    mean(is.na(f$data[[3]]))
  } else {
    1
  }
}))


# saveRDS(filterresults, "filterresults_small.rds")
# saveRDS(filterresults2, "filterresults_small_2.rds")

scaled <- function(scaling, fs = filterresults) {
  lapply(fs, function(f) {
    if (!is.null(f) && class(f) != "try-error") {
      switch(scaling,
        mosmafs = {
          datavec <- f$data[[3]]
          datavec <- datavec - mean(datavec, na.rm = TRUE)
          divisor <- max(abs(range(datavec, na.rm = TRUE)))
          if (divisor > 0) {
            ret <- datavec / divisor / 2 + 0.5
            ret[is.na(ret)] <- 0.5
            ret
          } else {
            0.5
          }
        },
        scale = {
          res <- scale(f$data[[3]])
          res[is.na(res)] <- 0
          res
        },
        rank = {
          dat <- f$data[[3]]
          dat[is.na(dat)] <- -Inf
          rank(dat) / length(dat)
        }
      )
    }
  })
}




snames <- strsplit(names(scaled("mosmafs")), ",")
brokennames <- snames[sapply(scaled("mosmafs"), is.null)]
usedfilters <- function(s) unique(sapply(s, function(x) x[1]))

broken.all <- usedfilters(brokennames)
broken.nodexter <- usedfilters(Filter(function(x) x[2] != "dexter", brokennames))
datasets <- unique(sapply(snames, function(x) x[2]))

construct.matrix <- function(method, withdexter = TRUE, outds = datasets, getdataorder = FALSE, which = filterresults) {
  transformed <- scaled(method, fs = which)
  dataorder <- list()
  snames <- strsplit(names(transformed), ",")
  for (i in seq_along(transformed)) {
    identity <- snames[[i]]
    method <- identity[1]
    dataset <- identity[2]
    if (is.null(dataorder[[method]])) {
      dataorder[[method]] <- list()
    }
    dataorder[[method]][[dataset]] <- transformed[[i]]
  }
  if (getdataorder) {
    return(dataorder)
  }

  if (withdexter) {
    sapply(dataorder[setdiff(names(dataorder), broken.all)], function(dats) unlist(dats[outds]))
  } else {
    sapply(dataorder[setdiff(names(dataorder), broken.nodexter)], function(dats) unlist(dats[setdiff(outds, "dexter")]))
  }
}

meanabsdiff <- function(x) {
  apply(x, 2, function(col1) {
    apply(x, 2, function(col2) {
      mean(abs(col1 - col2))
    })
  })
}

cordiff <- function(x) 1 - cor(x)

distmat <- function(method, dist, withdexter, dataset = datasets) {
  distmethod <- switch(dist,
    diff = meanabsdiff,
    cor = cordiff)
  distmethod(construct.matrix(method, withdexter, outds = datasets))
}

testmat <- distmat("mosmafs", "cor", TRUE)
scl <- cmdscale(testmat)
plot(scl)
text(scl, labels = row.names(scl), cex=.7)

heatmap(testmat)

heatmap(construct.matrix("mosmafs", FALSE, outds = "sonar"), Rowv = NA, distfun = function(x) dist(x, method = "manhattan"))
heatmap(construct.matrix("scale", FALSE, outds = "sonar"), Rowv = NA, distfun = function(x) dist(x, method = "manhattan"))
heatmap(construct.matrix("rank", FALSE, outds = "sonar"), Rowv = NA, distfun = function(x) dist(x, method = "manhattan"))

heatmap(construct.matrix("mosmafs", FALSE, outds = "madelon"), Rowv = NA, distfun = function(x) dist(x, method = "manhattan"))
heatmap(construct.matrix("scale", FALSE, outds = "madelon"), Rowv = NA, distfun = function(x) dist(x, method = "manhattan"))
heatmap(construct.matrix("rank", FALSE, outds = "madelon"), Rowv = NA, distfun = function(x) dist(x, method = "manhattan"))

heatmap(construct.matrix("mosmafs", FALSE, outds = "ionosphere"), Rowv = NA, distfun = function(x) dist(x, method = "manhattan"))
heatmap(construct.matrix("scale", FALSE, outds = "ionosphere"), Rowv = NA, distfun = function(x) dist(x, method = "manhattan"))
heatmap(construct.matrix("rank", FALSE, outds = "ionosphere"), Rowv = NA, distfun = function(x) dist(x, method = "manhattan"))

heatmap(construct.matrix("mosmafs", TRUE, outds = "dexter"), Rowv = NA, distfun = function(x) dist(x, method = "manhattan"))
heatmap(construct.matrix("scale", TRUE, outds = "dexter"), Rowv = NA, distfun = function(x) dist(x, method = "manhattan"))
heatmap(construct.matrix("rank", TRUE, outds = "dexter"), Rowv = NA, distfun = function(x) dist(x, method = "manhattan"))

meandist <- function(scaling, corfun = meanabsdiff, outds = datasets, both = FALSE) {
  meanmat <- Reduce(`+`, lapply(outds, function(ds) {
    if (both) {
      m1 <- construct.matrix(method = scaling, withdexter = "dexter" %in% outds, outds = ds, which = filterresults)
      m2 <- construct.matrix(method = scaling, withdexter = "dexter" %in% outds, outds = ds, which = filterresults2)
      colnames(m1) <- paste0(colnames(m1), ".1")
      colnames(m2) <- paste0(colnames(m2), ".2")
      m <- cbind(m1, m2)
      corfun(m)
    } else {
      corfun(construct.matrix(method = scaling, withdexter = "dexter" %in% outds, outds = ds, which = filterresults))
    }
  })) / length(outds)
  as.dist(meanmat)
}


plot(hclust(meandist("mosmafs")))
plot(hclust(meandist("mosmafs", outds = setdiff(datasets, "dexter"))))

plot(hclust(meandist("mosmafs", corfun = cordiff)))

plot(hclust(meandist("mosmafs", corfun = cordiff, outds = setdiff(datasets, "dexter"))))

plot(hclust(meandist("scale")))
plot(hclust(meandist("scale", corfun = cordiff)))
plot(hclust(meandist("scale", outds = setdiff(datasets, "dexter"))))
plot(hclust(meandist("scale", corfun = cordiff, outds = setdiff(datasets, "dexter"))))


plot(hclust(meandist("rank", both = TRUE)))


plot(hclust(meandist("rank")))





testmat <- meandist("rank")
scl <- cmdscale(testmat)
plot(scl)
text(scl, labels = row.names(scl), cex=.7)




plot(hclust(meandist("rank", corfun = cordiff)))
plot(hclust(meandist("rank", outds = setdiff(datasets, "dexter"))))
plot(hclust(meandist("rank", corfun = cordiff, outds = setdiff(datasets, "dexter"))))

dm <- construct.matrix(method = "rank", withdexter = TRUE)

dm <- dm[, setdiff(colnames(dm), c("univariate.model.score"))]

simplexvolume <- function(cols) {
  cols <- cols[, -1, drop = FALSE] - cols[, 1]
  sqrt(det(t(cols) %*% cols)) / factorial(ncol(cols))
}

choicerank <- function(matr, choices, includeohfive = TRUE) {
  choices <- combn(ncol(matr), choices)
  numbers <- apply(choices, 2, function(taking) {
    if (includeohfive) {
      simp <- cbind(matr[, taking], 0.5)
    } else {
      simp <- matr[, taking]
    }
    simplexvolume(simp)
  })
  names(numbers) <- apply(choices, 2, function(taking) {
    paste(colnames(matr)[taking], collapse = ",")
  })
  sort(numbers)
}

choicemodel <- function(cr) {
  indices <- strsplit(names(cr), ",")
  allidx <- sort(unique(unlist(indices)))
  choicemat <- t(sapply(indices, function(idx) {
    as.numeric(allidx %in% idx)
  }))
  colnames(choicemat) <- allidx
  lcr <- log(cr)
  lm(lcr ~ 0 + choicemat)
}

plot(choicerank(dm, 2))
tail(choicerank(dm, 2))
plot(choicerank(dm, 3))
tail(choicerank(dm, 3), 20)

coefs.2 <- summary(choicemodel(choicerank(dm, 2)))$coef
coefs.3 <- summary(choicemodel(choicerank(dm, 3)))$coef
coefs.4 <- summary(choicemodel(choicerank(dm, 4)))$coef
chr <- choicerank(dm, 5)
chx <- choicemodel(chr)
coefs.5 <- summary(chx)$coef

coefs.2.nonul <- summary(choicemodel(choicerank(dm, 2, FALSE)))$coef
coefs.3.nonul <- summary(choicemodel(choicerank(dm, 3, FALSE)))$coef
coefs.4.nonul <- summary(choicemodel(choicerank(dm, 4, FALSE)))$coef
chr.nonul <- choicerank(dm, 5, FALSE)
chx.nonul <- choicemodel(chr.nonul)
coefs.5.nonul <- summary(chx.nonul)$coef


coefs.2[order(coefs.2[, "Estimate"]), ]
coefs.3[order(coefs.3[, "Estimate"]), ]
coefs.4[order(coefs.4[, "Estimate"]), ]
coefs.5[order(coefs.5[, "Estimate"]), ]

coefs.2.nonul[order(coefs.2.nonul[, "Estimate"]), ]
coefs.3.nonul[order(coefs.3.nonul[, "Estimate"]), ]
coefs.4.nonul[order(coefs.4.nonul[, "Estimate"]), ]
coefs.5.nonul[order(coefs.5.nonul[, "Estimate"]), ]


# praznik_CMIM, ranger_impurity, rfSRC_var.select, ranger_permutation, rfSRC_importance, auc, variance, anova.test

# ranger_impurity, ranger_permutation, randomForestSRC_var.select,

# anova.test, CMIM, impurity, permutation, rfimportance

tail(choicerank(dm, 4), 10)

tail(chr, 10)

tail(choicerank(dm, 7), 100)



plot(as.ordered(sapply(strsplit(names(chr), ","), function(nm) sum(grepl("praznik", nm)))), log(chr))


tail(chr)


str(chx)

names(do)
do <- construct.matrix(method = "scale", withdexter = TRUE, getdataorder = TRUE)

plot(do[[2]][["sonar"]])
plot(sort(do[[14]][["sonar"]]))

do <- construct.matrix(method = "mosmafs", withdexter = TRUE, getdataorder = TRUE)

plot(do[[27]][["sonar"]])

do <- construct.matrix(method = "rank", withdexter = TRUE, getdataorder = TRUE)
plot(do[[4]][["ionosphere"]])
points(do[[1]][["ionosphere"]], pch = "x")

dataset <- create.hypersphere.data(4, 10000)
dataset <- create.classif.task(id = "hypersphere", dataset)
dataset <- task.add.permuted.cols(dataset, 5)

plot(dataset$orig.features, generateFilterValuesData(dataset, "praznik_JMI")$data[[3]])






x <- rnorm(10)
y <- rnorm(10)

sum(abs(rank(x) - rank(y)))

unique(sapply((1:1000)/1000, function(lambda) paste(as.character(rank(x * lambda + (1 - lambda) * y)), collapse = ",")))
