library("ggplot2")
library("data.table")
library("ecr")
library("batchtools")
library("reshape2")
library("magrittr")
library("OpenML")
library("mlrMBO")
library("mlrCPO")
library("mosmafs")
library("BBmisc")

source("def.R")


reg <- loadRegistry("registry", writeable = FALSE)

# playing around
getJobTable(findDone()[1, ])

findDone()

tab <- summarizeExperiments(by = c("job.id", "algorithm", "problem", "learner", "maxeval", "mu", "lambda", "initialization", "filter", "filter.during.run", "chw.bitflip", "adaptive.filter.weights"))

tab <- ijoin(tab, findDone())

tab <- tab[algorithm %in% c("randomsearch", "mosmafs")]
tab <- tab[maxeval == 4000]
tab$maxeval <- NULL
tab <- tab[is.na(mu) | (mu == 80 & lambda == 15)]
tab$mu <- NULL
tab$lambda <- NULL

tabuniq <- tab[algorithm == "mosmafs" & problem == "USPS" & learner == "xgboost"]
tabuniq$job.id <- NULL
tabuniq$problem <- NULL
tabuniq$learner <- NULL
tabuniq <- unique(tabuniq)

tabru <- tab[algorithm == "randomsearch" & problem == "USPS" & learner == "xgboost"]
tabru$job.id <- NULL
tabru$problem <- NULL
tabru$learner <- NULL
tabru <- unique(tabru)


tabuniq

consider <- tab[algorithm == "mosmafs" & filter.during.run & adaptive.filter.weights & problem %nin%  c("eating", "gina_agnostic", "dilbert", "philippine", "AP_Lung_Uterus", "hypersphere.200.50", "hypersphere.200.200")]

consider$algorithm <- NULL
consider$initialization <- NULL
consider$filter <- NULL
consider$filter.during.run <- NULL
consider$adaptive.filter.weights <- NULL
consider$.count <- NULL

consider$job.id[1, ]

matplot(extractSubList(loadResult(consider[1, ])$result$pareto.set, "filterweights"))

fino <- function(fi) {
  fi <- -log(1 - fi)
  fi / max(sum(fi), 0.001)
}

getfw <- function(x) {
  res <- rbindlist(lapply(x$result$pareto.set, function(ind) c(as.list(fino(ind$filterweights)), list(propfeat = attr(ind, "fitness")[2]))))
  colnames(res) <- c(FILTER[["custom"]], "propfeat")
  res
}


getnum <- function(x) {
  rbindlist(lapply(x$result$pareto.set, function(ind) c(list(numstrat = ind$stratparm.numeric), list(propfeat = attr(ind, "fitness")[2]))))
}


getfwfromid <- function(id) {
  tab <- getfw(loadResult(id))
  tab$job.id <- id
  tab
}

getnumfromid <- function(id) {
  tab <- getnum(loadResult(id))
  tab$job.id <- id
  tab
}


allnuminfos <- rbindlist(lapply(consider$job.id, getnumfromid))


consider[, list(problem, x = 1:2)]


allinfos <- rbindlist(lapply(consider$job.id, getfwfromid))

allinfos
consider

fullestinfo <- consider[allinfos, on = "job.id"]

fullestnuminfo <- consider[allnuminfos, on = "job.id"]

saveRDS(fullestinfo, "filterweights.rds")
saveRDS(fullestinfo, "~/filterweights.rds")
saveRDS(fullestnuminfo, "numstrat.rds")
saveRDS(fullestnuminfo, "~/numstrat.rds")

abline(log(0.1), 0)

t.test(fullestnuminfo$numstrat - log(0.1))

exp(mean(fullestnuminfo$numstrat) - log(0.1))

summary(lm(I(numstrat - log(0.1)) ~ 0 + learner, fullestnuminfo))

exp(mean(fullestnuminfo[learner == "kknn", ]$numstrat - log(0.1)))

plot(fullestnuminfo$numstrat, as.factor(fullestnuminfo$learner))



head(as.factor(fullestnuminfo$learner))


plot(as.data.frame(fullestinfo)[-c(1:4, 11)])

ggplot(fullestinfo[problem == "USPS"], aes(y = propfeat, x = randomForestSRC_var.select, color = learner)) + geom_point()
ggplot(fullestinfo[problem == "USPS"], aes(y = propfeat, x = randomForestSRC_var.select, color = learner)) + geom_point()
ggplot(fullestinfo[problem == "cnae-9"], aes(y = propfeat, x = DUMMY, color = auc)) + geom_point()

meanfi <- fullestinfo[, list(mean(FSelectorRcpp_information.gain), mean(randomForestSRC_var.select), mean(praznik_JMI), mean(auc), mean(praznik_CMIM), mean(DUMMY)),
  by = c("problem", "learner", "chw.bitflip")]

momofi <- melt(meanfi, measure.vars = paste0("V", 1:6))

ggplot(momofi, aes(x = variable, y = value, color = problem)) + geom_jitter(width = .1)

summary(lm(V2 ~ V4, meanfi))

cor(meanfi[, paste0("V", 1:6)])

heatmap(as.matrix(meanfi[, paste0("V", 1:6)]))

meanfi


head(meanfi)


momofi


FILTER[["custom"]]

meanfi

matplot(as.matrix(meanfi[, paste0("V", 1:6)]))
fullestinfo



ijoin(consider, allinfos)



getfw(loadResult(consider[1, ]))


xres <- loadResult(ijoin(tab, tabuniq[7, ])[1, ])
xres$result$control$p.mut
xres$result$control$p.recomb

print.function(environment(xres$result$control$mutate)$operators$selector.selection)

head(environment(environment(xres$result$control$mutate)$strats$selector.selection)$reset.dists)


res <- readRDS("results_raw/O/result.rds")

files <- paste0(dir("results_raw", "^O.*", full.names = TRUE), "/result.rds")

fullres <- readRDS("results_raw/OIHFiFmS/result.rds")
fullres <- fullres[problem %nin%  c("eating", "gina_agnostic", "dilbert", "philippine", "AP_Lung_Uterus", "hypersphere.200.50", "hypersphere.200.200")]


agdt <- function(ds) {
  as.data.frame(sapply(colnames(ds[[1]]), simplify = FALSE, function(cn) {
    rowMeans(sapply(ds, function(d) d[[cn]]))
  }))
}

allequiv <- sapply(files, function(f) {
  res <- readRDS(f)
  res <- res[problem %nin% c("eating", "gina_agnostic", "dilbert", "philippine", "AP_Lung_Uterus", "hypersphere.200.50", "hypersphere.200.200")]

  fullresmeaned <- fullres[, list(fullresult = list(agdt(result))), by = c("algorithm", "problem", "learner")]
  resmeaned <- res[, list(result = list(agdt(result))), by = c("algorithm", "problem", "learner")]

  frx <- fullresmeaned[resmeaned, on = c("algorithm", "problem", "learner")]

  frx$xres <- lapply(seq_len(nrow(frx)), function(idx) {
    tr <- frx$result[[idx]]
    fr <- frx$fullresult[[idx]]
    tr$equiv <- sapply(tr$naive.hout.domHV, function(n) fr$evals[min(which(fr$naive.hout.domHV >= n), nrow(fr))])
    tr
  })
  agdt(frx[learner == "SVM"]$xres)$equiv
})

table(fullres$learner)

matplot(fullres$result[[1]]$evals, allequiv)
abline(0, 1)

colnames(allequiv)

plot(frx[, list(xres = list(agdt(xres))), by = c("learner")]$xres[[1]][c("evals", "equiv")])

plot(1:10)

pop <- readRDS("results_raw/OIHFiFmS/population.rds")






nrow(fullres)

table(fullres$problem)




res



fullres

class(res)

str(res$result[[1]])

res$learner



tryalg <- ijoin(tab[algorithm == "mosmafs" & job.id > 200], findDone())[100, ]

tab


fitnesses <- reduceResultsList(tab[algorithm == "mosmafs" & problem == "madeline" &
                                    learner == "xgboost" & initialization == "unif" &
                                    filter == "custom" & filter.during.run &
                                    chw.bitflip & adaptive.filter.weights],
  function(x) t(x$result$pareto.front))

ggsummand <- function(fitnesses, color, alpha, cutrefpt = FALSE) {
  if {cutrefpt) {
    refpt <- max(unname(apply(do.call(cbind, fitnesses), 1, max))*1.1)
  } else {
    refpt <- c(1, 1)
  }
  refpt <- rep(refpt, 2)
  allparetos <- rbindlist(lapply(seq_along(fitnesses), function(fidx) {
    fi <- fitnesses[[fidx]]
    rownames(fi) <- c("mmce", "featfrac")
#    fi <- log(fi)
    dfe <- as.data.table(paretoEdges(t(fi), refpt))
    dfe$group <- fidx
    dfe
  }))

  refline <- data.frame(mmce = refpt[1], featfrac = refpt[2], point = FALSE,
    group = unique(allparetos$group))

  ggplot(allparetos, aes(x = mmce, y = featfrac, group = group)) +  # TODO: achsen von 0 bis refpt
    geom_polygon(data = rbind(allparetos, refline),
      alpha = alpha, fill = color) +
    geom_line(data = allparetos, size = .1) +
    geom_point(data = allparetos[point == TRUE])
}


ggsummand(fitnesses, "red", 0.1)






plot(paretoEdges((demoresult$result$pareto.front), c(1, 1))[1:2])

individuals <- demoresult$result$last.population




names(demoresult$result)

demoresult$result$task$fitness.fun()

getPF <- function(individuals) {
  fitnesses <- sapply(individuals, attr, which = "fitness")
  which.dominating <- nondominated(fitnesses)
  individuals[which.dominating]
}

getMedianHPs <- function(individuals) {
  pfront <- getPF(individuals)
  pff <- sapply(pfront, attr, which = "fitness")

  which.lower.median <- order(pff[2, ])[ceiling(ncol(pff) / 2)]

  dropNamed(pfront[[which.lower.median]], "selector.selection")
}

replaceWithHPs <- function(individuals, hps) {
  lapply(individuals, function(x) {
    assertSetEqual(names(x), c(names(hps), "selector.selection"))
    insert(x, hps)
  })
}

replaceWithHPs(getPF(individuals), getMedianHPs(individuals))


###


setOMLConfig(cachedir = "~/omlcache")

ot <- getOMLTask(167194)

tt <- convertOMLTaskToMlr(ot)

tt$mlr.rin





rr <- loadResult(consider[problem == "wdbc", ][1, ])

trainidx <- as.integer(rownames(rr$task.train$env$data))
testidx <- as.integer(rownames(rr$task.test$env$data))

plot(sort(c(trainidx, testidx)))

plot(trainidx, tt$mlr.rin$train.inds[[1]])

length(trainidx)
length(tt$mlr.rin$train.inds[[1]])

head(tt$mlr.task$env$data)

rr$task.train$env$data[match(0:5, trainidx), ]

