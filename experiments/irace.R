

library("mlr")
library("ecr")
library("mlrCPO")
library("magrittr")
devtools::load_all("..")

spheretask <- create.hypersphere.data(3, 200, radius = 1) %>%
  create.classif.task(id = "sphere") %>%
  task.add.permuted.cols(5)

## lrn <- makeLearner("classif.ksvm", predict.type = "prob")
##
## lrn.ps <- pSS(
##   C: numeric[10^(-3), 10^3], # according to Fröhlich et al.
##   sigma: numeric[10^(-3), 10^3]
## )

lrn <- makeLearner("classif.rpart", maxsurrogate = 0)
lrn.ps <- pSS(
  maxdepth: integer[1, 30],
  minsplit: integer[2, 30],
  cp: numeric[0.001, 0.999])

efun <- constructEvalSetting(
    spheretask, lrn, lrn.ps, measure = mlr::mmce, evals = 1e4,
    savedir = ".")

input <- sampleValue(getParamSet(efun), discrete.names = TRUE, trafo = TRUE)

# rl <- readLines("/projects/user/mosmafstraces/run_22.out")
# input <- eval(parse(text = rl[1]))


# parallelMap::parallelStart
input <- valuesFromNames(getParamSet(efun), input)

returned <- efun(input)

## analysis


savedir <- "/projects/user/mosmafstraces/"

done <- dir(savedir, pattern = "*10.rds")

patterns <- gsub("10.rds", "%s.rds", done)

traces <- lapply(patterns, function(p) {
  lapply(1:10, function(i) {
    readRDS(file.path(savedir, sprintf(p, i)))
  })
})

fidmax <- 6000
averageresults <- function(tracecollection) {
  results <- lapply(tracecollection, function(t) collectResult(t$run))
  as.data.frame(sapply(simplify = FALSE, colnames(results[[1]]), function(x) {
    rowMeans(na.rm = TRUE, as.data.frame(lapply(results, function(r) {
      yval <- c(0, r[[x]])
      if (sum(!is.na(yval)) <= 1) {
        return(rep(NA, fidmax))
      }
      approx(c(0, r$cum.fid), yval, xout = seq_len(fidmax))$y
    })))
  }))
}

results <- lapply(traces, averageresults)

rescol <- do.call(rbind, lapply(seq_along(results), function(idx) {
  r <- results[[idx]]
  r$runid <- idx
  r
}))

invertps <- function(value, ps) {
  lapply(ps$pars, function(p) {
    cval <- value[[getParamIds(p)]]
    if (isDiscrete(p, include.logical = FALSE)) {
      c(names(p$values), NA)[sapply(c(p$values, list(NULL)), function(x, y) isTRUE(all.equal(x, y)), cval)]
    } else {
      cval
    }
  })
}

PS1 <- readRDS("/projects/user/PS1.rds")
PS2 <- readRDS("/projects/user/PS2.rds")
PS3 <- getParamSet(efun)
defs <- list(ops.rec.nums = list(recGaussian = readRDS("/projects/user/PSrecGaussian.rds")))

invertps.smart <- function(value) {
  val1 <- invertps(value, PS1)
  val2 <- invertps(value, PS2)
  val3 <- invertps(value, PS3)
  for (nn in names(val1)) {
    if (length(val1[[nn]]) == 0) {
      if (length(val2[[nn]])) {
        val1[[nn]] <- val2[[nn]]
        next
      }
      if (length(val3[[nn]])) {
        val1[[nn]] <- val3[[nn]]
        next
      }
      if (nn %in% names(defs)) {
        def <- defs[[nn]]
        seln <- sapply(def, function(x, y) isTRUE(all.equal(x, y)), value[[nn]])
        if (any(seln)) {
          val1[[nn]] <- names(def)[seln]
          next
        }
      }
      catf("cannot reconstruct %s", nn)
    }
    if (length(val1[[nn]]) > 1) {
      if (nn == "ops.mut.int") {
        def <- PS1$pars[[nn]]$values
        seln <- sapply(def, function(x, y) isTRUE(all.equal(environment(x)$operator, environment(y)$operator)), value[[nn]])
        if (sum(seln) == 1) {
          val1[[nn]] <- names(def)[seln]
        }
        next
      }
      catf("cannot reconstruct %s", nn)
      val1[[nn]] = character(0)
    }
  }
  val1
}

. <- lapply(seq_along(traces), function(nt) {
  t <- traces[[nt]]
  ins <- invertps.smart(t[[1]]$params)
  NULL
})


. <- lapply(seq_along(traces), function(nt) {
  t <- traces[[nt]]
  lens <- sapply(invertps.smart(t[[1]]$params), length)
  lens <- lens[lens != 1]
  if (length(lens)) {
    cat(sprintf("%s has %s length %s\n", nt, collapse(names(lens)), collapse(lens)))
  }
})

ggplot(rescol, aes(x = cum.fid, color = as.ordered(runid))) +
  geom_line(aes(y = true.hout.domHV))

endings <- subset(rescol, cum.fid == 6000)

endings$AUC <- aggregate(rescol$true.hout.domHV, by = rescol["runid"], FUN = sum)$x

paramdf <- do.call(rbind, lapply(traces, function(t) as.data.frame(invertps.smart(t[[1]]$params))))
fullinfo <- cbind(endings, paramdf)

colnames(fullinfo)

ggplot(fullinfo, aes(y = AUC)) +
  geom_point(aes(x = init.distribution.constructor))

replace.na <- function(x, with = runif) {
  msn <- is.na(x)
  x[msn] <- with(sum(msn))
  x
}

# initial distribution
ggplot(fullinfo, aes(y = AUC, color = as.ordered(init.soften.iters))) +
  geom_point(aes(x = replace.na(init.distribution.param))) +
  facet_grid(. ~ init.distribution.constructor)

ggplot(fullinfo, aes(y = AUC, x = as.ordered(init.soften.iters))) +
  geom_boxplot()

ggplot(fullinfo, aes(y = AUC, x = as.ordered(init.soften.iters), color = init.distribution.constructor)) +
  geom_jitter(width = 0.20) +
  geom_boxplot()

ggplot(fullinfo, aes(y = AUC, x = as.ordered(init.soften.iters), color = use.SHW.init)) +
  geom_jitter(width = 0.20) +
  geom_boxplot()

summary(lm(AUC ~ init.soften.iters * use.SHW.init, data = fullinfo))

# filters
ggplot(fullinfo, aes(y = AUC, x = filters, color = filter.strategy)) +
  geom_jitter(width = 0.20) +
  geom_boxplot()

summary(lm(AUC ~ filters, data = fullinfo))
summary(lm(AUC ~ filters * filter.strategy, data = fullinfo))

ggplot(fullinfo, aes(y = AUC, x = selector.p, color = filters)) +
  geom_point()

ggplot(fullinfo, aes(y = AUC, x = selector.strategy.p)) +
  geom_jitter(width = 0.2)
summary(lm(AUC ~ selector.strategy.p, data = fullinfo))

# selection
ggplot(fullinfo, aes(y = AUC)) +
  geom_jitter(width = 0.2, aes(x = ops.parentsel))

ggplot(fullinfo, aes(y = AUC)) +
  geom_jitter(width = 0.2, aes(x = ops.survsel))

ggplot(fullinfo, aes(y = AUC, color = ops.parentsel)) +
  geom_jitter(width = 0.2, aes(x = ops.survsel))



ggplot(fullinfo, aes(y = AUC)) +
  geom_jitter(width = 0.2, aes(x = ops.tournament.sorting))

ggplot(fullinfo, aes(y = AUC)) +
  geom_point(aes(x = ops.tournament.k))

# mutation

ggplot(fullinfo, aes(y = AUC, x = ops.mut.int, color = ops.mut.strategy)) +
  geom_jitter(width = 0.2) +
  geom_boxplot()


ggplot(fullinfo, aes(y = AUC, x = ops.mut.numeric, color = ops.mut.strategy)) +
  geom_jitter(width = 0.2) +
  geom_boxplot()

ggplot(fullinfo, aes(y = AUC, x = ops.mut.sdev)) +
  geom_point()

ggplot(fullinfo, aes(y = AUC, x = ops.mut.p)) +
  geom_point()

ggplot(fullinfo, aes(y = AUC, x = ops.rec.nums, color = ops.rec.strategy)) +
  geom_jitter(width = 0.2)

summary(lm(AUC ~ ops.rec.nums + ops.rec.strategy, data = fullinfo))

ggplot(fullinfo, aes(y = AUC, x = ops.rec.crossover.p)) +
  geom_point()

summary(lm(AUC ~ ops.rec.crossover.p, data = fullinfo))

summary(lm(AUC ~ ops.rec.sbx.eta, data = fullinfo))

ggplot(fullinfo, aes(y = AUC, x = ops.rec.sbx.eta)) +
  geom_point()

ggplot(fullinfo, aes(y = AUC, x = mu)) +
  geom_point()

ggplot(fullinfo, aes(y = log(mu), x = log(lambda), color = AUC)) +
  geom_point()

ggplot(fullinfo, aes(y = AUC, x = generation.fid, color = dominance.fid)) +
  geom_jitter(width = 0.2)

ggplot(fullinfo, aes(y = AUC, x = dominance.fid, color = generation.fid)) +
  geom_jitter(width = 0.2)

summary(lm(AUC ~ generation.fid, data = fullinfo))
summary(lm(AUC ~ dominance.fid, data = fullinfo))
summary(lm(AUC ~ dominance.fid, data = subset(fullinfo, !generation.fid)))
summary(lm(AUC ~ dominance.fid*generation.fid, data = fullinfo))

ggplot(fullinfo, aes(y = AUC, x = generation.fid.point, color = dominance.fid)) +
  geom_point()

ggplot(fullinfo, aes(y = AUC, x = fixed.ri)) +
  geom_jitter(width = 0.2)

ggplot(fullinfo, aes(y = AUC, x = p.recomb, color = ops.rec.crossover.p)) +
  geom_point()

ggplot(fullinfo, aes(y = AUC, x = p.recomb, color = ops.rec.strategy)) +
  geom_point()


ggplot(fullinfo, aes(y = AUC, x = p.mut, color = ops.mut.strategy)) +
  geom_point()

infocols <- fullinfo[-(1:23)]
infocols <- infocols[!sapply(infocols, function(x) any(is.na(x)))]
summary(lm(AUC ~ ., data = infocols))

infocols <- fullinfo[-(setdiff(1:24, which(colnames(fullinfo) == "true.hout.domHV")))]
infocols <- infocols[!sapply(infocols, function(x) any(is.na(x)))]
summary(lm(true.hout.domHV ~ ., data = infocols))





fullinfo[which.min(fullinfo$AUC), ]

ggplot(results[[which.min(fullinfo$AUC)]], aes(x = cum.fid, y = true.hout.domHV)) +
  geom_line()




ggplot(fullinfo, aes(y = true.hout.domHV)) +
  geom_point(aes(x = ops.survsel))

ggplot(fullinfo, aes(y = true.hout.domHV)) +
  geom_point(aes(x = ops.tournament.k))

summary(lm(true.hout.domHV ~ ops.tournament.k, data = fullinfo))

ggplot(fullinfo, aes(y = true.hout.domHV)) +
  geom_point(aes(x = ops.tournament.sorting))

summary(lm(true.hout.domHV ~ 0 + ops.tournament.sorting, data = fullinfo))

ggplot(fullinfo, aes(y = true.hout.domHV)) +
  geom_point(aes(x = ops.mut.int))



colnames(fullinfo)


aggregate(rescol$fid.reeval, by = rescol["runid"], FUN = sum)$x > 0

plot(endings$evals, endings$runtime)
plot(endings$gen, endings$runtime)

lm(runtime~evals, data = endings)
colnames(endings)

plot(endings$fid.reeval, endings$runtime - endings$evals * 1.1)

plot(endings$runid, endings$runtime - endings$evals * 1.1)

which(endings$runtime - endings$evals * 1.1 > 3000)
plot(subset(rescol, runid == 1)$runtime)

endings
