
# Demo file on how to run a single evaluation
# TOC:
# * load libraries / source files
# * create task
# * create learner
# * specify parameter set
# * create operators
# * create fitness function
# * initialize population
# * call ecr()
# * analyse result


# --- load libraries ---
# devtools::install_github("jakobbossek/ecr2")
source("datagen.R")
source("ecrshims.R")
source("selectorcpo.R")
source("customnsga2.R")
source("operators.R")

library("magrittr")
library("ggplot2")
library("parallelMap")

# --- create task ---
dataset <- create.hypersphere.data(4, 10000)
keep <- apply(dataset$X, 1, function(x) all(x > 0))
dataset$Y <- dataset$Y[keep]
dataset$X <- dataset$X[keep, ]

task <- dataset %>% create.classif.task(id = "hypersphere") %>%
  task.add.permuted.cols(5)

# --- create filter values ---
# this step can take long for large tasks / slow methods, so should probably be cached.
#
# including 'variance' as abogus filter value that is useless in this example here.
filtervals <- generateFilterValuesData(task, method = c("praznik_JMI", "auc", "anova.test", "variance"))
filtervals <- filtervals$data[-(1:2)]

# --- get feature inclusion probabilities from filter values ---
# FILTERMAT should be a matrix with nrow == number of features, ncol == number of filters we consider

EXPECTFEATS <- 5  # expectation value of number of features to include in genesis population
MINPROB <- .1  # min prob per feature to be in genesis pop
MAXPROB <- .9  # max prob per feature to be in genesis pop

assertNumber(EXPECTFEATS / getTaskNFeats(task), lower = MINPROB, upper = MAXPROB)

FILTERMAT <- apply(filtervals, 2, function(col) {
  col <- col - mean(col)
  meanprob <- EXPECTFEATS / getTaskNFeats(task)
  if (0 %in% range(col)) {
    return(col + meanprob)
  }
  shrinkage <- max(range(col) / (c(MINPROB, MAXPROB) - meanprob))
  col / shrinkage + meanprob
})

#  matplot(FILTERMAT)

# --- create learner ---
lrn <- cpoSelector() %>>% makeLearner("classif.ksvm", type = "C-svc", kernel = "polydot")

# --- specify parameter set ---
ps <- pSS(
  scaled: logical,
  C: numeric[0.1, 10],
  scale: numeric[0.1, 10],
  offset: numeric[-10, 10],
  selector.selection: logical^getTaskNFeats(task),
  filterweights: numeric[.Machine$double.eps, 1 - .Machine$double.eps]^ncol(FILTERMAT)
)

# --- create operators ---
# the warnings can be ignored
mutator <- combine.operators(ps,
  numeric = mutGauss,
  logical = mutBitflip,
  integer = mutUniformInt,
  discrete = mutRandomChoice,
  selector.selection = mutUniformReset,
  .strategy.selector.selection = makeFilterStrategy(FILTERMAT, "filterweights", "reset.dist")
)

crossover <- combine.operators(ps,
  numeric = recSBX,
  integer = recIntSBX,
  discrete = recPCrossover,
  logical = recUnifCrossover)

# --- create fitness function ---
resinst <- makeResampleInstance(makeResampleDesc("CV", iters = 10, stratify = TRUE), task = task)
fitness.fun <- function(args) {
  args <- args[intersect(names(args), getParamIds(getParamSet(lrn)))]
  val <- resample(setHyperPars(lrn, par.vals = args), task, resinst, show.info = FALSE)$aggr
  propfeat <- mean(args$selector)
  c(val, propfeat)
}

# --- initialize population ---
MU <- 15
LAMBDA <- 4
initials <- sampleValues(ps, MU, discrete.names = TRUE)

initials <- lapply(initials, function(x) {
  x$selector.selection <- as.logical(mutUniformMetaReset(x$selector.selection, p = 1, FILTERMAT, x$filterweights))
  x
})

# --- call ecr() ---
parallelStartMulticore()

results <- my.nsga2(fitness.fun = fitness.fun, n.objectives = 2, minimize = TRUE,
  mu = MU, lambda = LAMBDA,
  mutator = mutator, recombinator = crossover,
  representation = "custom",
  initial.solutions = initials,
  log.pop = TRUE,
  terminators = list(stopOnIters(100)))

# --- analyse result ---
fitnesses <- function(results) {
  pops <- getPopulations(results$log)
  do.call(rbind, lapply(seq_along(pops), function(idx) {
    pop <- pops[[idx]]
    df <- as.data.frame(t(pop$fitness))
    colnames(df) <- c("perf", "propfeat")
    df$iter <- idx
    df
  }))
}

# pareto front
plotFront(results$pareto.front)

# development of pareto fronts
ggplot(data = fitnesses(results), aes(x = perf, y = propfeat, color = iter)) + geom_point()

# pareto set
results$pareto.set

# features in task that are original
task$orig.features

# population at generation x
x <- 50
getPopulations(results$log)[[x]]


