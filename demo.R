
# Demo file on how to run a single evaluation
# TOC:
# * load libraries / source files
# * create task
# * create learner
# * specify parameter set
# * create operators
# * create fitness function
# *


# --- load libraries ---
source("datagen.R")
source("ecrshims.R")
source("selectorcpo.R")
source("customnsga2.R")

library("magrittr")
library("ggplot2")
library("parallelMap")

# --- create task ---
task <- create.hypersphere.data(4, 1000) %>%
  create.classif.task(id = "hypersphere") %>%
  task.add.permuted.cols(5)

# --- create learner ---
lrn <- cpoSelector() %>>% makeLearner("classif.ksvm", type = "C-svc", kernel = "polydot")

# --- specify parameter set ---
ps <- pSS(
  scaled: logical,
  C: numeric[0.1, 10],
  scale: numeric[0.1, 10],
  offset: numeric[-10, 10],
  selector.selection: logical^getTaskNFeats(task)
)

# --- create operators ---
mutator <- combine.operators(ps,
  numeric = mutGauss,
  logical = makeMutator(function(x) mutSwap(mutBitflip(x))),
  scaled = mutBitflip)

crossover <- combine.operators(ps,
  numeric = recSBX,
  logical = recUnifCrossover)

# --- create fitness function ---
fitness.fun <- function(args) {
  val <- resample(setHyperPars(lrn, par.vals = args), task, cv10)$aggr
  propfeat <- mean(args$selector)
  c(val, propfeat)
}


MU <- 100
initials <- sampleValues(ps, MU, discrete.names = TRUE)

parallelStartMulticore()

results <- nsga2(fitness.fun = fitness.fun, n.objectives = 2, minimize = TRUE,
  mu = MU, lambda = 4,
  mutator = mutator, recombinator = crossover,
  representation = "custom",
  initial.solutions = initials,
  log.pop = TRUE,
  terminators = list(stopOnIters(100)))


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

plotFront(results$pareto.front)

ggplot(data = fitnesses(results), aes(x = perf, y = propfeat, color = iter)) + geom_point()

results$pareto.set



head(fitnesses(results))

getPopulations(results$log)[[500]]

getPopulations
