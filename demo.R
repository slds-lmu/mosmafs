
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
resinst <- makeResampleInstance(makeResampleDesc("CV", iters = 10, stratify = TRUE), task = task)
fitness.fun <- function(args) {
  val <- resample(setHyperPars(lrn, par.vals = args), task, resinst, show.info = FALSE)$aggr
  propfeat <- mean(args$selector)
  c(val, propfeat)
}

# --- initialize population ---
MU <- 15
LAMBDA <- 4
initials <- sampleValues(ps, MU, discrete.names = TRUE)

# --- call ecr() ---
parallelStartMulticore()

results <- nsga2(fitness.fun = fitness.fun, n.objectives = 2, minimize = TRUE,
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


