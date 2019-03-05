



devtools::document("..")

devtools::load_all("..")


devtools::test("..")

Sys.setenv("FASTVIGNETTE" = "true")

tools::buildVignettes(dir = "..")



library("mlr")
library("ggplot2")
library("ecr")
library("mlrCPO")

parallelStop()

x <- 1
list(a = system.time(x <- x + 1)[3], b = x)

parallelMap::parallelStop()
data.table::setDTthreads(1)

profvis::profvis(run.simple <- slickEcr(
  fitness.fun = makeObjective(lrn, task, ps.simple, cv5, holdout.data = task),
  lambda = 32,
  population = initials.simple,
  mutator = mutator.simple,
  recombinator = crossover.simple,
  parent.selector = selTournamentMO,
  generations = 3))

getPopulations(run.simple$log)[[1]]$population

plot(t(sapply(getPopulations(run.simple$log)[[1]]$population, function(x) attr(x, "fitness"))))
points(t(sapply(getPopulations(run.simple$log)[[1]]$population, function(x) attr(x, "fitness.holdout"))), pch = "x")

mutBitflipCHW

parallelMap::parallelStartMulticore(mc.set.seed = FALSE)

run.simple <- slickEcr(
  fitness.fun = makeObjective(lrn, task, ps.simple, cv5),
  lambda = 32,
  population = initials.simple,
  mutator = mutator.simple,
  recombinator = crossover.simple,
  parent.selector = selTournamentMO,
  generations = 3)

getStatistics(run.simple$log)
getStatistics(run.simple$log.newinds)

