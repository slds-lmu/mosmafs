



devtools::document("..")

devtools::load_all("..")

devtools::test("..")

Sys.setenv("FASTVIGNETTE" = "true")

tools::buildVignettes(dir = "..")




parallelStop()

x <- 1
list(a = system.time(x <- x + 1)[3], b = x)

parallelStop()
data.table::setDTthreads(1)

profvis::profvis(run.simple <- slickEcr(
  fitness.fun = makeObjective(lrn, task, ps.simple, cv5),
  lambda = 32,
  population = initials.simple,
  mutator = mutator.simple,
  recombinator = crossover.simple,
  parent.selector = selTournamentMO,
  generations = 3))


parallelStartMulticore(mc.set.seed = FALSE)

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

