
devtools::load_all("..")
library("ecr")
library("ggplot2")

fitness <- matrix(rnorm(1000, 0.5, 0.1), nrow = 2)

noise.2d <- function(fitness) fitness + rnorm(1000, 0, 0.03)
noise.1d <- function(fitness) {
  fitness[c(TRUE, FALSE)] <- fitness[c(TRUE, FALSE)] + rnorm(500, 0, 0.03)
  fitness
}

naiveHoldoutDomHV <- function(fitness, holdout, refpoint = c(1, 1)) {
  computeHV(holdout[, nondominated(fitness), drop = FALSE], refpoint)
}

trueHoldoutDomHV <- function(fitness, holdout, refpoint = c(1, 1)) unbiasedHoldoutDomHV(fitness, holdout, refpoint)

true.fitness.hv <- computeHV(fitness, c(1, 1))

oracle.naive.holdout.2d.reps <- replicate(10000, naiveHoldoutDomHV(fitness, noise.2d(fitness)))
oracle.naive.holdout.1d.reps <- replicate(10000, naiveHoldoutDomHV(fitness, noise.1d(fitness)))
oracle.corrected.holdout.2d.reps <- replicate(10000, trueHoldoutDomHV(fitness, noise.2d(fitness)))
oracle.corrected.holdout.1d.reps <- replicate(10000, trueHoldoutDomHV(fitness, noise.1d(fitness)))

realistic.naive.holdout.2d.reps <- replicate(10000, naiveHoldoutDomHV(noise.2d(fitness), noise.2d(fitness)))
realistic.naive.holdout.1d.reps <- replicate(10000, naiveHoldoutDomHV(noise.1d(fitness), noise.1d(fitness)))
realistic.corrected.holdout.2d.reps <- replicate(10000, trueHoldoutDomHV(noise.2d(fitness), noise.2d(fitness)))
realistic.corrected.holdout.1d.reps <- replicate(10000, trueHoldoutDomHV(noise.1d(fitness), noise.1d(fitness)))

realistic.truegen.naive.holdout.2d.reps <- replicate(10000, naiveHoldoutDomHV(noise.2d(fitness), fitness))
realistic.truegen.naive.holdout.1d.reps <- replicate(10000, naiveHoldoutDomHV(noise.1d(fitness), fitness))
realistic.truegen.corrected.holdout.2d.reps <- replicate(10000, trueHoldoutDomHV(noise.2d(fitness), fitness))
realistic.truegen.corrected.holdout.1d.reps <- replicate(10000, trueHoldoutDomHV(noise.1d(fitness), fitness))


plot(t(fitness), xlim = c(0, 1), ylim = c(0, 1))
points(t(noise.1d(fitness)), xlim = c(0, 1), ylim = c(0, 1), pch = "x")

ggplot() +
  geom_histogram(aes(x = oracle.naive.holdout.2d.reps), bins = 100, color = "blue") +
  geom_histogram(aes(x = oracle.corrected.holdout.2d.reps), bins = 100, color = "red")



true.fitness.hv
t.test(oracle.naive.holdout.2d.reps - true.fitness.hv)
t.test(oracle.corrected.holdout.2d.reps - true.fitness.hv)

t.test(realistic.naive.holdout.2d.reps, realistic.truegen.naive.holdout.2d.reps)

t.test(realistic.corrected.holdout.2d.reps, realistic.truegen.corrected.holdout.2d.reps)

t.test(realistic.corrected.holdout.1d.reps, realistic.truegen.corrected.holdout.1d.reps)


hist(realistic.naive.holdout.2d.reps, breaks = 100, xlim = c(0.4, 0.8))    # what the method tells me, i.e.:
abline(v = mean(realistic.naive.holdout.2d.reps))                        # estimated fitness of population, using estimated points
abline(v = true.fitness.hv, col = "red")                                 # true fitness of population, using oracle points
abline(v = mean(oracle.naive.holdout.2d.reps), col = "blue")             # estimated fitness of population, using oracle points
abline(v = mean(realistic.truegen.naive.holdout.2d.reps), col = "green") # true fitness of population, using estimated points

hist(realistic.corrected.holdout.2d.reps, breaks = 100, xlim = c(0.4, 0.8))  # what the method tells me, i.e.:
abline(v = mean(realistic.corrected.holdout.2d.reps))                        # estimated fitness of population, using estimated points
abline(v = true.fitness.hv, col = "red")                                     # true fitness of population, using oracle points
abline(v = mean(oracle.corrected.holdout.2d.reps), col = "blue")             # estimated fitness of population, using oracle points
abline(v = mean(realistic.truegen.corrected.holdout.2d.reps), col = "green") # true fitness of population, using estimated points


hist(realistic.naive.holdout.1d.reps, breaks = 100, xlim = c(0.4, 0.8))    # what the method tells me, i.e.:
abline(v = mean(realistic.naive.holdout.1d.reps))                        # estimated fitness of population, using estimated points
abline(v = true.fitness.hv, col = "red")                                 # true fitness of population, using oracle points
abline(v = mean(oracle.naive.holdout.1d.reps), col = "blue")             # estimated fitness of population, using oracle points
abline(v = mean(realistic.truegen.naive.holdout.1d.reps), col = "green") # true fitness of population, using estimated points

hist(realistic.corrected.holdout.1d.reps, breaks = 100, xlim = c(0.4, 0.8))  # what the method tells me, i.e.:
abline(v = mean(realistic.corrected.holdout.1d.reps))                        # estimated fitness of population, using estimated points
abline(v = true.fitness.hv, col = "red")                                     # true fitness of population, using oracle points
abline(v = mean(oracle.corrected.holdout.1d.reps), col = "blue")             # estimated fitness of population, using oracle points
abline(v = mean(realistic.truegen.corrected.holdout.1d.reps), col = "green") # true fitness of population, using estimated points
