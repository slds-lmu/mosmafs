source("reduce.R")

dir.create("registry/plots")
dir.create("registry/plots/domhypervol")
dir.create("registry/plots/paretofront")

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

for (i in 1:nrow(res)) {
	job.id = res[i, ]$job.id
	data = res[i, ]$result[[1]]
	data.fitnesses = fitnesses(data)

	p = ggplot(data = data.fitnesses, aes(x = perf, y = propfeat, color = iter)) + geom_point()
	p = p + ylim(c(0, 1)) + xlim(c(0, 1))
	ggsave(filename = paste("registry/plots/paretofront/", job.id, ".png", sep = ""), plot = p)
}

test = res[3, ]$result[[1]]


test.fitnesses = fitnesses(test)

# development of pareto fronts
p

# pareto set
fitness.matrix = t(as.matrix(test.fitnesses))
hypervol = sapply(unique(test.fitnesses$iter), function(x) computeHV(fitness.matrix[c(1, 2), fitness.matrix[3, ] == x], ref.point = c(1, 1)))
df = data.frame(iter = unique(test.fitnesses$iter), hypervol = hypervol)

p = ggplot(data = df, aes(x = iter, y = hypervol)) + geom_line()
p = p + ylim(c(0, 1)) 
p


