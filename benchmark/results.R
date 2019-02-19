library(ggplot2)
library(data.table)
library(ecr)

# source("reduce.R")
dir.create("plots/performance-front")
dir.create("plots/performance-population")
dir.create("plots/runtime")
dir.create("plots/hypvervol")

res = readRDS("res_test.rds")
runtime = readRDS("runtime_test.rds")

# runtime plot


# development of hypervolume
vars = unique(res[, c("problem", "n", "p.noise", "learner")])

# can this be done more efficiently?
dfs = lapply(res$job.id, function(x) as.matrix(cbind(job.id = x, res[job.id == x, ]$result[[1]])))
domhypervol = lapply(seq_along(dfs), function(x) cbind(job.id = unique(dfs[[x]][, "job.id"]), iter = unique(dfs[[x]][, "iter"]), domHV = sapply(unique(dfs[[x]][, "iter"]), function(i) computeHV(t(dfs[[x]][dfs[[x]][, "iter"] == i, c("perf", "propfeat")])))))


# one mu, different lambdas
prob = "hypersphere"
idx = which(res$problem == prob)
p = ggplot(data = )









    data.fitnesses = fitnesses(data)
    fitness.matrix = t(as.matrix(data.fitnesses))
    hypervol = sapply(unique(data.fitnesses$iter), function(x) computeHV(fitness.matrix[c(1, 2), fitness.matrix[3, ] == x], ref.point = c(1, 1)))


# dataframe

  job.id = res[i, ]$job.id
  data = res[i, ]$result[[1]]

  if (res[i, ]$algorithm == "mosmafs") {
      data.fitnesses = fitnesses(data)
      p = ggplot(data = data.fitnesses) + geom_point(aes(x = perf, y = propfeat, color = iter))
  } else {
    data.fitnesses = data.frame(t(res[i, ]$result[[1]]$fitnesses))
    names(data.fitnesses) = c("perf", "propfeat")



# x pop | isnew | fitness valid | fitness test set | nfeat | hyperpars | features |





# Viz 1: Performance of Population on Test set (boxplots) vs. iteration

# Viz 2: Performance of Population on Validation set (boxplots) vs. iteration

# Viz 3: Plot Development of Pareto-Fronts (on test set)

# Viz 4: Plot Developtment of Whole Population 

# Viz 5: Compare initial population sizes

# Viz 6: Compare Offspring sizes






for (i in 1:nrow(res)) {
	job.id = res[i, ]$job.id
	data = res[i, ]$result[[1]]

  if (res[i, ]$algorithm == "mosmafs") {
      data.fitnesses = fitnesses(data)
      p = ggplot(data = data.fitnesses) + geom_point(aes(x = perf, y = propfeat, color = iter))
  } else {
    data.fitnesses = data.frame(t(res[i, ]$result[[1]]$fitnesses))
    names(data.fitnesses) = c("perf", "propfeat")
    p = ggplot(data = data.fitnesses) + geom_point(aes(x = perf, y = propfeat))

  }
	# p = p + geom_line()
  p = p + ylim(c(0, 1)) + xlim(c(0, 1)) + theme_bw()
	if (res[i, ]$problem == "hypersphere") {
      p = p + ggtitle(paste(res[i, ]$problem, "data, p.inf =", res[i, ]$p.inf, ", p.noise = ", res[i, ]$p.noise, ", filter.method = ", res[i, ]$filter.method, sep = ""))
  } else {
      p = p + ggtitle(paste(res[i, ]$problem, ", filter.method = ", res[i, ]$filter.method, sep = ""))    
  }

  ggsave(filename = paste("registry/plots/paretofront/", job.id, ".png", sep = ""), plot = p)
}

df.list = list()
c = 1

for (i in 1:nrow(res)) {
  job.id = res[i, ]$job.id
  data = res[i, ]$result[[1]]
  if (res[i, ]$algorithm == "mosmafs") {
    data.fitnesses = fitnesses(data)
    fitness.matrix = t(as.matrix(data.fitnesses))
    hypervol = sapply(unique(data.fitnesses$iter), function(x) computeHV(fitness.matrix[c(1, 2), fitness.matrix[3, ] == x], ref.point = c(1, 1)))

    df.list[[c]] = data.frame(job.id = job.id, iter = unique(data.fitnesses$iter), hypervol = hypervol)
    c = c + 1
  } else {
    data.fitnesses = data.frame(t(res[i, ]$result[[1]]$fitnesses))
    names(data.fitnesses) = c("perf", "propfeat")
  }

}

df = do.call("rbind", df.list)

df = merge(df, tab, by = "job.id", all.x = TRUE)

res.grouped = setDT(res)[, , by = c("problem", "filter.method", "p.inf", "p.noise")]

exps = unique(df[, c("problem", "p.inf", "p.noise", "n")])

for (i in 1:nrow(exps)) {
  dfplot = merge(df, exps[i, ], by = c("problem", "p.inf", "p.noise", "n"))
  p = ggplot(data = dfplot, aes(x = as.factor(iter), y = hypervol, fill = filter.method)) + geom_boxplot() + theme_bw()
  p = p + ylim(c(0, 1)) 
  if (dfplot[i, ]$problem == "hypersphere") {
      p = p + ggtitle(paste(dfplot[i, ]$problem, "data, p.inf =", dfplot[i, ]$p.inf, ", p.noise = ", dfplot[i, ]$p.noise, sep = ""))
  } else {
      p = p + ggtitle(paste(dfplot[i, ]$problem, sep = ""))    
  }
  ggsave(filename = paste("registry/plots/domhypervol/", i, ".png", sep = ""), plot = p, width = 10, height = 7)

}


