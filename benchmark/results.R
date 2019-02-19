library(ggplot2)
library(data.table)
library(ecr)

# source("reduce.R")
dir.create("plots/performance-front")
dir.create("plots/performance-population")
dir.create("plots/runtime")
dir.create("plots/hypervol")

res = readRDS("res.rds")

# runtime depending on problem size
res$runtime = sapply(res$result, function(x) x$runtime[[3]])
res$p = as.factor(res$p.inf + res$p.noise)
p = ggplot(data = res, aes(x = n, y = runtime, colour = p, lty = learner)) + geom_line()
p

# plot development of hypervolume
hypervol = lapply(1:nrow(res), function(x) data.frame(job.id = res[x, ]$job.id, hypervol = unlist(res[x, ]$result[[1]]$domhypervol), iter = 1:length(res[x, ]$result[[1]]$domhypervol)))
hypervol = do.call("rbind", hypervol)
hypervol = merge(res[, - c("result")], hypervol, all = FALSE, by = "job.id")
hypervol$mu = as.factor(hypervol$mu)
hypervol$lambda = as.factor(hypervol$lambda)

for (prob in unique(hypervol$problem)) {
   dir.create(paste("plots/hypervol/", prob, sep = ""))
   for (lrn in unique(hypervol$learner)) {
      savedir = paste("plots/hypervol/", prob, sep = "")
      df = hypervol[learner == lrn & problem == prob, ]
      baseline = which(df$mu == df$maxeval & df$lambda == 1L)
      df$strategy = paste("lambda = ", df$lambda, "mu = ", df$mu)
      p = ggplot(data = df[- baseline, ], aes(x = iter, y = hypervol, colour = strategy, lty = initialization)) + geom_line()
      p = p + geom_hline(data = df[baseline, ], aes(yintercept = hypervol), colour = "red")
      p = p + ylim(c(0, 1)) + theme_bw()
      p = p + facet_grid(n ~ p)
      p = p + ggtitle("Hypersphere data")
      ggsave(filename = paste(savedir, "/", lrn, ".png", sep = ""), plot = p, width = 10, height = 6)
    } 
}





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


