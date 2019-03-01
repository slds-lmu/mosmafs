library(ggplot2)
library(data.table)
library(ecr)
library(batchtools)
library(reshape2)

source("helpers.R")
# source("reduce.R")
# dir.create("results/plots/performance-front")
# dir.create("results/plots/performance-population")
# dir.create("results/plots/runtime")
respath = "results/reduced"
plotpath = "results/plots"


# create plots for hypervolume development
type = "hypervol_init"

# create directory
dir.create(file.path(plotpath, type))
res = readRDS(paste(respath, "/", type, ".rds", sep = ""))

# plot development of hypervolume
hypervol = lapply(1:nrow(res), function(x) data.frame(job.id = res[x, ]$job.id, hypervol = unlist(res[x, ]$result[[1]])))
hypervol = lapply(hypervol, function(x) ijoin(x, res[, - c("result")], by = "job.id"))
hypervol = lapply(hypervol, function(x) cbind(x, data.frame(generation = 1:nrow(x), evals = seq(from = x[1, ]$mu, by = x[1, ]$lambda, length.out = nrow(x)))))
hypervol = do.call("rbind", hypervol)
hypervol$mu = as.factor(hypervol$mu)
hypervol$lambda = as.factor(hypervol$lambda)
hypervol$initialization = as.factor(hypervol$initialization)

for (prob in unique(hypervol$problem)) {
   savepath = paste(plotpath, "/", type, "/", prob, sep = "")
   dir.create(savepath)
   for (lrn in unique(hypervol$learner)) {
      df = hypervol[learner == lrn & problem == prob, ]
      # baseline = which(df$mu == df$maxeval & df$lambda == 1L)
      df.sum = df[, hypervol := mean(hypervol), by = c("mu", "lambda", "evals", "initialization")]
      p = ggplot(data = df.sum, aes(x = evals, y = hypervol, colour = mu, lty = lambda)) + geom_line()
      # p = p + geom_hline(data = df[baseline, ], aes(yintercept = hypervol), colour = "red")
      p = p + theme_bw()
      p = p + facet_grid(~ initialization)
      p = p + ggtitle(paste(prob, " data, ", lrn, ", averaged over 5 replications", sep = ""))
      ggsave(filename = paste(savepath, "/", lrn, ".png", sep = ""), plot = p, width = 10, height = 6)
    } 
}

# calculate ranks across problems and ids
dfr = calculateRanks(hypervol, 1000L)
names(dfr)[3] = "rank_1000"
for (i in 2:10) {
  dfr = cbind(dfr, calculateRanks(hypervol, i * 1000L)$V1) 
  names(dfr)[i + 2] = paste("rank", 1000 * i, sep = "_")
}

write.csv(dfr, file.path(plotpath, type, "ranks.csv"))


# create plots for hypervolume development
type = "paretotest_init"
type = "paretovalid_init"

# create directory
dir.create(file.path(plotpath, type))
res = readRDS(paste(respath, "/", type, ".rds", sep = ""))
res[, index := seq_len(.N), by = c("problem", "mu", "lambda", "maxeval", "learner", "initialization")]

# plot development of hypervolume
df = lapply(1:nrow(res), function(x) cbind(job.id = res[x, ]$job.id, res[x, ]$result[[1]]))
df = lapply(df, function(x) ijoin(x, res[, - c("result")], by = "job.id"))
# df = lapply(1:length, function(x) cbind(x, data.frame(generation = seq(from = x[1, ]$mu, by = x[1, ]$lambda, length.out = nrow(x)))))

df = do.call("rbind", df)
names(df)[2:3] = c("mmce", "nfeat") 
df$mu = as.factor(df$mu)
df$lambda = as.factor(df$lambda)
df$index = as.factor(df$index)
df$initialization = as.factor(df$initialization)

df = df[parent.sel == "selDomHV", ]

for (prob in unique(df$problem)) {
   savepath = paste(plotpath, "/", type, "/", prob, sep = "")
   dir.create(savepath)
   for (lrn in unique(df$learner)) {
      dfs = df[learner == lrn & problem == prob, ]
      # baseline = which(dfs$lambda == 1L)
      p = ggplot(data = dfs, aes(x = mmce, y = nfeat, colour = index)) + geom_point() + geom_line()
      p = p + theme_bw()
      p = p + facet_grid(initialization~mu) + ylim(c(0, 0.1)) + xlim(c(0, 1))
      p = p + theme(legend.position = "none")
      p = p + ggtitle(paste(prob, " data, ", lrn, ", 5 replications", sep = ""))
      ggsave(filename = paste(savepath, "/", lrn, ".png", sep = ""), plot = p, width = 10, height = 6)
    } 
}



# create boxplots for accuracies 

type = "mmce_init"
# create directory
dir.create(file.path(plotpath, type))

# data for 
data = "pareto_all_init"
res = readRDS(paste(respath, "/", data, ".rds", sep = ""))
df = lapply(1:nrow(res), function(x) data.frame(job.id = res[x, ]$job.id, pareto = unlist(res[x, ]$result[[1]])))
df = lapply(df, function(x) ijoin(x, res[, - c("result")], by = "job.id"))
df = do.call("rbind", df)
names(df)[2:4] = c("generation", "mmce", "nfeat") 
df$mu = as.factor(df$mu)
df$lambda = as.factor(df$lambda)
df$initialization = as.factor(df$initialization)

# data = "paretotest"
# res2 = readRDS(paste(respath, "/", data, ".rds", sep = ""))
# df2 = lapply(1:nrow(res2), function(x) data.frame(job.id = res[x, ]$job.id, pareto = unlist(res[x, ]$result[[1]])))
# df2 = lapply(df2, function(x) ijoin(x, res[, - c("result")], by = "job.id"))
# df2 = do.call("rbind", df2)
# names(df2)[2:3] = c("mmce", "nfeat") 
# df2$mu = as.factor(df2$mu)
# df2$lambda = as.factor(df2$lambda)

for (prob in unique(df$problem)) {
   savepath = paste(plotpath, "/", type, "/", prob, sep = "")
   dir.create(savepath)
   for (lrn in unique(df$learner)) {
      dfs = df[learner == lrn & problem == prob, ]
      dfs = dfs[, .(mmce = mean(mmce), min = min(mmce), max = max(mmce)), by = c("job.id", "generation", "mu", "lambda", "initialization")]
      dfs = dfs[, .(mean.mmce = mean(mmce), min.mmce = mean(min), max.mmce = mean(max)), by = c("generation", "mu", "lambda", "initialization")]
      # dfs = dfs[- which(dfs$lambda == 1L), ]
      dfs = melt(dfs, id.vars = names(dfs)[1:4])
      p = ggplot(data = dfs, aes(x = generation, y = value, colour = variable)) + geom_line()
      p = p + theme_bw()
      p = p + facet_grid(initialization ~ mu)
      p = p + theme(legend.position = "none")
      p = p + ggtitle(paste(prob, " data, ", lrn, ", 5 replications", sep = ""))
      ggsave(filename = paste(savepath, "/", lrn, ".png", sep = ""), plot = p, width = 10, height = 6)
    } 
}












# runtime plot
# runtime depending on problem size
res$runtime = sapply(res$result, function(x) x$runtime[[3]])
res$p = as.factor(res$p.inf + res$p.noise)
p = ggplot(data = res, aes(x = n, y = runtime, colour = p, lty = learner)) + geom_line()
p


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


