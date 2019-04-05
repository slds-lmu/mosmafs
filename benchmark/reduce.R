library(batchtools)
library(dplyr)

source("helpers.R")
source("probdesign.R")

# load registry
reg = loadRegistry("registry")
tab = summarizeExperiments(by = c("job.id", "algorithm", 
	"problem", "learner", "maxeval", "filter", "initialization", 
	"lambda", "mu", "parent.sel", "chw.bitflip", "adaptive.filter.weights",
	"filter.during.run", "surrogate", "MBMOmethod", "propose.points"))
tab = tab[maxeval %in% c(4000), ]
tab = rbind(tab[lambda != 4L, ], tab[is.na(lambda), ])

path = "results_raw"

problems = c("wdbc", "ionosphere", "sonar", "hill-valley", "clean1", 
  "tecator", "USPS", "madeline", "lsvt", "madelon", "isolet", "cnae-9")

experiments = list(
	O = data.table(algorithm = "mosmafs", filter = "none", initialization = "none", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OI = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OIFi = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OIFiFm = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = TRUE),
	OIFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = TRUE, filter.during.run = TRUE),
	OIH = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OIHFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE),
	RS = data.table(algorithm = "randomsearch", initialization = "none", filter = "none"),
	RSI = data.table(algorithm = "randomsearch", initialization = "unif", filter = "none"),
	RSIF = data.table(algorithm = "randomsearch", initialization = "unif", filter = "custom")
	)

collectBenchmarkResults(path, experiments, tab)
collectParetofront(path, experiments = experiments[c("O", "OIHFiFmS", "RS", "RSI", "RSIF")], tab, problems, learners = c("SVM", "kknn"))






# lrn = "SVM"
# datasets = c(
# 	"wdbc", "ionosphere", "sonar", "hill-valley", "clean1", 
# 	"tecator", "USPS", "madeline", "lsvt", "madelon", "isolet", "cnae-9")

# for (ds in datasets) {
# 	tabdone = ijoin(tab, findDone(), by = "job.id")
# 	fitnesses <- reduceResultsList(tab[algorithm == "mosmafs" & problem == ds &
#                                     learner == lrn & initialization == "unif" &
#                                     filter == "custom" & filter.during.run &
#                                     chw.bitflip & adaptive.filter.weights],
#   			function(x) t(x$result$pareto.front))

# 	saveRDS(fitnesses, paste("results_raw/pareto_examples/", lrn, "/", ds, ".rds", sep = ""))
# }

# ggsummand <- function(fitnesses, color, alpha) {
#   allparetos <- rbindlist(lapply(seq_along(fitnesses), function(fidx) {
#     fi <- fitnesses[[fidx]]
#     rownames(fi) <- c("mmce", "featfrac")
#     dfe <- as.data.table(paretoEdges(t(fi), c(1, 1)))
#     dfe$group <- fidx
#     dfe
#   }))

#   refpt <- data.frame(mmce = 1, featfrac = 1, point = FALSE,
#     group = unique(allparetos$group))

#   p = ggplot(allparetos, aes(x = mmce, y = featfrac, group = group)) 
#   p = p + geom_polygon(data = rbind(allparetos, refpt), alpha = alpha, fill = color) 
#   p = p + geom_line(data = allparetos, colour = "grey", alpha = 0.6) 
#   p = p + geom_point(data = allparetos[point == TRUE], alpha = 0.2)
#   p = p + scale_colour_Publication() + theme_Publication() 
#   # p = p + ylab("Value") + labs(colour = "Variant", lty = "Algorithm") 
#   p = p + xlim(c(0, 1)) + ylim(c(0, 1)) + coord_fixed()
#   p = p + xlab(expression(mmce[outer])) + ylab("Fraction of features selected")
#   p

# }

# for (lrn in c("SVM", "xgboost", "kknn")) {
# 	plist = list()
# 	for (ds in datasets) {
# 		fitnesses = readRDS(paste("results_raw/pareto_examples/", lrn, "/", ds, ".rds", sep = ""))
# 		plist[[ds]] = ggsummand(fitnesses, "red", 0.05)
# 	    plist[[ds]] = plist[[ds]] + ggtitle(ds) + theme_Publication(base_size = 8)
# 	    # ggsave(file.path(plotspath, "front", lrn, paste(ds, "front.pdf", sep = "_")), plist[[ds]], width = 5, height = 5, device = "pdf")
# 	}	
# 	g = do.call("grid.arrange", plist)
# 	ggsave(file.path(plotspath, "front", paste("all_fronts_", lrn, ".pdf", sep = "")), g, width = 8, height = 10, device = "pdf")
# }


