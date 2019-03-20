library(data.table)
library(ggplot2)
library(batchtools)
library(ggpubr)

mytheme = theme_pubr(base_size = 12, base_family = "", border = FALSE,
  margin = TRUE, legend = c("top", "bottom", "left", "right", "none"),
  x.text.angle = 0)

experiments = list(
	O = data.table(algorithm = "mosmafs", filter = "none", initialization = "none", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OI = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OIFi = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OIFiFm = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = TRUE),
	OIFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = TRUE, filter.during.run = TRUE),
	OIH = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OIHFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE),
	RS = data.table(algorithm = "randomsearch", initialization = "none", filter = "none")#,
	# RSI = data.table(algorithm = "randomsearch", initialization = "unif", filter = "none"),
	# RSIF = data.table(algorithm = "randomsearch", initialization = "unif", filter = "custom")
	)

# --- read the data ---
datapath = "results_raw"
plotspath = "results_plots"
reslist = lapply(names(experiments), function(x) readRDS(file.path(datapath, x, "result.rds")))
res = do.call("rbind", reslist)

# --- opt.paths per task per learner --- 
plotOptPathFacetGrid(res, plotspath)
plotRankPlotPerEval(res, plotspath, generations = c()) # steps needs to be a multiple of 15

plotRankPlotPerEval = function(res, plotspath, steps) {
	df = extractFromSummary(res, c("evals", "eval.domHV"))
  	df = df[- which(problem %in% c("hypersphere.200.50", "hypersphere.200.200")), ] 
  	df = df[evals != 8000, ]
  	df$gen = (df$evals - df$mu) / df$lambda
  	dfp = df[, .(mean.domHV = mean(eval.domHV)), by = c("algorithm", "gen", "problem", "learner", "variant")]
  	dfp = dfp[gen %in% seq(0, max(dfp$gen, na.rm = TRUE), by = steps), c("algorithm", "gen", "problem", "learner", "variant", "mean.domHV")]

  	# --- calculate ranks ---
  	dfr = dfp[, rank_variant := rank(- mean.domHV), by = c("learner", "problem", "gen")]
    # mean rank over problems and learners
    dfr_all = dfr[, .(mean(rank_variant)), by = c("gen", "variant")]
  	dfr_per_learner = dfr[, .(mean(rank_variant)), by = c("gen", "variant", "learner")]
  
    p = ggplot()
    p = p + geom_line(data = dfr_all, aes(x = gen, y = V1, colour = variant))
	p = p + theme_bw()
	p = p + scale_colour_Publication() + theme_Publication()

	for (lrn in unique(dfr$learner)) {
      p = ggplot()
	  p = p + geom_line(data = dfr_per_learner[learner == lrn, ], aes(x = gen, y = rank_variant, colour = variant))
	  p = p + theme_bw()
	  p = p + scale_colour_Publication() + theme_Publication()
      ggsave(file.path(plotspath, "eval.domHV", paste(lrn, "ranks.png", sep = ".")))
    }


	ggsave(file.path(plotspath, "eval.domHV", "ranks_all.png"))

}


	dflist = lapply(dflist, function(x) extractFromSummary(x, toextract))
	df = do.call("rbind", dflist)

	# vizualise hypervolume per evaluation	
	p = ggplot()
	p = p + geom_line(data = dfp[algorithm == "mosmafs", ], aes_string(x = "evals", y = "mean.domHV", colour = plot.by.colour, lty = plot.by.lty))
	p = p + geom_line(data = dfp[algorithm == "MBObaseline", ], aes_string(x = "evals", y = "mean.domHV"), colour = "black")
	p = p + facet_grid(learner ~ problem) + theme_bw()
	ggsave(paste("results_plots/", toextract[1], "/", paste(names(experiments), collapse = "_"), ".png", sep = ""), p, width = 15)

	for (lrn in unique(df$learner)) {
		for (prob in unique(df$problem)[c(1:6, 8:10)]) {
				dir.create(paste("results_plots/", toextract[1], "/", learner, "_", prob, sep = ""))
				dfp = df[problem == prob & learner == lrn, ]
				dfp = dfp[- which(initialization == "none"), ]
				dfp = dfp[- which(!chw.bitflip), ]
				dfp$job.id = as.factor(dfp$job.id)

				p = ggplot()
				p = p + geom_line(data = dfp, aes_string(x = "evals", y = "eval.domHV", colour = "job.id"))
				p = p + facet_grid( ~ algorithm) + theme_bw()
				ggsave(paste("results_plots/", toextract[1], "/", learner, "_", prob, "/", "plot.png", sep = ""), p, width = 15)

		}
	}








