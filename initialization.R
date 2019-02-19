# A new version of the sampleValue function
# allows to specify a distribution over a function of the parameterset

resampleIndividual = function(ind, ones = 1L, length, probs = NULL) {

	res = rep(FALSE, length)
	
	if (ones > 0L) {
		idx = sample(length, ones, replace = FALSE, probs)
		res[idx] = TRUE
	}

	ind$selector.selection = res
	return(ind)
}


# modify with respect to a distribution over the number of features
resamplePopulationFeatures = function(inds, ps, minfeat = 0L, maxfeat = NULL, sampler = NULL, args, probs = NULL) {
	
	length = ps$pars$selector.selection$len

	if (is.null(maxfeat))
		maxfeat = length

	args = c(n = length(inds), args)
	
	dist = floor(do.call(sampler, args))
	dist[dist > maxfeat] = maxfeat
	dist[dist < minfeat] = minfeat

	inds = lapply(seq_along(dist), function(i) resampleIndividual(inds[[i]], ones = dist[i], length = length, probs = probs))

	return(inds)
}

