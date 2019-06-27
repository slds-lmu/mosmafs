
#' @title Create Linear Model Data
#'
#' @description
#' Create linear model data `Y = X * beta + epsilon` with
#' X as a `n * p` matrix of multivariate normal distributed
#' rows with covariance matrix
#'
#' ```
#' 1     rho rho^2 rho^3 ... rho^p
#' rho     1 rho   rho^2 ... rho^(p-1)
#' rho^2 rho   1   rho ...   rho^(p-2)
#' ...
#' rho^p ...
#' ```
#' 
#' `epsilon` is standard normally distributed and 
#' `beta[i] = beta0 * q ^ (i - 1)` for `i = 1,..., p`.  
#' 
#' If `permute == TRUE`, columns of `X` as well as `beta` are permuted before
#' the linear model equation is evaluated to generate `Y`. These permuted
#' values are also the ones returned in the result.
#' 
#' `$orig.features` are the features with `beta > 1 / sqrt(n)`.
#'
#' @param n `[integer(1)]` number of rows to generate.
#' @param p `[integer(1)]` number of columns to generate.
#' @param q `[numeric(1)]` attenuation factor for beta coefficients.
#' @param beta0 `[numeric(1)]` size of first coefficient.
#' @param rho `[numeric(1)]` parameter for correlation matrix.
#' @param permute `[logical(1)]` whether to permute columns of `X` and coefficient
#'   vector (`beta`).
#' @return `list(X=[Matrix], Y=[vector], beta = [vector], orig.features = logical)`
#' @family Artificial Datasets
#' @export
create.linear.data <- function(n, p, q = exp(-1), beta0 = 1, rho = 0, permute = TRUE) {
  assertInt(n, lower = 1)
  assertInt(p, lower = 1)
  assertNumber(q, lower = 0)
  assertNumber(beta0)
  assertNumber(rho, lower = -1, upper = 1)
  assertLogical(permute, len = 1)
  
  cormat <- sapply(seq_len(p), function(x) rho ^ abs(x - seq_len(p)))
  X <- MASS::mvrnorm(n, rep(0, p), cormat)
  if (permute) {
    X <- X[, sample(p)]
  }

  beta <- beta0 * q ^ seq(0, p - 1)
  if (permute) {
    beta <- sample(beta)
  }

  epsilon <- rnorm(n)

  Y <- X %*% beta + epsilon

  list(X = X, Y = Y, beta = beta, orig.features = beta > 1 / sqrt(n))
}

#' @title Create Hypersphere Data
#'
#' @description
#' Creates hypersphere data with X  as a `n * dim` matrix of sampled columns 
#' from `dist`. `dist` must be a function 
#' `n -> vector length (n)` 
#' and should (probably) sample randomly to create `X`.
#' 
#' Y is a vector with entries `Y[i]` = `+1` if the L_`norm` of `X[i, ]` is `< radius^norm`, 
#' and `Y[i]` = `-1` otherwise.
#'
#' @param dim `[integer(1)]` number of columns to create.
#' @param n `[integer(1)]` number of sample to create.
#' @param dist `[function]` function `n` -> `numeric(n)` that is used to sample points dimension-wise.
#' @param norm `[numeric(1)]` Norm exponent.
#' @param radius `[numeric(1)]` Radius to check against.
#' @return `list(X = [Matrix], Y = [vector], orig.features = logical)`
#' @family Artificial Datasets
#' @export
create.hypersphere.data <- function(dim, n, dist = function(x) runif(x, -1, 1), norm = 2, radius = 1) {
  assertInt(dim, lower = 1)
  assertInt(n, lower = 1)
  assertFunction(dist, nargs = 1)
  assertNumber(norm)
  assertNumber(radius, lower = 0)
  X = replicate(dim, dist(n))
  Y = sign(radius^norm - apply(X, 1, function(x) sum(abs(x)^norm)))
  list(X = X, Y = Y, orig.features = rep(TRUE, dim))
}

#' @title Linear Toy Data
#'
#' @description 
#' 
#' Based on Weston (2000) Feature Selection for SVMs.
#'
#' Creates matrix `X` and vector `Y` with six dimensions out of 202 relevant and
#' equal probability of `y` = 1 or -1.
#'
#' With a prob of 0.7 we draw `xi = y * norm(i, 1)` for `i` = 1, 2, 3 and
#' `xi` = `norm(0, 1)` for `i` = 4, 5, 6.      
#' Otherwise: `xi = norm(0, 1)` for `i` = 1, 2, 3 and `xi = y * norm(i - 3, 1)` 
#' for `i` = 4, 5, 6.
#'
#' All other features are noise.
#' @param n `[integer(1)]` number of samples to draw.
#' @return `list(X = [Matrix], Y = [vector], orig.features = logical)`
#' @family Artificial Datasets
#' @export
create.linear.toy.data <- function(n) {
  assertInt(n, lower = 1, na.ok = FALSE)
  Y = sample(c(-1, 1), n, replace = TRUE)
  X1 = cbind(apply(matrix(1:3, nrow = 1), 2, function(i) Y * rnorm(n, i, 1)), replicate(3, rnorm(n)))
  X2 = cbind(replicate(3, rnorm(n)), apply(matrix(1:3, nrow = 1), 2, function(i) Y * rnorm(n, i - 3, 1)))
  u = runif(n)
  X = rbind(X1[u <= 0.7, ], X2[u > 0.7, ])
  X = cbind(X, replicate(196, rnorm(n, 0, 20)))
  list(X = X, Y = Y, orig.features = c(rep(TRUE, 6), rep(FALSE, 196)))
}


#' @title Create mlr-Task from Data
#'
#' @description
#' Both `create.regr.task` and `create.classif.task` take a numeric target column
#' `Y`, but `create.classif.task` binarizes it on `cutoff` to create a classification
#' task, while `create.regr.task` creates a regression task.
#'
#' @param id `[character(1)]` ID to use for [`Task`].
#' @param data `[named list]` with columns entries `$X`, `$Y`, and `$orig.features`.
#' @return [`Task`]
#' @family Artificial Datasets
#' @export
create.regr.task <- function(id, data) {
  assertString(id)
  assertList(data, any.missing = FALSE, min.len = 3)
  assertTRUE(all(c("X", "Y", "orig.features") %in% names(data)))
  task <- makeRegrTask(id, data.frame(X = data$X, Y = data$Y), target = "Y")
  task$orig.features <- data$orig.features
  task
}

#' @rdname create.regr.task
#' @param cutoff `[numeric(1)]` cutoff at which to binarize target.
#' @family Artificial Datasets
#' @export
create.classif.task <- function(id, data, cutoff = 0) {
  assertString(id)
  assertList(data, any.missing = FALSE, min.len = 3)
  assertTRUE(all(c("X", "Y") %in% names(data)))
  assertNumber(cutoff)
  makeClassifTask(id, data.frame(X = data$X,
    Y = ifelse(data$Y < cutoff, "-", "+")), target = "Y", positive = "+")
}

#' @title Replace Data in Task with new Data
#'
#' @description
#' Create new task identical to the old one, but with `newdata` instead
#' of old data. This should either preserve the `orig.features` of the
#' original task, or should add new noise-features, in which case `orig.features`
#' should mark the features that correspond to the full original task.
#'
#' @param task `[Task]` mlr [`Task`] to use.
#' @param newdata `[data.frame]` data to replace `task` data with; must
#'   include the target column with same name.
#' @param newid `[character(1)]` ID to use for new `Task`.
#' @param orig.features `[logical]` features that correspond to original task's data.
#' @return [`Task`]
#' @family Artificial Datasets
#' @export
clonetask <- function(task, newdata, newid,
  orig.features = rep(TRUE, ncol(newdata) - length(getTaskTargetNames(task)))) {
  assertClass(task, "Task")
  assertDataFrame(newdata)
  assertString(newid)
  assertLogical(orig.features)
  
  rettask <- switch(getTaskType(task),
    classif = {
      makeClassifTask(newid, newdata,
        getTaskTargetNames(task),
        positive = getTaskDesc(task)$positive)
    },
    regr = {
      makeRegrTask(newid, newdata,
        getTaskTargetNames(task))
    },
    stop("Unknown task type."))
  # chain orig.features
  if (!is.null(task$orig.features)) {
    stopifnot(sum(orig.features) == length(task$orig.features))
    orig.features[orig.features] = task$orig.features
  }
  rettask$orig.features = orig.features
  rettask
}

#' @title Add Sampled Noise-Features to Task
#'
#' @description
#' Adds `num` new features sampled from `dist` to `task`.
#' New features are inserted at random positions in the task
#' and named `RANDOM.1`...`RANDOM.[num]`
#'
#' The returned [`Task`] has a `$orig.features` which
#' is a logical vector indicating the features that were originally
#' in the `Task`.
#'
#' If the `$orig.features` slot is already present in
#' the input `task`, then the output will have added `FALSE` entries at
#' appropriate positions.
#' @param task `[Task]` the input task.
#' @param num `[integer(1)]` number of noise features to add.
#' @param dist `[function]` function `n` -> `numeric(n)` that samples
#'   random noise features.
#' @return [`Task`]
#' @family Artificial Datasets
#' @export
task.add.random.cols <- function(task, num, dist = rnorm) {
  
  assertClass(task, "Task")
  assertInt(num, lower = 1)
  assertFunction(dist)
  # use 'task$env$data' instead of 'getTaskData' because we want
  # the target column(s) as a data.frame
  data <- task$env$data
  target <- data[getTaskTargetNames(task)]
  data[getTaskTargetNames(task)] <- NULL
  orig.features <- sample(rep(c(TRUE, FALSE), c(ncol(data), num)))
  newdata <- cbind(data, RANDOM = replicate(num, dist(nrow(data))))
  stopifnot(length(orig.features) == ncol(newdata))
  reorder <- vector("integer", length(orig.features))
  reorder[orig.features] <- seq_len(ncol(data))
  reorder[!orig.features] <- seq(ncol(data) + 1, ncol(newdata))
  newdata <- newdata[reorder]
  newdata <- cbind(newdata, target)
  newid <- paste0(getTaskId(task), ".withrandom")

  clonetask(task, newdata, newid, orig.features)
}

#' @title Add Permuted Noise-Features to Task
#' 
#' @description 
#' Adds `num` copies of the `task` with permuted rows.
#'
#' The feature names of the `i`th permuted copy have `PERM.i.` prepended to them.
#' The returned task has a new member `$orig.features` which is a logical vector
#' indicating the features that were originally in the task.
#'
#' If the `$orig.features` slot is already present in the input `task`, then the
#' output will have added `FALSE` entries at appropriate positions.
#' @param task `[Task]` the input task.
#' @param num `[integer(1)]` Number of noise features to add.
#' @return [`Task`]
#' @family Artificial Datasets
#' @export
task.add.permuted.cols <- function(task, num) {
  data <- task$env$data
  target <- data[getTaskTargetNames(task)]
  data[getTaskTargetNames(task)] <- NULL

  perm <- paste0("PERM.", seq_len(num))
  perm <- append(perm, "", sample(num + 1, 1) - 1)

  orig.features = rep(perm == "", each = ncol(data))
  newdata <- sapply(perm, function(instname) {
    if (instname == "") return(data)
    data[sample(nrow(data)), ]
  }, simplify = FALSE)

  newdata <- do.call(cbind, c(newdata, target))

  newid <- paste0(getTaskId(task), ".withperm")
  clonetask(task, newdata, newid, orig.features)
}

