
library("MASS")
library("mlr")

# create linear model data Y = X * beta + \epsilon
# with X = shuffle_columns(X') and
# X' an [n] * [p] matrix of MVN rows with covariance matrix
#
# 1     rho rho^2 rho^3 ... rho^p
# rho     1 rho   rho^2 ... rho^(p-1)
# rho^2 rho   1   rho ...   rho^(p-2)
# ...
# rho^p ...
#
# \epsilon normally distributed with stdev 1.
# beta = shuffle(beta') and
# and beta'[i] = beta0 * q ^ (i - 1) for i = 1..p
#
# Returns a list (X=[Matrix], Y=[vector], beta = [vector])
#
create.linear.data <- function(n, p, q = exp(-1), beta0 = 1, rho = 0) {
  cormat <- sapply(seq_len(p), function(x) rho ^ abs(x - seq_len(p)))
  X <- mvrnorm(n, rep(0, p), cormat)
  X <- X[, sample(p)]

  beta <- beta0 * q ^ seq(0, p - 1)
  beta <- sample(beta)

  epsilon <- rnorm(n)

  Y <- X %*% beta + epsilon

  list(X = X, Y = Y, beta = beta)
}

# Creates matrix X and vector Y:
# sample [dim] columns of data from [dist] (which must be a function
# `n -> vector length (n)` and should probably sample randomly to create X.
# Y[i] is +1 if the L_`norm`-norm of X[i, ] is < 1, and -1 otherwise.
# Returns list(X = [Matrix], Y = [vector]
create.hypersphere.data <- function(dim, n, dist = function(x) runif(x, -1, 1), norm = 2) {
  X = replicate(dim, dist(n))
  Y = sign(1 - apply(X, 1, function(x) sum(x^norm))^(1/norm))
  list(X = X, Y = Y)
}

# data must have $X and $Y
create.regr.task <- function(id, data) {
  makeRegrTask(id, data.frame(X = data$X, Y = data$Y), target = "Y")
}

# data must have $X and $Y
# returns task with binary target Y => cutoff vs. Y < cutoff
create.classif.task <- function(id, data, cutoff = 0) {
  makeClassifTask(id, data.frame(X = data$X, Y = ifelse(data$Y < cutoff, "-", "+")), target = "Y", positive = "+")
}

# create new task identical to the old one, but with 'newdata' instead
# of old data.
clonetask <- function(task, newdata, newid) {
  rettask = switch(getTaskType(task),
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
}

# adds [num] new features sampled from [dist] to [task].
# New features are inserted at random positions in the task
# and named RANDOM.1 .. RANDOM.[num]
#
# The returned task has a new member `$orig.features` which
# is a logical vector indicating the features that were originally
# in the task.
task.add.random.cols <- function(task, num, dist = rnorm) {

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

  rettask <- clonetask(task, newdata, newid)
  rettask$orig.features <- orig.features
  rettask
}

# adds [num] copies of the task with permuted rows.
# The feature names of the 'i'th permuted copy have 'PERM.i.' prepended to them.
# The returned task has a new member `$orig.features` which is a logical vector
# indicating the features that were originally in the task.
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
  rettask <- clonetask(task, newdata, newid)
  rettask$orig.features <- orig.features
  rettask
}
