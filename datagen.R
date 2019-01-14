
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
# \epsilon normally distributed with stdev [error],
# beta = shuffle(beta') and
# and beta'[i] = beta0 * q ^ (i - 1) for i = 1..p
#
# Returns a list (X=[Matrix], Y=[vector], beta = [vector])
#
create.linear.data <- function(n, p, q = exp(-1), beta0 = 1, rho = 0, error = 1) {
  cormat <- sapply(seq_len(p), function(x) rho ^ abs(x - seq_len(p)))
  print(cormat)
  X <- mvrnorm(n, rep(0, p), cormat)
  X <- X[, sample(p)]

  beta <- beta0 * q ^ seq(0, p - 1)
  beta <- sample(beta)

  epsilon <- rnorm(n, sd = error)

  Y <- X %*% beta + epsilon

  list(X = X, Y = Y, beta = beta)
}

# returns list (task, beta)
create.linear.regr.task <- function(id, ...) {
  data <- create.linear.data(...)
  list(task = makeRegrTask(id, cbind(X = data$X, Y = data$Y), target = "Y"),
    beta = data$beta)
}

# return list (task, beta, Y)
# Task has a binary target depending on whether Y is less or greater than [cutoff]
create.linear.classif.task <- function(id, cutoff = 0, ...) {
  data <- create.linear.data(...)
  list(task = makeClassifTask(id, cbind(X = data$X, Y = ifelse(data$Y < cutoff, "-", "+")), target = "Y", positive = "+"),
    beta = data$beta, Y = data$Y)
}


