source("incl/start.R")
strategies <- all_strategies(excl = "multiprocess")

message("*** bplapply() w/ FutureParam ...")

for (strategy in strategies) {
  message(sprintf("- plan('%s') ...", strategy))
  plan(strategy)

  message("  - sqrt()")
  x <- 1:10
  expected <- lapply(x, FUN = sqrt)
  current <- bplapply(x, FUN = sqrt)
  stopifnot(identical(expected, current))

  message("  - identity()")
  current <- bplapply(list(), FUN = identity)
  stopifnot(identical(list(), current))

  mu <- 1.0
  sigma <- 2.0
  message("  - rnorm()")
  expected <- lapply(1:3, mu = mu, sigma = sigma, function(i, mu, sigma) {
    set.seed(0xBEEF)
    rnorm(i, mean = mu, sd = sigma)
  })
  current <- bplapply(1:3, mu = mu, sigma = sigma, function(i, mu, sigma) {
    set.seed(0xBEEF)
    rnorm(i, mean = mu, sd = sigma)
  })
  stopifnot(identical(expected, current))

  message(sprintf("- plan('%s') ... DONE", strategy))
} ## for (strategy ...)

message("*** bplapply() w/ FutureParam ... DONE")

source("incl/end.R")
