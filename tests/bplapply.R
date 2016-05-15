library("BiocParallel.FutureParam")
oopts <- options(mc.cores=2L, warn=1L)
strategies <- future:::supportedStrategies()
strategies <- setdiff(strategies, "multiprocess")

register(FutureParam())
plan(lazy)

message("*** bplapply() w/ FutureParam ...")

for (strategy in strategies) {
  message(sprintf("- plan('%s') ...", strategy))
  plan(strategy)

  message("  - sqrt()")
  x <- 1:10
  expected <- lapply(x, FUN=sqrt)
  current <- bplapply(x, FUN=sqrt)
  stopifnot(identical(expected, current))

  message("  - identity()")
  current <- bplapply(list(), FUN=identity)
  stopifnot(identical(list(), current))

  message(sprintf("- plan('%s') ... DONE", strategy))
} ## for (strategy ...)

message("*** bplapply() w/ FutureParam ... DONE")

options(oopts)
