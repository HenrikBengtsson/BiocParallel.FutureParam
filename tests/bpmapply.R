library("BiocParallel.FutureParam")
oopts <- options(mc.cores=2L, warn=1L)
strategies <- future:::supportedStrategies()
strategies <- setdiff(strategies, "multiprocess")

register(FutureParam())
plan(sequential)

message("*** bpmapply() w/ FutureParam ...")

for (strategy in strategies) {
  message(sprintf("- plan('%s') ...", strategy))
  plan(strategy)

  message("  - rep()")
  a <- 1:4
  b <- 4:1
  expected <- mapply(rep, a, b)
  current <- bpmapply(rep, a, b)
  stopifnot(identical(expected, current))

  message("  - rep() w/ named arguments")
  a <- 1:4
  b <- 4:1
  expected <- mapply(rep, times=a, x=b)
  current <- bpmapply(rep, times=a, x=b)
  stopifnot(identical(expected, current))

  message("  - rep() w/ named arguments + MoreArgs")
  a <- 1:4
  b <- 4:1
  expected <- mapply(rep, times=a, MoreArgs = list(x=42))
  current <- bpmapply(rep, times=a, MoreArgs = list(x=42))
  stopifnot(identical(expected, current))

  message(sprintf("- plan('%s') ... DONE", strategy))
} ## for (strategy ...)

message("*** bpmapply() w/ FutureParam ... DONE")

options(oopts)

