source("incl/start.R")

strategies <- future:::supportedStrategies()
strategies <- setdiff(strategies, "multiprocess")

register(FutureParam())
plan(sequential)

message("*** bpvec() w/ FutureParam ...")

## Serial version of pvec()
svec <- function(...) {
  parallel::pvec(..., mc.cores=1L)
}


for (strategy in strategies) {
  message(sprintf("- plan('%s') ...", strategy))
  plan(strategy)

  message("  - sqrt()")
  a <- 1:10
  expected <- svec(a, FUN=sqrt)
  current <- bpvec(a, FUN=sqrt)
  stopifnot(identical(expected, current))

  message(sprintf("- plan('%s') ... DONE", strategy))
} ## for (strategy ...)

message("*** bpvec() w/ FutureParam ... DONE")

source("incl/end.R")
