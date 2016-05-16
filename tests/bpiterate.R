library("BiocParallel.FutureParam")
oopts <- options(mc.cores=2L, warn=1L)
strategies <- future:::supportedStrategies()
strategies <- setdiff(strategies, "multiprocess")

register(FutureParam())
plan(lazy)

message("*** bpiterate() w/ FutureParam ...")

## Factory creating an iterator function
countIterator <- function(max) {
  local({
    count <- 0L
    function() {
      if (count == max) return(NULL)
      count <<- count + 1L
      count
    }
  })
} ## countIterator()


for (strategy in strategies) {
  message(sprintf("- plan('%s') ...", strategy))
  plan(strategy)

  message("  - sqrt()")
  max <- 5L
  expected <- lapply(1:max, FUN=sqrt)
  ITER <- countIterator(max)
  current <- bpiterate(ITER, FUN=sqrt)
  stopifnot(identical(expected, current))

  message(sprintf("- plan('%s') ... DONE", strategy))
} ## for (strategy ...)

message("*** bpiterate() w/ FutureParam ... DONE")

options(oopts)

