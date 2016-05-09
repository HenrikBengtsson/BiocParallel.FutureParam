library("BiocParallel.FutureParam")
register(FutureParam())
plan(lazy)

message("*** bplapply() w/ FutureParam ...")

message("- sqrt()")
x <- 1:10
expected <- lapply(x, FUN=sqrt)
message("  sqrt")
current <- bplapply(x, FUN=sqrt)
stopifnot(identical(expected, current))

message("- identity()")
current <- bplapply(list(), FUN=identity)
stopifnot(identical(list(), current))


message("*** bplapply() w/ FutureParam ... DONE")


