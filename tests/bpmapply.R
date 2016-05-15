library("BiocParallel.FutureParam")
register(FutureParam())
plan(lazy)

message("*** bpmapply() w/ FutureParam ...")

message("- rep()")
a <- 1:4
b <- 4:1
expected <- mapply(rep, a, b)
current <- bpmapply(rep, a, b)
stopifnot(identical(expected, current))

message("- rep() w/ named arguments")
a <- 1:4
b <- 4:1
expected <- mapply(rep, times=a, x=b)
current <- bpmapply(rep, times=a, x=b)
stopifnot(identical(expected, current))

message("- rep() w/ named arguments + MoreArgs")
a <- 1:4
b <- 4:1
expected <- mapply(rep, times=a, MoreArgs = list(x=42))
current <- bpmapply(rep, times=a, MoreArgs = list(x=42))
stopifnot(identical(expected, current))

message("*** bpmapply() w/ FutureParam ... DONE")
