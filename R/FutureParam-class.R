### =========================================================================
### FutureParam objects
### -------------------------------------------------------------------------

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Constructor
###

#' @importFrom methods new setRefClass
#' @importFrom BiocParallel bplogdir
.FutureParam <- setRefClass("FutureParam",
    contains="BiocParallelParam",
    fields=list(
        logdir="character"),
    methods=list(
        initialize = function(...,
            threshold="INFO",
            logdir=NA_character_)
        {
            callSuper(...)
            initFields(threshold=threshold, logdir=logdir)
        },
        show = function() {
            callSuper()
            cat("  bplogdir: ", bplogdir(.self), "\n", sep="")
        })
)

#' Creates a FutureParam object
#'
#' @param catch.errors ...
#' @param stop.on.error ...
#' @param log ...
#' @param threshold ...
#' @param logdir ...
#'
#' @return A \link[BiocParallel:BiocParallelParam]{BiocParallelParam} object of class FutureParam
#'
#' @export
#' @importFrom methods validObject
FutureParam <-
    function(catch.errors=TRUE, stop.on.error = TRUE,
             log=FALSE, threshold="INFO", logdir=NA_character_)
{
    if (!missing(catch.errors))
        warning("'catch.errors' is deprecated, use 'stop.on.error'")

    x <- .FutureParam(workers=1L, catch.errors=catch.errors,
                      stop.on.error=stop.on.error,
                      log=log, threshold=threshold, logdir=logdir)
    validObject(x)
    x
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods - control
###

#' @importFrom methods setMethod
setMethod("bpworkers", "FutureParam", function(x) 1L)

#' @importFrom methods setMethod
setMethod("bpisup", "FutureParam", function(x) TRUE)

#' @importFrom methods setReplaceMethod validObject
setReplaceMethod("bplog", c("FutureParam", "logical"),
    function(x, value)
{
    x$log <- value
    validObject(x)
    x
})

#' @importFrom methods setReplaceMethod
setReplaceMethod("bpthreshold", c("FutureParam", "character"),
    function(x, value)
{
    x$threshold <- value
    x
})

#' @importFrom methods setMethod
setMethod("bplogdir", "FutureParam",
    function(x)
{
    x$logdir
})

#' @importFrom methods setReplaceMethod
setReplaceMethod("bplogdir", c("FutureParam", "character"),
    function(x, value)
{
    .valid.SnowParam.log <- BiocParallel:::.valid.SnowParam.log

    if (!length(value))
        value <- NA_character_
    x$logdir <- value
    if (is.null(msg <- .valid.SnowParam.log(x)))
        x
    else
        stop(msg)
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods - evaluation
###

#' @importFrom methods setMethod
#' @importFrom BiocParallel bplog bpok bpparam bpstopOnError bpthreshold bptimeout
setMethod("bplapply", c("ANY", "FutureParam"),
    function(X, FUN, ..., BPREDO=list(), BPPARAM=bpparam())
{
    .composeTry <- BiocParallel:::.composeTry
    .error_bplist <- BiocParallel:::.error_bplist
    .log_load <- BiocParallel:::.log_load
    .redo_index <- BiocParallel:::.redo_index

    if (!length(X))
        return(list())

    FUN <- match.fun(FUN)

    idx <- .redo_index(X, BPREDO)
    if (any(idx))
        X <- X[idx]

    .log_load(bplog(BPPARAM), bpthreshold(BPPARAM))

    FUN <- .composeTry(FUN, bplog(BPPARAM), bpstopOnError(BPPARAM),
                       stop.immediate=bpstopOnError(BPPARAM),
                       timeout=bptimeout(BPPARAM))

    res <- lapply(X, FUN, ...)

    names(res) <- names(X)

    if (any(idx)) {
        BPREDO[idx] <- res
        res <- BPREDO
    }

    if (!all(bpok(res)))
        stop(.error_bplist(res))

    res
})

.bpiterate_serial <- function(ITER, FUN, ..., REDUCE, init)
{
    ITER <- match.fun(ITER)
    FUN <- match.fun(FUN)
    N_GROW <- 100L
    n <- 0
    result <- vector("list", n)
    if (!missing(init)) result[[1]] <- init
    i <- 0L
    repeat {
        if(is.null(dat <- ITER()))
            break
        else
            value <- FUN(dat, ...)

        if (missing(REDUCE)) {
            i <- i + 1L
            if (i > n) {
                n <- n + N_GROW
                length(result) <- n
            }
            result[[i]] <- value
        } else {
            if (length(result))
                result[[1]] <- REDUCE(result[[1]], unlist(value))
            else
                result[[1]] <- value
        }
    }
    length(result) <- ifelse(i == 0L, 1, i)
    result
}

#' @importFrom methods setMethod
#' @importFrom BiocParallel bplog bpparam bpthreshold
setMethod("bpiterate", c("ANY", "ANY", "FutureParam"),
    function(ITER, FUN, ..., BPPARAM=bpparam())
{
    ITER <- match.fun(ITER)
    FUN <- match.fun(FUN)

    .composeTry <- BiocParallel:::.composeTry
    .log_load <- BiocParallel:::.log_load

    .log_load(bplog(BPPARAM), bpthreshold(BPPARAM))

    FUN <- .composeTry(FUN, bplog(BPPARAM), bpstopOnError(BPPARAM),
                       timeout=bptimeout(BPPARAM))

    .bpiterate_serial(ITER, FUN, ...)
})
