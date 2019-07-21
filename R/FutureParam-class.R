#' @importFrom methods new setRefClass
#' @importFrom BiocParallel bplogdir
.FutureParam <- setRefClass("FutureParam", contains="BiocParallelParam",
  fields=list(logdir="character"),
  methods=list(
    initialize = function(...) {
      callSuper(...)
    },
    show = function() {
      callSuper()
      cat("  bplogdir: ", bplogdir(.self), "\n", sep="")
    }
  )
)

#' Creates a FutureParam object
#'
#' @param \ldots Arguments passed to the initialization method of
#'   [BiocParallel::BiocParallelParam].
#'
#' @return A [BiocParallel::BiocParallelParam] object of class FutureParam.
#'
#' @example incl/BiocParallel.FutureParam.R
#'
#' @export
#' @importFrom methods validObject
#' @rawNamespace
#' if (getRversion() >= "3.6.0") {
#'   importFrom(BiocParallel,.prototype_update)
#'   importFrom(BiocParallel,.BiocParallelParam_prototype)
#' }
FutureParam <- function(...) {
  defaults <- list()
  if (getRversion() >= "3.6.0") {
    prototype <- .prototype_update(
      .BiocParallelParam_prototype,
      workers=1L,
      ...
    )
  } else {
    ## To please R CMD check
    .prototype_update <- NULL
    .BiocParallelParam_prototype <- NULL
    prototype <- list(workers=1L, ...)
    names <- names(prototype)
    stopifnot(all(nchar(names) > 0))
    if (getRversion() >= "3.5.0") {
      if (!is.element(name <- "tasks", names)) defaults[[name]] <- 0L
      if (!is.element(name <- "catch.errors", names)) defaults[[name]] <- TRUE
      if (!is.element(name <- "stop.on.error", names)) defaults[[name]] <- TRUE
      if (!is.element(name <- "exportglobals", names)) defaults[[name]] <- TRUE
      if (!is.element(name <- "log", names)) defaults[[name]] <- FALSE
      if (!is.element(name <- "logdir", names)) defaults[[name]] <- NA_character_
      if (!is.element(name <- "threshold", names)) defaults[[name]] <- "INFO"
      prototype <- c(prototype, defaults)
    }
  }

  if (is.element("catch.errors", setdiff(names(prototype), names(defaults)))) {
    .Defunct(msg = "Argument 'catch.errors' is deprecated, use 'stop.on.error' instead.")
  }

  x <- do.call(.FutureParam, args = prototype)

  validObject(x)

  x
}

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods - control
###

#' @importFrom methods setMethod
#' @importFrom future nbrOfWorkers
setMethod("bpworkers", "FutureParam", function(x) {
  nbrOfWorkers()
})

#' @importFrom methods setMethod
setMethod("bpisup", "FutureParam", function(x) {
  TRUE
})

#' @importFrom methods setReplaceMethod validObject
setReplaceMethod("bplog", c("FutureParam", "logical"), function(x, value) {
  x$log <- value
  validObject(x)
  x
})

#' @importFrom methods setReplaceMethod
setReplaceMethod("bpthreshold", c("FutureParam", "character"), function(x, value) {
  x$threshold <- value
  x
})

#' @importFrom methods setMethod
setMethod("bplogdir", "FutureParam", function(x) {
  x$logdir
})

#' @importFrom methods setReplaceMethod
setReplaceMethod("bplogdir", c("FutureParam", "character"), function(x, value) {
  .valid.SnowParam.log <- importBP(".valid.SnowParam.log")

  if (!length(value)) {
    value <- NA_character_
  }

  x$logdir <- value
  if (is.null(msg <- .valid.SnowParam.log(x))) {
    x
  } else {
    stop(msg)
  }
})

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Methods - evaluation
###

#' @importFrom methods setMethod
#' @importFrom BiocParallel bplog bpok bpparam bpstopOnError bpthreshold bptimeout
#' @importFrom future future resolve values
setMethod("bplapply", c("ANY", "FutureParam"), function(X, FUN, ..., BPREDO=list(), BPPARAM=bpparam()) {
  .composeTry <- importBP(".composeTry")
  .error_bplist <- importBP(".error_bplist")
  .log_load <- importBP(".log_load")
  .redo_index <- importBP(".redo_index")

  if (!length(X)) return(list())

  FUN <- match.fun(FUN)

  idx <- .redo_index(X, BPREDO)
  if (any(idx)) {
    X <- X[idx]
  }

  .log_load(bplog(BPPARAM), bpthreshold(BPPARAM))

  FUN <- .composeTry(FUN, bplog(BPPARAM), bpstopOnError(BPPARAM),
                     stop.immediate=bpstopOnError(BPPARAM),
                     timeout=bptimeout(BPPARAM))

  ## Create futures
  fs <- list()
  for (kk in seq_along(X)) {
    X_kk <- X[[kk]]
    fs[[kk]] <- future(FUN(X_kk, ...))
  }
  names(fs) <- names(X)

  ## Resolve futures
  fs <- resolve(fs, value=TRUE)

  ## Retrieve values
  res <- values(fs, signal=FALSE)

  if (any(idx)) {
    BPREDO[idx] <- res
    res <- BPREDO
  }

  if (!all(bpok(res))) {
    stop(.error_bplist(res))
  }

  res
})



#' @importFrom methods setMethod
#' @importFrom BiocParallel bploop bplog bptimeout bpstopOnError
#' @importFrom future future resolve values
setMethod("bpiterate", c("ANY", "ANY", "FutureParam"), function(ITER, FUN, ..., REDUCE, init, reduce.in.order=FALSE, BPPARAM=bpparam()) {
  .composeTry <- importBP(".composeTry")

  ITER <- match.fun(ITER)
  FUN <- match.fun(FUN)
  hasREDUCE <- !missing(REDUCE)
  hasinit <- !missing(init)

  if (!hasREDUCE) {
    if (reduce.in.order) {
      stop("Argument 'REDUCE' must be provided when 'reduce.in.order = TRUE'")
    }
    if (hasinit) {
      stop("Argument 'REDUCE' must be provided when 'init' is given")
    }
  }

  FUN <- .composeTry(FUN, bplog(BPPARAM), bpstopOnError(BPPARAM),
                     timeout=bptimeout(BPPARAM))
  ARGFUN <- function(value) c(list(value), list(...))


  ## Create futures
  fs <- list()
  ii <- 1L

  if (hasinit) {
    fs[[ii]] <- init
    ii <- ii + 1L
  }

  repeat {
    item <- ITER()
    if (is.null(item)) break
    fs[[ii]] <- future(FUN(item, ...))
    ii <- ii + 1L
  }

  ## Resolve futures
  fs <- resolve(fs, value=TRUE)

  ## Retrieve values
  res <- values(fs, signal=FALSE)

  if (hasREDUCE && length(res) > 1) {
    res <- Reduce(REDUCE, res)
  }

  res
})


importBP <- function(name, mode="function", inherits=FALSE) {
  ns <- getNamespace("BiocParallel")
  get(name, mode=mode, envir=ns, inherits=inherits)
}
