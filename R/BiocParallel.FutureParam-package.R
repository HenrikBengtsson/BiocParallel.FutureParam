#' BiocParallel.FutureParam: A BiocParallelParam class using Futures
#'
#' The \pkg{BiocParallel.FutureParam} package provides [FutureParam],
#' a [BiocParallel::BiocParallelParam] class, for the
#' \pkg{BiocParallel} package that works with \emph{any} type of future.
#' (that is supported by Future API of the \pkg{future} package) can
#' be used for asynchronous (parallel/distributed) or synchronous
#' (sequential) processing.
#'
#' Futures themselves are provided by the \pkg{future} package, e.g.
#' multicore, multisession, ad hoc cluster, and MPI cluster futures.
#' Additional futures are provided by other packages.
#' For example, batchtools futures are implemented by the
#' \pkg{future.batchtools} package, which expands the support for
#' asynchronous processing to anything that the \pkg{batchtools}
#' package supports.
#'
#' To use futures with the \pkg{BiocParallel} package, load
#' \pkg{BiocParallel.FutureParam}, call `register(FutureParam())`,
#' select the type of future you wish to use via
#' [future:plan()].
#'
#' @example incl/BiocParallel.FutureParam.R
#'
#' @docType package
#' @name BiocParallel.FutureParam
NULL
