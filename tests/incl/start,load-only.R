## Record original state
ovars <- ls()
oopts <- options(warn = 1L, mc.cores = 2L, future.debug = FALSE)
oopts$future.delete <- getOption("future.delete")
oplan <- future::plan()

## Use sequential futures by default
future::plan(sequential)

## Use FutureParam with BiocParallel by default
BiocParallel::register(BiocParallel.FutureParam::FutureParam())

## To please R CMD check when using require().
future.batchtools <- "future.batchtools"  #nolint

fullTest <- (Sys.getenv("_R_CHECK_FULL_") != "")

all_strategies <- function(excl = "multiprocess") {
  strategies <- Sys.getenv("R_FUTURE_TESTS_STRATEGIES")
  strategies <- unlist(strsplit(strategies, split = ","))
  strategies <- gsub(" ", "", strategies)
  strategies <- strategies[nzchar(strategies)]
  
  builtin <- future:::supportedStrategies()
  if (require(future.batchtools, character.only = TRUE)) {
    builtin <- c(builtin, "batchtools_local", "batchtools_interactive")
  }
  
  strategies <- c(builtin, strategies)
  strategies <- unique(strategies)
  strategies <- setdiff(strategies, excl)
  strategies
}

test_strategy <- function(strategy) {
  strategy %in% all_strategies()
}

hpaste <- future:::hpaste

attach_locally <- function(x, envir = parent.frame()) {
  for (name in names(x)) {
    assign(name, value = x[[name]], envir = envir)
  }
}
