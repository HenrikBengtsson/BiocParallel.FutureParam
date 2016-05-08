# BiocParallel.FutureParam
R package: BiocParallel.FutureParam - a BiocParallelParam class using Futures

_This is currently only a place holder repository for a package that hopefully will eventually implemented!_

R package BiocParallel.FutureParam implements class FutureParam, which is a [future] backend for the [BiocParallel] package.  The FutureParam class extends the BiocParallelParam of BiocParallel.  This makes it possible to use any type of futures together with BiocParallel.  For example,
```r
library('BiocParallel.FutureParam')
register(FutureParam(), default=TRUE)
plan(multiprocess)

mu <- 1.0
sigma <- 2.0
bplapply(1:3, mu=mu, sigma=sigma, function(i, mu, sigma) {
  rnorm(i, mean=mu, sd=sigma)
})
```

_This is currently only a place holder repository for a package that hopefully will eventually implemented!_

[future]: https://cran.r-project.org/package=future
[BiocParallel]: https://bioconductor.org/packages/release/bioc/html/BiocParallel.html
