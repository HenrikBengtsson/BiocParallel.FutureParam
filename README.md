# BiocParallel.FutureParam: A BiocParallelParam class using Futures

## Introduction
The [future] package provides a generic API for using futures in R.
A future is a simple yet powerful mechanism to evaluate an R expression
and retrieve its value at some point in time.  Futures can be resolved
in many different ways depending on which strategy is used.
There are various types of synchronous and asynchronous futures to
choose from in the [future] package.
Additional futures are implemented in other packages.
For instance, the [future.BatchJobs] package provides futures for
_any_ type of backend that the [BatchJobs] package supports.
For an introduction to futures in R, please consult the
vignettes of the [future] package.

The [BiocParallel.FutureParam] package provides FutureParam, a BiocParallelParam class, for the [BiocParallel] package that works with _any_ type of future.
The BiocParallel.FutureParam package is cross platform just as the future package.

Below is an example showing how to use FutureParam with
_multiprocess_ futures.  A multiprocess future will be evaluated in
parallel using forked processes.  If process forking is not supported
by the operating system, then multiple background R sessions will
instead be used to resolve the futures.

```r
library("BiocParallel.FutureParam")
register(FutureParam())
plan(multiprocess)

mu <- 1.0
sigma <- 2.0
x <- bplapply(1:3, mu=mu, sigma=sigma, function(i, mu, sigma) {
  rnorm(i, mean=mu, sd=sigma)
})
```


## FutureParam replaces existing BiocParallelParam classes

Due to the generic nature of futures, the FutureParam class
provides the same functionality as many of the existing
BiocParallelParam classes, e.g. SerialParam, SnowParam,
MulticoreParam, BatchJobsParam and DoParParam.

<table style="width: 100%;">
<tr>
<th>BiocParallel usage</th><th>BiocParallel.FutureParam alternative</th>
</tr>

<tr style="vertical-align: center;">
<td>
<pre><code class="r">library("BiocParallel")
register(SerialParam())

</code></pre>
</td>
<td>
<pre><code class="r">library("BiocParallel.FutureParam")
register(FutureParam())
plan(eager)
</code></pre>
</td>
</tr>

<tr style="vertical-align: center;">
<td>
<pre><code class="r">library("BiocParallel")
register(MulticoreParam())

</code></pre>
</td>
<td>
<pre><code class="r">library("BiocParallel.FutureParam")
register(FutureParam())
plan(multicore)
</code></pre>
</td>
</tr>

<tr style="vertical-align: center;">
<td>
<pre><code class="r">library("BiocParallel")
register(SnowParam(2, "SOCK"))

</code></pre>
</td>
<td>
<pre><code class="r">library("BiocParallel.FutureParam")
register(FutureParam())
plan(multisession, workers=2)
</code></pre>
</td>
</tr>

<tr style="vertical-align: center;">
<td>
<pre><code class="r">library("BiocParallel")
cl <- parallel::makeCluster(2, "SOCK")
register(as(cl, "SnowParam"))

</code></pre>
</td>
<td>
<pre><code class="r">library("BiocParallel.FutureParam")
register(FutureParam())
cl <- parallel::makeCluster(2, "SOCK")
plan(cluster, workers=cl)
</code></pre>
</td>
</tr>


<tr style="vertical-align: center;">
<td>
<pre><code class="r">library("BiocParallel")
register(SnowParam(4, "MPI"))


</code></pre>
</td>
<td>
<pre><code class="r">library("BiocParallel.FutureParam")
register(FutureParam())
cl <- parallel::makeCluster(4, type="MPI")
plan(cluster, workers=cl)
</code></pre>
</td>
</tr>


<tr style="vertical-align: center;">
<td>
<pre><code class="r">library("BiocParallel")
library("BatchJobs")
funs <- makeClusterFunctionsSLURM("~/slurm.tmpl")
register(BatchJobsParam(4, cluster.functions=funs))

</code></pre>
</td>
<td>
<pre><code class="r">library("BiocParallel.FutureParam")
register(FutureParam())
library(future.BatchJobs)
plan(batchjobs, backend="~/.BatchJobs.R")
</code></pre>
</td>
</tr>

<table>


[BatchJobs]: https://cran.r-project.org/package=BatchJobs
[BiocParallel]: https://bioconductor.org/packages/release/bioc/html/BiocParallel.html
[BiocParallel.FutureParam]: https://github.com/HenrikBengtsson/BiocParallel.FutureParam
[future]: https://cran.r-project.org/package=future
[future.BatchJobs]: https://github.com/HenrikBengtsson/future.BatchJobs

## Installation
R package BiocParallel.FutureParam is only available via [GitHub](https://github.com/HenrikBengtsson/BiocParallel.FutureParam) and can be installed in R as:
```r
source('http://callr.org/install#HenrikBengtsson/BiocParallel.FutureParam')
```

### Pre-release version
To install the pre-release version that is available in branch `release/0.1.0`, use:
```r
source('http://callr.org/install#HenrikBengtsson/BiocParallel.FutureParam@release/0.1.0')
```
This will install the package from source.  



## Software status

| Resource:     | GitHub        | Travis CI     | Appveyor         |
| ------------- | ------------------- | ------------- | ---------------- |
| _Platforms:_  | _Multiple_          | _Linux_       | _Windows_        |
| R CMD check   |  | <a href="https://travis-ci.org/HenrikBengtsson/BiocParallel.FutureParam"><img src="https://travis-ci.org/HenrikBengtsson/BiocParallel.FutureParam.svg" alt="Build status"></a> | <a href="https://ci.appveyor.com/project/HenrikBengtsson/biocparallel-futureparam"><img src="https://ci.appveyor.com/api/projects/status/github/HenrikBengtsson/BiocParallel.FutureParam?svg=true" alt="Build status"></a> |
| Test coverage |                     | <a href="https://coveralls.io/r/HenrikBengtsson/BiocParallel.FutureParam"><img src="https://coveralls.io/repos/HenrikBengtsson/BiocParallel.FutureParam/badge.svg?branch=develop" alt="Coverage Status"/></a>   |                  |
