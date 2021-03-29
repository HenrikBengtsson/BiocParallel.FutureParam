

<div id="badges"><!-- pkgdown markup -->

<a href="https://github.com/HenrikBengtsson/BiocParallel.FutureParam/actions?query=workflow%3AR-CMD-check"><img border="0" src="https://github.com/HenrikBengtsson/BiocParallel.FutureParam/workflows/R-CMD-check/badge.svg?branch=develop" alt="Build status"></a></a>
<a href="https://travis-ci.org/HenrikBengtsson/BiocParallel.FutureParam"><img border="0" src="https://travis-ci.org/HenrikBengtsson/BiocParallel.FutureParam.svg" alt="Build status"></a></a>
<a href="https://ci.appveyor.com/project/HenrikBengtsson/biocparallel-futureparam"><img border="0" src="https://ci.appveyor.com/api/projects/status/github/HenrikBengtsson/BiocParallel.FutureParam?svg=true" alt="Build status"></a></a>
<a href="https://codecov.io/gh/HenrikBengtsson/BiocParallel.FutureParam"><img border="0" src="https://codecov.io/gh/HenrikBengtsson/BiocParallel.FutureParam/branch/develop/graph/badge.svg" alt="Coverage Status"></a></a>
<a href="https://lifecycle.r-lib.org/articles/stages.html"><img border="0" src="man/figures/lifecycle-experimental-orange.svg" alt="Life cycle: experimental"></a></a>
</div>

# BiocParallel.FutureParam: Use Futures with BiocParallel 

## Introduction
The [future] package provides a generic API for using futures in R.
A future is a simple yet powerful mechanism to evaluate an R expression
and retrieve its value at some point in time.  Futures can be resolved
in many different ways depending on which strategy is used.
There are various types of synchronous and asynchronous futures to
choose from in the [future] package.
Additional futures are implemented in other packages.
For instance, the [future.batchtools] package provides futures for
_any_ type of backend that the [batchtools] package supports.
For an introduction to futures in R, please consult the
vignettes of the [future] package.

The [BiocParallel.FutureParam] package provides FutureParam, a BiocParallelParam class, for the [BiocParallel] package that works with _any_ type of future.
The BiocParallel.FutureParam package is cross platform just as the future package.

Below is an example showing how to use FutureParam with
_multicore_ futures.  A multicore future will be evaluated in
parallel using forked workers, which is not supported on MS Windows
when it will fall back to sequential processing.

```r
library("BiocParallel.FutureParam")
register(FutureParam())
plan(multicore)

mu <- 1.0
sigma <- 2.0
x <- bplapply(1:3, mu = mu, sigma = sigma, function(i, mu, sigma) {
  rnorm(i, mean = mu, sd = sigma)
})
```


## FutureParam replaces existing BiocParallelParam classes

Due to the generic nature of futures, the FutureParam class
provides the same functionality as many of the existing
BiocParallelParam classes, e.g. SerialParam, SnowParam,
MulticoreParam, BatchtoolsParam and DoParParam.  In addition,
it provides supports for additional backends that are not yet
implemented in [BiocParallel], e.g. [callr] and [batchtools].

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
plan(sequential)
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
register(SnowParam(2, type = "SOCK"))

</code></pre>
</td>
<td>
<pre><code class="r">library("BiocParallel.FutureParam")
register(FutureParam())
plan(multisession, workers = 2)
</code></pre>
</td>
</tr>

<tr style="vertical-align: center;">
<td>
<pre><code class="r">library("BiocParallel")
cl <- parallel::makeCluster(2, type = "SOCK")
register(as(cl, "SnowParam"))

</code></pre>
</td>
<td>
<pre><code class="r">library("BiocParallel.FutureParam")
register(FutureParam())
cl <- parallel::makeCluster(2, type = "SOCK")
plan(cluster, workers = cl)
</code></pre>
</td>
</tr>


<tr style="vertical-align: center;">
<td>
<pre><code class="r">library("BiocParallel")
register(SnowParam(4, type = "MPI"))


</code></pre>
</td>
<td>
<pre><code class="r">library("BiocParallel.FutureParam")
register(FutureParam())
cl <- parallel::makeCluster(4, type = "MPI")
plan(cluster, workers = cl)
</code></pre>
</td>
</tr>


<tr style="vertical-align: center;">
<td>
<pre><code class="r">library("BiocParallel")
register(BatchtoolsParam(cluster="sge",
                         template="~/sge.tmpl"))
</code></pre>
</td>
<td>
<pre><code class="r">library("BiocParallel.FutureParam")
register(FutureParam())
plan(future.batchtools::batchtools_sge,
     template = "~/sge.tmpl")
</code></pre>
</td>
</tr>

<tr style="vertical-align: center;">
<td>
N/A
</td>
<td>
<pre><code class="r">library("BiocParallel.FutureParam")
register(FutureParam())
plan(future.callr::callr)
</code></pre>
</td>
</tr>

<table>


[batchtools]: https://cran.r-project.org/package=batchtools
[callr]: https://cran.r-project.org/package=callr
[BiocParallel]: https://bioconductor.org/packages/release/bioc/html/BiocParallel.html
[BiocParallel.FutureParam]: https://github.com/HenrikBengtsson/BiocParallel.FutureParam
[future]: https://cran.r-project.org/package=future
[future.batchtools]: https://cran.r-project.org/package=future.batchtools
[future.callr]: https://cran.r-project.org/package=future.callr

## Installation
R package BiocParallel.FutureParam is only available via [GitHub](https://github.com/HenrikBengtsson/BiocParallel.FutureParam) and can be installed in R as:
```r
remotes::install_github("HenrikBengtsson/BiocParallel.FutureParam", ref="master")
```


### Pre-release version

To install the pre-release version that is available in Git branch `develop` on GitHub, use:
```r
remotes::install_github("HenrikBengtsson/BiocParallel.FutureParam", ref="develop")
```
This will install the package from source.  


<!-- pkgdown-drop-below -->

## Contributing

To contribute to this package, please see [CONTRIBUTING.md](CONTRIBUTING.md).
