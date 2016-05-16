library("BiocParallel.FutureParam")
register(FutureParam())
plan(lazy)

mu <- 1.0
sigma <- 2.0
x <- bplapply(1:3, mu=mu, sigma=sigma, function(i, mu, sigma) {
  rnorm(i, mean=mu, sd=sigma)
})
print(x)
