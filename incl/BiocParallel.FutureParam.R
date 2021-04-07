library("BiocParallel.FutureParam")
register(FutureParam())
plan(multisession)

mu <- 1.0
sigma <- 2.0
x <- bplapply(1:3, mu = mu, sigma = sigma, function(i, mu, sigma) {
  rnorm(i, mean = mu, sd = sigma)
})
print(x)

## WORKAROUND: For some reason, 'R CMD check' on Windows will give an
## error when running this example with plan(multisession), unless we
## reset the future plan at the end.
plan(sequential)
