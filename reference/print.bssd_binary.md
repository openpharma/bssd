# Print estimated mean event rate in each arm, and the posterior probability that the true event rate is greater in the active arm.

Print estimated mean event rate in each arm, and the posterior
probability that the true event rate is greater in the active arm.

## Usage

``` r
# S3 method for class 'bssd_binary'
print(x, mcmc = FALSE, alpha = 0.05, ...)
```

## Arguments

- x:

  stanfit object.

- mcmc:

  MCMC flag.

- alpha:

  if not null, 1 - alpha confidence interval is calculated.

- ...:

  extra parameters are passed to [`stan_bssd`](stan_bssd.md).
