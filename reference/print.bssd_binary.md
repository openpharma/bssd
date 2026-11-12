# Print a BSSD model summary.

Print a BSSD model summary.

## Usage

``` r
# S3 method for class 'bssd_binary'
print(x, mcmc = FALSE, alpha = 0.05, ...)
```

## Arguments

- x:

  A `bssd_binary` object.

- mcmc:

  If `TRUE`, also print the fitted Stan model.

- alpha:

  Complement of the interval mass; 0.05 gives a 95% interval.

- ...:

  Unused.

## Value

No return value, called for side effects. The method prints the
posterior mean event probabilities in both arms and the posterior
probability that the event rate in arm 2 is greater than that in arm 1,
together with their highest posterior density intervals. If
`mcmc = TRUE`, the underlying Stan fit is also printed.
