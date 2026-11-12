# Print posterior summaries from the meta-analytic-predictive BSSD model.

Print posterior summaries from the meta-analytic-predictive BSSD model.

## Usage

``` r
# S3 method for class 'bssd_binary_map'
print(x, mcmc = FALSE, alpha = 0.05, ...)
```

## Arguments

- x:

  A `bssd_binary_map` object.

- mcmc:

  If `TRUE`, also print the fitted Stan model.

- alpha:

  Complement of the interval mass; 0.05 gives a 95% interval.

- ...:

  Unused.

## Value

No return value, called for side effects. The method prints the
posterior mean event probability and the meta-analytic-predictive event
probability for a new exchangeable trial, together with their highest
posterior density intervals. If `mcmc = TRUE`, the underlying Stan fit
is also printed.
