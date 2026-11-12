# Plot meta-analytic predictive event probabilities.

Plot meta-analytic predictive event probabilities.

## Usage

``` r
# S3 method for class 'bssd_binary_map'
plot(x, ...)
```

## Arguments

- x:

  A `bssd_binary_map` object.

- ...:

  Unused.

## Value

A `ggplot` object showing the posterior density distributions of the
mean event probability (`avg`) and the meta-analytic-predictive event
probability for a new exchangeable trial (`pi_star`).
