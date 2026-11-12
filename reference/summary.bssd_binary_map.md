# Summary statistics of a meta-analytic predictive BSSD model.

Summary statistics of a meta-analytic predictive BSSD model.

## Usage

``` r
# S3 method for class 'bssd_binary_map'
summary(object, alpha = 0.05, ...)
```

## Arguments

- object:

  A `bssd_binary_map` object.

- alpha:

  Complement of the interval mass; 0.05 gives a 95% interval.

- ...:

  Unused.

## Value

A tibble with two rows of posterior summary statistics. The `variable`
column identifies the mean event probability (`mean`) and the
meta-analytic-predictive event probability for a new exchangeable trial
(`pi_star`). The `mean`, `median`, and `sd` columns summarize the
corresponding posterior draws. The `.lower` and `.upper` columns give
the lower and upper bounds of the highest posterior density interval,
and `alpha` gives the complement of the interval mass.
