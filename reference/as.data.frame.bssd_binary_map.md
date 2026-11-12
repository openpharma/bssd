# Extract posterior draws from a meta-analytic predictive BSSD model.

Extract posterior draws from a meta-analytic predictive BSSD model.

## Usage

``` r
# S3 method for class 'bssd_binary_map'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A `bssd_binary_map` object.

- row.names:

  Unused.

- optional:

  Unused.

- ...:

  The optional `pars` argument selects Stan parameters to extract; other
  arguments are unused.

## Value

A data frame containing posterior draws from the Stan
meta-analytic-predictive model. By default, it contains the historical
trial event probabilities (`pi_h`), model parameters `a` and `b`, the
effective sample size (`ess`), mean event probability (`avg`), and the
predictive event probability for a new exchangeable trial (`pi_star`).
Alternative Stan parameters can be selected using `pars` in `...`.
