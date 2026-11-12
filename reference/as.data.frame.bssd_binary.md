# Extract posterior draws from a BSSD model.

Extract posterior draws from a BSSD model.

## Usage

``` r
# S3 method for class 'bssd_binary'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  A `bssd_binary` object.

- row.names:

  Unused.

- optional:

  Unused.

- ...:

  The optional `pars` argument selects Stan parameters to extract; other
  arguments are unused.

## Value

A data frame containing posterior draws from the Stan model. By default,
the data frame contains `pi1` and `pi2`, representing the probabilities
of at least one event in a single unit of time in arms 1 and 2,
respectively. Alternative Stan parameters can be selected using `pars`
in `...`.
