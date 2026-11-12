# Plot posterior event probabilities from a BSSD model.

Plot posterior event probabilities from a BSSD model.

## Usage

``` r
# S3 method for class 'bssd_binary'
plot(x, ...)
```

## Arguments

- x:

  A `bssd_binary` object.

- ...:

  Unused.

## Value

A `ggplot` object showing the posterior density distributions of `pi1`
and `pi2`, the probabilities of at least one event in a single unit of
time in arms 1 and 2, respectively.
