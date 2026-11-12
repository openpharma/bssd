# Summary statistics of a BSSD model.

Summary statistics of a BSSD model.

## Usage

``` r
# S3 method for class 'bssd_binary'
summary(object, alpha = 0.05, ...)
```

## Arguments

- object:

  A `bssd_binary` object.

- alpha:

  Complement of the interval mass; 0.05 gives a 95% interval.

- ...:

  Unused.

## Value

A tibble with three rows of summary statistics. The `variable` column
identifies the event probabilities in arms 1 and 2 (`pi1` and `pi2`) and
the probability that the event rate in arm 2 is greater than that in arm
1 (`prob_higher`). The `mean`, `median`, and `sd` columns contain the
corresponding estimates, while `.lower` and `.upper` give the lower and
upper bounds of the highest posterior density interval. The `alpha`
column gives the complement of the interval mass.
