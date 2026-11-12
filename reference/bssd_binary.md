# Creator for class `bssd_binary`.

The object in element `fit` is an object of type `stanfit` from the
rstan package. It contains samples for variables `pi1` and `pi2`. These
are the probabilities of at least one event in a single unit of time in
arms one and two.

## Usage

``` r
bssd_binary(y, time, fit)
```

## Arguments

- y:

  instance of class `integer`. y integer vector, 0 for no event, 1 for
  event, for all patients.

- time:

  instance of class `numeric`. Time numeric vector, strictly positive,
  time covariate.

- fit:

  instance of class `stanfit`.

## Value

A list of class `bssd_binary` (also inheriting from `bssd`), containing
the observed event indicators `y`, follow-up times `time`, and the
fitted Stan model `fit`.

## See also

[`stan_bssd()`](stan_bssd.md) for examples.
