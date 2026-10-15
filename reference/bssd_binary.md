# Creator for class `bssd_binary`.

The object in slot `fit` is an object of type `stanfit` from the rstan
package. It contains samples for variables `pi1` and `pi2`. These are
the probabilities of at least one event in a single unit of time in arms
one and two.

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

object of type `bssd_binary`.

## See also

[`stan_bssd()`](stan_bssd.md) for examples.
