# Creator for class `bssd_binary_map`.

The object in slot `fit` is an object of type `stanfit` from the rstan
package. The `pi_star` variable contains MCMC samples of the probability
of at least one event in a single unit of time, suitable to form the
prior for a new trial that is exchangeable with the historical trials.
The `ess` variable contains samples for the effective sample size of the
distribution of pi_star. `avg` contains samples for the mean of pi_star.
The `pi_h[i]` variables contains samples for the probability of at least
one event in a single unit of time for historical trials 1, 2...

## Usage

``` r
bssd_binary_map(fit)
```

## Arguments

- fit:

  instance of class `stanfit`.

## Value

list-like object with type `bssd_binary_map`.

## See also

[`stan_map_summary_data()`](stan_map_summary_data.md) and
[`stan_map_patient_data()`](stan_map_patient_data.md) for examples.
