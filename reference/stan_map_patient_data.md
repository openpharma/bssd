# Fit a Stan MAP prior generation model to historical patient-level data.

Fit a Stan MAP prior generation model to historical patient-level data.

## Usage

``` r
stan_map_patient_data(y, trial, time, a_alpha, a_beta, b_alpha, b_beta, ...)
```

## Arguments

- y:

  integer vector, 0 for no event, 1 for event, for all patients.

- trial:

  integer vector, denoting which trials patients belong to.

- time:

  numeric vector, strictly positive, times at risk for patients.

- a_alpha:

  numeric, first shape param of beta prior on a. See Details.

- a_beta:

  numeric, second shape param of beta prior on a. See Details.

- b_alpha:

  numeric, first shape param of beta prior on b. See Details.

- b_beta:

  numeric, second shape param of beta prior on b. See Details.

- ...:

  extra parameters are passed to
  [`sampling`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html).

## Value

object of type [`bssd_binary_map`](bssd_binary_map.md).

## Details

This routine fits the meta-analytical model equation: \$\$ log(-log(1 -
p\_{ij})) = \alpha\_{j} + log(T\_{ij}) \$\$ where \\p\_{ij}\\ is the
probability of event in trial j, adjusted for the time-at-risk of
patient i, \\T\_{ij}\\. The model is equivalent to: \$\$
log(\lambda\_{ij}) = \alpha\_{j} \$\$ where \$\$ \lambda\_{ij} =
-log(1 - p\_{j}) / T\_{ij} \$\$ is the hazard of the event, assumed
constant through time, and \\p\_{j}\\ is the probability of event in
unit time in trial j. Thus this is a random-effects model that assumes
the log-hazard terms are exchangeable across trials, and the \\
\alpha\_{j} \\ terms are trial-specific intercepts. To complete the
meta-analytical model, we apply the hierarchical distribution \$\$
p\_{j} \sim Beta(abN, (1-a)bN)\$\$ where N is the total number of
patients in the historical trials, and a and b are hyperparameters. To
illustrate the role of a and b, consider that the effective sample size
of this distribution is \\abN + (1-a)bN) = bN\\, i.e. b is a discount to
the historical data, taking values on (0, 1). Furthermore, the mean of
this distribution is \\ abN / bN = a\\. Thus, a is the expected
probability of event in unit time, and once again a is bound to the
range (0, 1). Hyperpriors \\ a \sim Beta(a\_{\alpha}, a\_{\beta}) \\ and
\\ b \sim Beta(b\_{\alpha}, b\_{\beta}) \\ are natural choices to
enforce these ranges. If you do not have patient-level times-at-risk and
adverse event statuses,
[`stan_map_summary_data`](stan_map_summary_data.md) provides a similar
method to create a prior from summary-level data, requiring only the
intended or average times-at-risk and total counts of patients with the
event in each trial. It is likely aggregate statistics like this can be
derived from publications and/or filings. The patient-level approach is
generally preferable, however, because it allows adjustment for how long
each individual patient spent at risk.

Note: setting a seed in R does not affect the state of the random
generator in stan. If a reproducible output is desired, then the
optional parameter `seed` needs to be populated. For additional
information about this parameter, see
[`sampling`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html).

## Examples

``` r
# Use a short MCMC run for this illustrative example.
check_iter <- 500
check_chains <- 1
set.seed(2021)
N <- 100
time <- 0.5 + cumsum(rexp(N, 100))
# Use constant hazard model:
hazard <- 0.15
prob <- 1 - exp(- hazard * time)
y <- rbinom(N, size = 1, prob = prob)
trial <- sample(1:5, size = N, replace = TRUE)
x <- stan_map_patient_data(y = y, trial = trial, time = time,
                           a_alpha = 1 / 3, a_beta = 1 / 3,
                           b_alpha = 5, b_beta = 5,
                           iter = check_iter, chains = check_chains,
                           refresh = 0)
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
x
#> Prob(Event): Mean 0.117 (95% HPDI 0.05, 0.188)
#> MAP Prob(Event): Mean 0.123 (95% HPDI 0.02, 0.245)
```
