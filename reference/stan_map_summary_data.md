# Fit a Stan MAP prior generation model to summary-level data.

Fit a Stan MAP prior generation model to summary-level data.

## Usage

``` r
stan_map_summary_data(y, n, time, a_alpha, a_beta, b_alpha, b_beta, ...)
```

## Arguments

- y:

  integer vector, number of events in the trials.

- n:

  integer vector, number of patients in the trials.

- time, :

  numeric vector, strictly positive, time at risk in the trials.

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
p\_{i})) = \alpha\_{i} + log(T\_{i}) \$\$ where \\p\_{i}\\ is the
probability of event in trial i, adjusted for the intended or average
time-at-risk, \\T\_{i}\\. The model is equivalent to: \$\$
log(\lambda\_{i}) = \alpha\_{i} \$\$ where \$\$ \lambda\_{i} = -log(1 -
p\_{i}) / T\_{i} \$\$ is the hazard of the event, assumed constant
through time, and \\p\_{i}\\ is the probability of event in unit time in
trial i. Thus this is a random-effects model that assumes the log-hazard
terms are exchangeable across trials, and the \\ \alpha\_{i} \\ terms
are trial-specific intercepts. To complete the meta-analytical model, we
apply the hierarchical distribution \$\$ p\_{j} \sim Beta(abN, (1-a)bN)
\$\$ where N is the total number of patients in the historical trials,
and a and b are hyperparameters. To illustrate the role of a and b,
consider that the effective sample size of this distribution is \\abN +
(1-a)bN) = bN\\, i.e. b is a discount to the historical data, taking
values on (0, 1). Furthermore, the mean of this distribution is \\ abN /
bN = a\\. Thus, a is the expected probability of event in unit time, and
once again a is bound to the range (0, 1). Hyperpriors \\ a \sim
Beta(a\_{\alpha}, a\_{\beta}) \\ and \\ b \sim Beta(b\_{\alpha},
b\_{\beta}) \\ are natural choices to enforce these ranges. This method
creates a prior from summary-level data, requiring only the intended or
average times-at-risk and total counts of patients with an adverse event
in each trial. It is likely aggregate statistics like this can be
derived from publications and/or filings. However, if you have
patient-level times-at-risk and adverse event statuses,
[`stan_map_patient_data`](stan_map_patient_data.md) provides a similar
method to create a prior from patient-level data. The patient-level
approach is generally preferable because it allows adjustment for how
long each individual patient spent at risk.

This model is effectively the same as that fit by
[`stan_map_patient_data`](stan_map_patient_data.md) with the notable
exception that since patient-level times-at-risk are not known, the
intended trial follow-up times, \\ T\_{j} \\, are used instead.

Note: setting a seed in R does not affect the state of the random
generator in stan. If a reproducible output is desired, then the
optional parameter `seed` needs to be populated. For additional
information about this parameter, see
[`sampling`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html).

## Examples

``` r
set.seed(2021)
H <- 10
time <- 0.5 + cumsum(rexp(H, 5))
# Use constant hazard model:
hazard <- 0.15
prob <- 1 - exp(- hazard * time)
n <- rpois(H, lambda = 100 * time)
y <- rbinom(H, size = n, prob = prob)
x <- stan_map_summary_data(y = y, n = n, time = time,
                           a_alpha = 1 / 3, a_beta = 1 / 3,
                           b_alpha = 5, b_beta = 5)
#> 
#> SAMPLING FOR MODEL 'map_prior_summary_data' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 2e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.2 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.06 seconds (Warm-up)
#> Chain 1:                0.064 seconds (Sampling)
#> Chain 1:                0.124 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'map_prior_summary_data' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 1.2e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.12 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 0.056 seconds (Warm-up)
#> Chain 2:                0.065 seconds (Sampling)
#> Chain 2:                0.121 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'map_prior_summary_data' NOW (CHAIN 3).
#> Chain 3: 
#> Chain 3: Gradient evaluation took 1.1e-05 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.11 seconds.
#> Chain 3: Adjust your expectations accordingly!
#> Chain 3: 
#> Chain 3: 
#> Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 3: 
#> Chain 3:  Elapsed Time: 0.059 seconds (Warm-up)
#> Chain 3:                0.062 seconds (Sampling)
#> Chain 3:                0.121 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'map_prior_summary_data' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 1e-05 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.1 seconds.
#> Chain 4: Adjust your expectations accordingly!
#> Chain 4: 
#> Chain 4: 
#> Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 4: 
#> Chain 4:  Elapsed Time: 0.055 seconds (Warm-up)
#> Chain 4:                0.049 seconds (Sampling)
#> Chain 4:                0.104 seconds (Total)
#> Chain 4: 
x
#> Prob(Event): Mean 0.148 (95% HPDI 0.129, 0.168)
#> MAP Prob(Event): Mean 0.148 (95% HPDI 0.113, 0.183)
```
