# Fit a Bayesian safety signal detection analysis for a two-arm RCT using Stan.

If you know the treatment allocations, provide those as a vector of 1s
and 2s via the `tmt` argument. If you do not know the treatment
allocations (i.e. the trial remains blinded), specify the `prob_arm1`
argument. Exactly one of `tmt` and `prob_arm1` must be specified. Note:
setting a seed in R does not affect the state of the random generator in
stan. If a reproducible output is desired, then the optional parameter
`seed` needs to be populated. For additional information about this
parameter, see
[`sampling`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html).

## Usage

``` r
stan_bssd(
  y,
  time,
  tmt = NULL,
  prob_arm1 = NULL,
  w1 = c(1),
  pi1_alpha = c(1),
  pi1_beta = c(1),
  w2 = c(1),
  pi2_alpha = c(1),
  pi2_beta = c(1),
  ...
)
```

## Arguments

- y:

  integer vector, 0 for no event, 1 for event, for all patients.

- time:

  numeric vector, strictly positive, time covariate.

- tmt:

  integer vector, 1 for placebo (arm1), 2 for active (arm2), for all
  patients. Specify this for an unblinded analysis (i.e. `prob_arm1` is
  NULL).

- prob_arm1:

  numeric, probability of being randomized to placebo (arm1). Specify
  this for a blinded analysis (i.e. `tmt` is NULL).

- w1:

  numeric vector, weights of constituent beta distributions in mixture
  prior on unit-time event probability in placebo (arm1). Should sum to
  1.

- pi1_alpha:

  numeric vector, first shape params of constituent beta distributions
  in mixture prior on unit-time event probability in placebo (arm1).

- pi1_beta:

  numeric vector, second shape params of constituent beta distributions
  in mixture prior on unit-time event probability in placebo (arm1).

- w2:

  numeric vector, weights of constituent beta distributions in mixture
  prior on unit-time event probability in active (arm2). Should sum to
  1.

- pi2_alpha:

  numeric vector, first shape params of constituent beta distributions
  in mixture prior on unit-time event probability in active (arm2).

- pi2_beta:

  numeric vector, second shape params of constituent beta distributions
  in mixture prior on unit-time event probability in active (arm2).

- ...:

  extra parameters are passed to
  [`sampling`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html).

## Value

object of type [`bssd_binary`](bssd_binary.md).

## Examples

``` r
# Use a short MCMC run for this illustrative example.
check_iter <- 500
check_chains <- 1
# Create prior
set.seed(2021)
H <- 10
time_0 <- 0.5 + cumsum(rexp(H, 5))
# Use constant hazard model:
hazard <- 0.15
prob_0 <- 1 - exp(- hazard * time_0)
n <- rpois(H, lambda = 100 * time_0)
y <- rbinom(H, size = n, prob = prob_0)
x <- stan_map_summary_data(y = y, n = n, time = time_0,
                           a_alpha = 1 / 3, a_beta = 1 / 3,
                           b_alpha = 5, b_beta = 5,
                           iter = check_iter, chains = check_chains,
                           refresh = 0)
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess

# Use prior in blinded BSSD analysis
prob_arm1 <- 0.333
N <- 100
time <- 0.5 + cumsum(rexp(N, 100))
tmt <- sample(1:2, size = N, replace = TRUE,
              prob = c(prob_arm1, 1 - prob_arm1))
hazard1 <- 0.15
hazard2 <- 0.25
prob <- numeric(length = N)
prob[tmt == 1] <- 1 - exp(- hazard1 * time[tmt == 1])
prob[tmt == 2] <- 1 - exp(- hazard2 * time[tmt == 2])
y <- rbinom(N, size = 1, prob = prob)
mix <- fit_mixture(x)
w1 <- mix["w",]
pi1_alpha <- mix["a",]
pi1_beta <- mix["b",]
w2 <- as.array(c(1))
pi2_alpha <- as.array(c(0.3))
pi2_beta <- as.array(c(0.3))
fit1 <- stan_bssd(y = y, time = time, prob_arm1 = prob_arm1,
                  w1 = w1, pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
                  w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta,
                  iter = check_iter, chains = check_chains, refresh = 0)
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
fit1
#> Prob(Event) in arm 1: 0.15 (95% HPDI 0.123, 0.182)
#> Prob(Event) in arm 2: 0.232 (95% HPDI 0.136, 0.388)
#> Prob(Event rate in arm 2 > arm 1): 0.908 (95% HPDI 0.892, 0.924)

# Use the same prior in an unblinded BSSD analysis
fit2 <- stan_bssd(y = y, time = time, tmt = tmt,
                  w1 = w1, pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
                  w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta,
                  iter = check_iter, chains = check_chains, refresh = 0)
fit2
#> Prob(Event) in arm 1: 0.145 (95% HPDI 0.108, 0.176)
#> Prob(Event) in arm 2: 0.251 (95% HPDI 0.146, 0.362)
#> Prob(Event rate in arm 2 > arm 1): 0.974 (95% HPDI 0.96, 0.984)
```
