// This is an hierarchical model for binary event probabilities in exchangeable
// trials, where we observe binomial event counts and intended time-at-risk (>0)
// for each trial (i.e. summary-level or trial-level data).
// The model assumes that the hazard rate within a trial, lambda, is constant
// through time.
// I.e. for follow-up time t, prob(event) = 1 - exp(-lambda * t).
// The log hazard is assumed exchangeable across trials. I.e. for trial h:
// log(lambda_h) = alpha_h
// Let pi_h be prob(event) in unit time (i.e. t=1). We impose the hierarchical
// distribution pi_h ~ beta(abN, (1-a)bN).
// Using standard properties of beta distributions:
// E[pi_h] = a, and ESS = abN + (1-a)bN = bN.
// Thus, a is the prior expected event rate in unit time, and b is a discount to
// the total number of patients, N.
// Finally, we impose beta priors on a & b.

data {
  // Data on trials
  int<lower=0> H;                            // Number of historic trials
  array[H] int<lower=0> y;                  // Numbers of events in trials 1..H
  array[H] int<lower=0> non_events;         // Numbers of non-events in trials 1..H
  vector<lower=0>[H] time;                   // Time-at-risk covariates for trials 1..H

  // Hyperparameters
  real<lower=0> a_alpha;                     // 1st shape param of beta prior on a
  real<lower=0> a_beta;                      // 2nd shape param of beta prior on a
  real<lower=0> b_alpha;                     // 1st shape param of beta prior on b
  real<lower=0> b_beta;                      // 2nd shape param of beta prior on b
}

transformed data {
  array[H] int<lower=0> n;                  // Number of patients in trials 1..H
  int<lower=0> N;                            // Total number of patients in all trials

  for (i in 1:H) {
    n[i] = y[i] + non_events[i];
  }

  N = sum(n);
}

parameters {
  real<lower=0, upper=1> a;                 // Prior expected value of pi_h
  real<lower=0, upper=1> b;                 // Proportion of historic data to borrow
  vector<lower=0, upper=1>[H] pi_h;         // Prob(event) in unit time in trials 1..H
  real<lower=0, upper=1> pi_star;           // Prob(event) in unit time in new trial
}

transformed parameters {
  vector<lower=0, upper=1>[H] p;            // Prob(event) in trial time in trials 1..H
  real<lower=0> pi_star_alpha;              // 1st shape param of beta dist on pi_star
  real<lower=0> pi_star_beta;               // 2nd shape param of beta dist on pi_star
  real<lower=0, upper=1> avg;               // Mean of beta dist on pi_star
  real<lower=0> ess;                        // ESS of beta dist on pi_star

  p = inv_cloglog(log(-log(1 - pi_h)) + log(time));
  pi_star_alpha = a * b * N;
  pi_star_beta = (1 - a) * b * N;
  avg = pi_star_alpha / (pi_star_alpha + pi_star_beta);
  ess = pi_star_alpha + pi_star_beta;
}

model {
  pi_h ~ beta(a * b * N, (1 - a) * b * N);
  a ~ beta(a_alpha, a_beta);
  b ~ beta(b_alpha, b_beta);
  y ~ binomial(n, p);

  // MAP distribution
  pi_star ~ beta(pi_star_alpha, pi_star_beta);
}