// This is an hierarchical model for binary event probabilities in exchangeable
// trials, where we observe a bernoulli event and time-at-risk (>0) for each
// patient in each trial (i.e. patient-level data).
// The model assumes that the hazard rate within a trial, lambda, is constant
// through time.
// I.e. let t be the follow-up time and pi be the probability of the event.
// Then, pi = 1 - exp(-lambda * t).
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
  // Data on patients and trials
  int<lower=0> N;                              // Total number of patients in all trials
  int<lower=0> H;                              // Number of historic trials
  array[N] int<lower=1, upper=H> trial;        // 1..H indicators of trial membership
                                                // in patients 1..N
  array[N] int<lower=0, upper=1> y;            // 0..1 event indicators in patients 1..N
  vector<lower=0>[N] time;                     // Time-at-risk in patients 1..N

  // Hyperparameters
  real<lower=0> a_alpha;                       // 1st shape param of beta prior on a
  real<lower=0> a_beta;                        // 2nd shape param of beta prior on a
  real<lower=0> b_alpha;                       // 1st shape param of beta prior on b
  real<lower=0> b_beta;                        // 2nd shape param of beta prior on b
}

parameters {
  real<lower=0, upper=1> a;                    // Prior expected value of pi_h
  real<lower=0, upper=1> b;                    // Proportion of historic data to borrow
  vector<lower=0, upper=1>[H] pi_h;            // Prob(event) in unit time in trials 1..H
  real<lower=0, upper=1> pi_star;              // Prob(event) in unit time in new trial
}

transformed parameters {
  real<lower=0> pi_star_alpha;                 // 1st shape param of beta dist on pi_star
  real<lower=0> pi_star_beta;                  // 2nd shape param of beta dist on pi_star
  real<lower=0, upper=1> avg;                  // Mean of beta dist on pi_star
  real<lower=0> ess;                           // ESS of beta dist on pi_star

  pi_star_alpha = a * b * N;
  pi_star_beta = (1 - a) * b * N;
  avg = pi_star_alpha / (pi_star_alpha + pi_star_beta);
  ess = pi_star_alpha + pi_star_beta;
}

model {
  real thisprob;

  pi_h ~ beta(a * b * N, (1 - a) * b * N);
  a ~ beta(a_alpha, a_beta);
  b ~ beta(b_alpha, b_beta);

  for (i in 1:N) {
    thisprob =
      inv_cloglog(log(-log(1 - pi_h[trial[i]])) + log(time[i]));
    y[i] ~ binomial(1, thisprob);
  }

  // MAP distribution
  pi_star ~ beta(pi_star_alpha, pi_star_beta);
}
