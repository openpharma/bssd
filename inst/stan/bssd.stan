// This is a model for binary event probabilities in a blinded two-arm RCT,
// where we observe a bernoulli event and time-at-risk (>0) for each patient.
// The model assumes that the hazard rate within a trial, lambda, is constant
// through time.
// I.e. let t be the follow-up time and pi be the probability of the event.
// Then, pi = 1 - exp(-lambda * t).
// We assume finite beta mixture priors on the event rate in each arm.
// The likelihood is also effectively a mixture distribution since treatment
// allocation remains secret.

data {
  int<lower=1> N;                              // Number of patients
  array[N] int<lower=0, upper=1> y;            // Binary outcomes in patients 1..N
  array[N] real<lower=0> time;                 // Time-at-risk in patients 1..N
  real<lower=0, upper=1> prob_arm1;            // Prob being allocated to arm 1

  int<lower=1> K1;                             // Number of components in prior on pi1
  simplex[K1] w1;                              // Weights of components in prior on pi1
  array[K1] real<lower=0> pi1_alpha;           // Shape params 1 for beta priors on pi1
  array[K1] real<lower=0> pi1_beta;            // Shape params 2 for beta priors on pi1

  int<lower=1> K2;                             // Number of components in prior on pi2
  simplex[K2] w2;                              // Weights of components in prior on pi2
  array[K2] real<lower=0> pi2_alpha;           // Shape params 1 for beta priors on pi2
  array[K2] real<lower=0> pi2_beta;            // Shape params 2 for beta priors on pi2
}

parameters {
  vector[2] mu;                                // Location parameters
}

transformed parameters {
  real<lower=0, upper=1> pi1;                  // Prob(event) in unit time in arm 1
  real<lower=0, upper=1> pi2;                  // Prob(event) in unit time in arm 2

  pi1 = inv_cloglog(mu[1]);
  pi2 = inv_cloglog(mu[2]);
}

model {
  vector[2] log_theta;

  // Priors
  vector[K1] lps1 = log(w1);
  vector[K2] lps2 = log(w2);

  for (k in 1:K1) {
    lps1[k] += beta_lpdf(pi1 | pi1_alpha[k], pi1_beta[k]);
  }
  target += log_sum_exp(lps1);

  for (k in 1:K2) {
    lps2[k] += beta_lpdf(pi2 | pi2_alpha[k], pi2_beta[k]);
  }
  target += log_sum_exp(lps2);

  // Adjust by the log-abs-determinant of the Jacobian after change of variable
  target += mu[1] + mu[2] - exp(mu[1]) - exp(mu[2]);

  // Fit the mixture likelihood
  log_theta[1] = log(prob_arm1);
  log_theta[2] = log(1 - prob_arm1);

  for (n in 1:N) {
    vector[2] lps = log_theta;

    for (k in 1:2) {
      lps[k] += binomial_lpmf(
        y[n] | 1,
        inv_cloglog(mu[k] + log(time[n]))
      );
    }

    target += log_sum_exp(lps);
  }
}