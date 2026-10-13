library(bssd)
get_example_bssd_fit <- function() {
  # Data
  set.seed(2021)
  H <- 10
  time <- 0.5 + cumsum(rexp(H, 5))
  hazard <- 0.15
  prob <- 1 - exp(- hazard * time)
  n <- rpois(H, lambda = 100 * time)
  y <- rbinom(H, size = n, prob = prob)
  # MAP prior
  x <- stan_map_summary_data(y = y, n = n, time = time,
                             a_alpha = 1 / 3, a_beta = 1 / 3,
                             b_alpha = 5, b_beta = 5)

  # Blinded BSSD analysis
  prob_arm1 <- 0.333
  N <- 100
  time <- rep(1, N)
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
  fit1 <- stan_bssd(y = y, time = time, prob_arm1 = prob_arm1,
                    w1 = w1, pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
                    w2 = 1, pi2_alpha = 0.3, pi2_beta = 0.3)
  return(fit1)
}
