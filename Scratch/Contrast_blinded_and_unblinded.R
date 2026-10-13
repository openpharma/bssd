
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
names(x)
print(x)
summary(x)
as.data.frame(x)
plot(x)
forest_plot(x)

# Use prior in blinded BSSD analysis
prob_arm1 <- 0.333
N <- 100
# time <- 0.5 + cumsum(rexp(N, 100))
time <- rep(1, N)
tmt <- sample(1:2, size = N, replace = TRUE,
              prob = c(prob_arm1, 1 - prob_arm1))
hazard1 <- 0.15
hazard2 <- 0.25
# hazard2 <- hazard1 * 3
prob <- numeric(length = N)
prob[tmt == 1] <- 1 - exp(- hazard1 * time[tmt == 1])
prob[tmt == 2] <- 1 - exp(- hazard2 * time[tmt == 2])
y <- rbinom(N, size = 1, prob = prob)
mix <- fit_mixture(x)
w1 <- mix["w",]
pi1_alpha <- mix["a",]
pi1_beta <- mix["b",]

library(dplyr)

# The unknowable truth
(true_prob_ae_1 <- 1 - exp(- hazard1))
(true_prob_ae_2 <- 1 - exp(- hazard2))

# The unknowable truth (data)
tibble(y, tmt, time) %>%
  summarise(n(), sum(y), mean(y), mean(time))
tibble(y, tmt, time) %>%
  group_by(tmt) %>%
  summarise(n(), sum(y), mean(y), mean(time))

# Prior
(prior_prob_ae_ctl <- mix[2] / (mix[2] + mix[3]))



fit1 <- stan_bssd(y = y, time = time, prob_arm1 = prob_arm1,
                  w1 = w1, pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
                  w2 = 1, pi2_alpha = 0.3, pi2_beta = 0.3)
fit1
# Prob(1) centred on prior.
# Prob(2) = centred on logic:
(sum(y) - prob_arm1 * N * prior_prob_ae_ctl) / (N * (1 - prob_arm1))

# Use the same prior in an unblinded BSSD analysis
fit2 <- stan_bssd(y = y, time = time, tmt = tmt,
                  w1 = w1, pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
                  w2 = 1, pi2_alpha = 0.3, pi2_beta = 0.3)
fit2

# Remove prior
fit3 <- stan_bssd(y = y, time = time, tmt = tmt,
                  w1 = w1, pi1_alpha = 1, pi1_beta = 1,
                  w2 = 1, pi2_alpha = 0.3, pi2_beta = 0.3)
fit3

library(ggplot2)
p1 <- plot(fit1) + xlim(0, 0.5)
p2 <- plot(fit2) + xlim(0, 0.5)
p3 <- plot(fit3) + xlim(0, 0.5)
library(patchwork)
p1/p2/p3


names(fit1)
print(fit1)
summary(fit1)
as.data.frame(fit1)
plot(fit1)
forest_plot(fit1) # Breaks
