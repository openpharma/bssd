
.rs.restartR()

library(bssd)
library(dplyr)

# Patient-level MAP model ----
set.seed(2021)
N <- 100
trial <- sample(1:5, size = N, replace = TRUE)
time <- 0.4 * trial + rexp(N, 20) # Later trials were longer
hazard <- 0.15
prob <- 1 - exp(- hazard * time)
y <- rbinom(N, size = 1, prob = prob)
hist_df <- tibble(trial, y, time, prob)
hist_df %>%
  group_by(trial) %>%
  summarise(
    n = n(),
    events = sum(y),
    event_rate = mean(y),
    mean_tar = mean(time),
    mean_prob = mean(prob),
    .groups = "drop"
  )
x <- stan_map_patient_data(
  y = y,
  trial = trial,
  time = time,
  a_alpha = 1 / 3,
  a_beta = 1 / 3,
  b_alpha = 5,
  b_beta = 5
)
names(x)
x$fit
forest_plot(x)
x$alpha %>% hist
x$beta %>% hist
x$mean %>% hist
x$ess %>% hist
mix <- fit_mixture(x)
mix
plot(mix)$mix

# Summary-level MAP model ----
set.seed(2021)
H <- 10
time <- 0.5 + cumsum(rexp(H, 5))
# Use constant hazard model:
hazard <- 0.15
prob <- 1 - exp(- hazard * time)
n <- rpois(H, lambda = 100 * time)
y <- rbinom(H, size = n, prob = prob)
x <- stan_map_summary_data(
  y = y,
  n = n,
  time = time,
  a_alpha = 1 / 3,
  a_beta = 1 / 3,
  b_alpha = 5,
  b_beta = 5
)
x$fit
as.data.frame(x$fit) %>% head
x$alpha %>% hist
x$beta %>% hist
x$mean %>% hist
x$ess %>% hist
x$pi_star %>% hist
mix <- fit_mixture(x)
mix
plot(mix)$mix






# BSSD model ----
set.seed(2021)
prob_arm1 <- 0.333
N <- 100
time <- 0.5 + cumsum(rexp(N, 100))
arm <- sample(1:2, size = N, replace = TRUE, prob = c(prob_arm1, 1 - prob_arm1))
hazard1 <- 0.15
hazard2 <- 0.25
prob <- numeric(length = N)
prob[arm == 1] <- 1 - exp(- hazard1 * time[arm == 1])
prob[arm == 2] <- 1 - exp(- hazard2 * time[arm == 2])
y <- rbinom(N, size = 1, prob = prob)
summary_df <- tibble(arm, y, time, prob) %>%
  group_by(arm) %>%
  summarise(
    n = n(),
    events = sum(y),
    event_rate = mean(y),
    mean_tar = mean(time),
    mean_prob = mean(prob),
    .groups = "drop")
summary_df
mix <- fit_mixture(x)
w1 <- mix["w",]
# w1 <- c(0.9, 0.1)
pi1_alpha <- mix["a",]
# dat$pi1_alpha <- c(38.64074, 1)
pi1_beta <- mix["b",]
# dat$pi1_beta <- c(436.3206, 1)
w2 <- as.array(c(1))
# dat$w2 <- as.array(c(1))
pi2_alpha <- as.array(c(0.3))
# dat$pi2_alpha <- as.array(c(0.3))
pi2_beta <- as.array(c(0.3))
# dat$pi2_beta <- as.array(c(0.3))
fit <- stan_bssd(y = y, time = time, prob_arm1 = prob_arm1,
                 w1 = w1, pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
                 w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta)
names(fit)
x <- fit
fit
print(fit, mcmc = TRUE)
summary(fit)
as.data.frame(fit) %>% head
plot(fit)

as.data.frame(fit$fit) %>%
  summarise(
    n = n(),
    mean(pi1),
    sd(pi1),
    mean(pi2),
    sd(pi2),
    mean(pi2 > pi1)
  )
# c.f.
summary_df
