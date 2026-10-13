
library(BssdSimulation)

# Historic data ----
set.seed(2021)
H <- 10
time <- 0.5 + cumsum(rexp(H, 5))
# Use constant hazard model:
hazard <- 0.15
prob <- 1 - exp(- hazard * time)
n <- rpois(H, lambda = 100 * time)
y <- rbinom(H, size = n, prob = prob)


# Priors ----

# Stan MAP prior
stan_map_prior <- stan_map_summary_data(y = y, n = n, time = time,
                                        a_alpha = 1 / 3, a_beta = 1 / 3,
                                        b_alpha = 5, b_beta = 5,
                                        refresh = 0)
stan_map_prior
summarise_bssd_map(stan_map_prior)
stan_map_mix <- fit_mixture(stan_map_prior)
stan_map_mix

# Chi-squ method prior
chisqu_prior <- chi_squ_prior(y = y, n = n, time = time)
chisqu_prior
summarise_beta_dist(chisqu_prior$alpha, chisqu_prior$beta)

# Current data ----
prob_arm1 <- 0.333
N <- 100
c_time <- 0.5 + cumsum(rexp(N, 100))
tmt <- sample(1:2, size = N, replace = TRUE,
              prob = c(prob_arm1, 1 - prob_arm1))
hazard1 <- 0.15
hazard2 <- 0.25
prob <- numeric(length = N)
prob[tmt == 1] <- 1 - exp(- hazard1 * c_time[tmt == 1])
prob[tmt == 2] <- 1 - exp(- hazard2 * c_time[tmt == 2])
c_y <- rbinom(N, size = 1, prob = prob)


# BSSD ----

# Stan BSSD - Stan MAP prior - blinded
w1 <- stan_map_mix["w",]
pi1_alpha <- stan_map_mix["a",]
pi1_beta <- stan_map_mix["b",]
w2 <- as.array(c(1))
pi2_alpha <- as.array(c(0.3))
pi2_beta <- as.array(c(0.3))
stan_bssd_b <- stan_bssd(
  y = c_y, time = c_time, prob_arm1 = prob_arm1,
  w1 = w1, pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
  w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta,
  refresh = 0
)
stan_bssd_b

# Stan BSSD - Stan MAP prior - unblinded
stan_bssd_u <- stan_bssd(
  y = c_y, time = c_time, tmt = tmt,
  w1 = w1, pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
  w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta,
  refresh = 0
)
stan_bssd_u

# Stan BSSD - chi-squ prior - blinded
stan_bssd_cs_b <- stan_bssd(
  y = c_y, time = c_time, prob_arm1 = prob_arm1,
  w1 = 1, pi1_alpha = chisqu_prior$alpha, pi1_beta = chisqu_prior$beta,
  w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta,
  refresh = 0
)
stan_bssd_cs_b

# Stan BSSD - Stan MAP prior - unblinded
stan_bssd_cs_u <- stan_bssd(
  y = c_y, time = c_time, tmt = tmt,
  w1 = 1, pi1_alpha = chisqu_prior$alpha, pi1_beta = chisqu_prior$beta,
  w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta,
  refresh = 0
)
stan_bssd_cs_u

# Blinded analyses side-by-side
stan_bssd_b
stan_bssd_cs_b

# Unblinded analyses side-by-side
stan_bssd_u
stan_bssd_cs_u

library(ggplot2)
library(patchwork)
p_b <- (plot(stan_bssd_b) + xlim(0, 0.5) + labs(title = "Blinded data",
                                         subtitle = "MAP prior")) /
  (plot(stan_bssd_cs_b) + xlim(0, 0.5) + labs(subtitle = "Chi-squ prior"))
p_u <- (plot(stan_bssd_u) + xlim(0, 0.5) + labs(title = "Unblinded data",
                                         subtitle = "MAP prior")) /
  (plot(stan_bssd_cs_u) + xlim(0, 0.5) + labs(subtitle = "Chi-squ prior"))

pbm <- plot(stan_bssd_b) + xlim(0, 0.5) + ylim(0, 40) +
  labs(title = "Blinded data", subtitle = "MAP prior") +
  theme(legend.position = "none")
pbc <- plot(stan_bssd_cs_b) + xlim(0, 0.5) + ylim(0, 40) +
  labs(subtitle = "Chi-squ prior") +
  theme(legend.position = "none")
pum <- plot(stan_bssd_u) + xlim(0, 0.5) + ylim(0, 40) +
  labs(title = "Unblinded data", subtitle = "MAP prior") +
  theme(legend.position = "none")
puc <- plot(stan_bssd_cs_u) + xlim(0, 0.5) + ylim(0, 40) +
  labs(subtitle = "Chi-squ prior") +
  theme(legend.position = "none")
(pbm + pum) / (pbc + puc) # Interesting

# Which cutoff? ----
stan_bssd_b$fit %>%
  as.data.frame() %>%
  as_tibble()
stan_bssd_u$fit %>%
  as.data.frame() %>%
  as_tibble()

# How many excess events in next unit of time?

# Need to model how many people will be recruited

t_fwd <- 0.5
n_fwd <- 50

stan_bssd_b
stan_bssd_b$fit %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(
    diff = pi2 - pi1,
    pi1_fwd = 1 - exp(-pi1 * t_fwd),
    pi2_fwd = 1 - exp(-pi2 * t_fwd),
    r1_fwd = rbinom(n = nrow(.), size = n_fwd, prob = pi1_fwd),
    r2_fwd = rbinom(n = nrow(.), size = n_fwd, prob = pi2_fwd),
    r_diff = r2_fwd - r1_fwd
  ) %>%
  ggplot(aes(x = r_diff)) +
  geom_density()
  # apply(2, mean)




# Stan BSSD - Chi-squ prior - blinded
# TODO

# Stan BSSD - Uniform prior - blinded
stan_bssd(y = c_y, time = c_time, prob_arm1 = prob_arm1,
          w1 = as.array(1), pi1_alpha = as.array(1), pi1_beta = as.array(1),
          w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta,
          refresh = 0)
# Always many divergences

# Approximate BSSD - Stan MAP prior - blinded
approx_bssd_b <- bssd(y = sum(c_y), time = sum(c_time),
                      prob_arm1 = prob_arm1,
                      pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
                      pi2_alpha = pi2_alpha, pi2_beta = pi2_beta)
approx_bssd_b



# Scratch
prior = chisqu_prior
bind_rows(
  tibble(Parameter = "a", Mean = prior$alpha / (prior$alpha + prior$beta),
         SD = 0)
)
