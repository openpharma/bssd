test_that("stan_map_summary_data breaks when it should.", {

# Initial Params ----------------------------------------------------------

  y <- c(1, 0, 5)
  n <- c(10, 5, 55)
  time <- c(1, 0.5, 1.2)

# Correct data ----------------------------------------------------------

  # y negative values
  y2 <- c(-1, 0, 5)

  expect_error(
    stan_map_summary_data(
      y = y2,
      n = n,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), "y must be non-negative and non-missing."
  )

  # y with NAs
  y2 <- c(NA, 0, 5)

  expect_error(
    stan_map_summary_data(
      y = y2,
      n = n,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), "y must be non-negative and non-missing."
  )

  # n with zero
  n2 <- c(0, 5, 55)

  expect_error(
    stan_map_summary_data(
      y = y,
      n = n2,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), "Each element of n must be at least 1 and non-missing."
  )

  # n with NAs
  n2 <- c(NA, 5, 55)

  expect_error(
    stan_map_summary_data(
      y = y,
      n = n2,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), "Each element of n must be at least 1 and non-missing."
  )

  # time negative
  time2 <- c(-1, 0.5, 1.2)

  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time2,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), "Each element of time should be strictly positive and non-missing."
  )

  # time contain zero
  time2 <- c(0, 0.5, 1.2)

  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time2,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), "Each element of time should be strictly positive and non-missing."
  )

  # time containing NA
  time2 <- c(NA, 0.5, 1.2)

  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time2,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), "Each element of time should be strictly positive and non-missing."
  )

  # a_alpha zero
  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time,
      a_alpha = 0,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  # a_alpha negative
  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time,
      a_alpha = -0.1,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  # a_alpha missing
  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time,
      a_alpha = NA,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  # a_alpha is null
  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time,
      a_alpha = NULL,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  # a_beta zero
  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 0,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  # a_beta negative
  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time,
      a_alpha = 1 / 3,
      a_beta = -0.1,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  # a_beta NA
  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time,
      a_alpha = 1 / 3,
      a_beta = NA,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  # a_beta Null
  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time,
      a_alpha = 1 / 3,
      a_beta = NULL,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  # b_alpha zero
  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 0,
      b_beta = 5,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  # b_alpha negative
  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = -0.1,
      b_beta = 5,
      refresh = 0
    )
  )

  # b_alpha NA
  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = NA,
      b_beta = 5,
      refresh = 0
    )
  )

  # b_alpha Null
  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = NULL,
      b_beta = 5,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  # b_beta zero
  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 0,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  # b_beta negative
  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = -0.1,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  # b_beta NA
  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = NA,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  # b_beta Null
  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = NULL,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

# Mismatched lengths ------------------------------------------------------

  ## n lengt
  expect_error(
    stan_map_summary_data(
      y = y,
      n = n[-1],
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), "n must be the same length as y and time."
  )

  ## time length
  expect_error(
    stan_map_summary_data(
      y = y,
      n = n,
      time = time[-1],
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), "time must be the same length as y and n."
  )

  ## y length
  expect_error(
    stan_map_summary_data(
      y = y[-1],
      n = n,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), "y must be the same length as n and time."
  )

  # individual values of y must be always < n{
  y2 <- y
  y2[2] <- n[2] + 1

  expect_error(
    stan_map_summary_data(
      y = y2,
      n = n,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), paste0("Each element of y must equal to or less than ",
              "the corresponding element in n.")
  )


 })

# Theoritical params vs MAP fitting at different sample size -------------------

test_that("stan_map_summary_data with low sample size", {

  # Map Fit
  set.seed(2021)
  H <- 10
  time <- 0.5 + cumsum(rexp(H, 5))
  hazard <- 0.15
  unit_prob <- 1 - exp(- hazard)
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
    b_beta = 5,
    refresh = 0
  )

  # Estimated means for pi_h should be close to unit_prob
  epsilon <- unlist(
              lapply(as.data.frame(x, pars = "pi_h"),
                     function(x) {
                       3 * sd(x)
                     }))

  pi_h <- as.data.frame(x, pars = "pi_h") %>% colMeans()
  expect_true(all(abs(pi_h - unit_prob) < epsilon))

  # Estimated medians for pi_h should be close to unit_prob
  pi_h <- as.data.frame(x, pars = "pi_h") %>% apply(2, median)
  expect_true(all(abs(pi_h - unit_prob) < epsilon))

  # 99% CIs for pi_h should contain unit_prob
  pi_h_ci <- as.data.frame(x, pars = "pi_h") %>%
    apply(2, quantile, probs = c(0.005, 0.995)) %>%
    t() %>%
    as.data.frame()
  colnames(pi_h_ci) <- c("lower", "upper")
  all_covered <- pi_h_ci %>%
    dplyr::mutate(covered = lower < unit_prob & upper > unit_prob) %>%
    dplyr::summarise(all_covered = all(covered)) %>%
    dplyr::pull(all_covered)
  expect_true(all_covered)

})

test_that("stan_map_summary_data with high sample size.", {

  # Map Fit
  set.seed(2021)
  H <- 10
  time <- 0.5 + cumsum(rexp(H, 5))
  hazard <- 0.15
  unit_prob <- 1 - exp(- hazard)
  prob <- 1 - exp(- hazard * time)
  n <- rpois(H, lambda = 10000 * time)
  y <- rbinom(H, size = n, prob = prob)
  x <- stan_map_summary_data(
    y = y,
    n = n,
    time = time,
    a_alpha = 1 / 3,
    a_beta = 1 / 3,
    b_alpha = 5,
    b_beta = 5,
    refresh = 0
  )

  # Estimated means for pi_h should be close to unit_prob
  epsilon <- unlist(
    lapply(as.data.frame(x, pars = "pi_h"),
           function(x) {
             3 * sd(x)
           }))

  pi_h <- as.data.frame(x, pars = "pi_h") %>% colMeans()
  expect_true(all(abs(pi_h - unit_prob) < epsilon))

  # Estimated medians for pi_h should be close to unit_prob
  pi_h <- as.data.frame(x, pars = "pi_h") %>% apply(2, median)
  expect_true(all(abs(pi_h - unit_prob) < epsilon))

  # 99% CIs for pi_h should contain unit_prob
  pi_h_ci <- as.data.frame(x, pars = "pi_h") %>%
    apply(2, quantile, probs = c(0.005, 0.995)) %>%
    t() %>%
    as.data.frame()
  colnames(pi_h_ci) <- c("lower", "upper")
  all_covered <- pi_h_ci %>%
    dplyr::mutate(covered = lower < unit_prob & upper > unit_prob) %>%
    dplyr::summarise(all_covered = all(covered)) %>%
    dplyr::pull(all_covered)
  expect_true(all_covered)
  # Notably, these CIs include unit_prob but exclude hazard, so we have
  # demonstrated that the model is estimating probability and not hazard.

})
