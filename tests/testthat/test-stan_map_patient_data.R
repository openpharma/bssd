
# Source Data -----------------------------------------------------
  # Params
  set.seed(2021)
  N <- 100
  num_trials <- 5
  time <- 0.5 + cumsum(rexp(N, 100))
  # User constant hazard model
  hazard <- 0.15
  prob <- 1 - exp(- hazard * time)
  y <- rbinom(N, size = 1, prob = prob)
  trial <- sample(seq_len(num_trials), size = N, replace = TRUE)

# Correct data y -----------------------------------------------------------
test_that("stan_map_patient_data breaks when y data is incorrect.", {

  # y different from 0 or 1
  y2 <- y
  y2[2] <- 1.5

  expect_error(
    stan_map_patient_data(
      y = y2,
      trial = trial,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), "y should be a vector of 0s and 1s only, and cannot be missing."
  )

  # y contains NAs
  y2 <- y
  y2[2] <- NA

  expect_error(
    stan_map_patient_data(
      y = y2,
      trial = trial,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), "y should be a vector of 0s and 1s only, and cannot be missing."
  )
})

# Correct data trial -----------------------------------------------------------
test_that("stan_map_patient_data breaks when trial data is incorrect.", {
  # trial contains NA
  trial2 <- trial
  trial2[2] <- NA

  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial2,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), "Each element of trial should be non-missing."
  )

  # trial contains 0, but trial should only host integer values that are
  # strictly positive.
  trial2 <- trial
  trial2[2] <- as.integer(0)

  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial2,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), "Each element of trial should be a positive integer."
  )

  # trial has a negative value, it should be a value >0 and integer
  trial2 <- trial
  trial2[2] <- as.integer(-1)

  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial2,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), "Each element of trial should be a positive integer."
  )

  # trial has a non integer
  trial2 <- trial
  trial2[2] <- 0.5

  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial2,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), "Each element of trial should be a positive integer."
  )

})

# Correct data time -----------------------------------------------------------
test_that("stan_map_patient_data breaks when time data is incorrect.", {
    # Time contains NAs
    time2 <- time
    time2[2] <- NA

    expect_error(
      stan_map_patient_data(
        y = y,
        trial = trial,
        time = time2,
        a_alpha = 1 / 3,
        a_beta = 1 / 3,
        b_alpha = 5,
        b_beta = 5,
        refresh = 0
      ), "Each element of time should be strictly positive and non-missing."
    )

    # time contains zero
    time2 <- time
    time2[2] <- 0

    expect_error(
      stan_map_patient_data(
        y = y,
        trial = trial,
        time = time2,
        a_alpha = 1 / 3,
        a_beta = 1 / 3,
        b_alpha = 5,
        b_beta = 5,
        refresh = 0
      ), "Each element of time should be strictly positive and non-missing."
    )

    # time contains a negative value
    time2 <- time
    time2[2] <- -8

    expect_error(
      stan_map_patient_data(
        y = y,
        trial = trial,
        time = time2,
        a_alpha = 1 / 3,
        a_beta = 1 / 3,
        b_alpha = 5,
        b_beta = 5,
        refresh = 0
      ), "Each element of time should be strictly positive and non-missing."
    )
})

# Correct data hyperparams --------------------------------------------------
test_that("stan_map_patient_data breaks when params data is incorrect.", {

  # zeros
  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial,
      time = time,
      a_alpha = 0,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be strictly ",
              "positive and non-missing.")
  )

  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 0,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 0,
      b_beta = 5,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 0,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  # Negative values
  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial,
      time = time,
      a_alpha = -0.1,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 0,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial,
      time = time,
      a_alpha = 1 / 3,
      a_beta = -0.1,
      b_alpha = 5,
      b_beta = 0,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = -0.1,
      b_beta = 0,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = -0.1,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  # NAs
  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial,
      time = time,
      a_alpha = NA,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial,
      time = time,
      a_alpha = 1 / 3,
      a_beta = NA,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = NA,
      b_beta = 5,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = NA,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  # NULL
  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial,
      time = time,
      a_alpha = NULL,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial,
      time = time,
      a_alpha = 1 / 3,
      a_beta = NULL,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = NULL,
      b_beta = 5,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = NULL,
      refresh = 0
    ), paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
              "strictly positive and non-missing.")
  )

})


# Mismatched lengths -----------------------------------------------------
test_that(paste0("stan_map_patient_data breaks when ",
                 "data contain mismatching lengths."), {
  # y  length
  expect_error(
    stan_map_patient_data(
      y = y[-1],
      trial = trial,
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), "y must be the same length as trial and time."
  )

  # trial length
  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial[-1],
      time = time,
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), "trial must be the same length as y and time."
  )

  # time length
  expect_error(
    stan_map_patient_data(
      y = y,
      trial = trial,
      time = time[-1],
      a_alpha = 1 / 3,
      a_beta = 1 / 3,
      b_alpha = 5,
      b_beta = 5,
      refresh = 0
    ), "time must be the same length as y and trial."
  )
})

# Theoretical params vs MAP fitting at different sample sizes ------------------
test_that("stan_map_patient_data with high sample size.", {

  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")

  # Map Fit
  set.seed(2021)
  N <- 5000
  time <- 0.5 + cumsum(rexp(N, 100))
  hazard <- 0.15
  unit_prob <- 1 - exp(- hazard) # true prob
  prob <- 1 - exp(- hazard * time)
  y <- rbinom(N, size = 1, prob = prob)
  trial <- sample(1:5, size = N, replace = TRUE)
  x <- stan_map_patient_data(
    y = y,
    trial = trial,
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

  #Estimated medians for pi_h should be close to unit_prob
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

test_that("stan_map_patient_data with low sample size.", {

  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")

  # Map Fit
  set.seed(2021)
  N <- 1000
  time <- 0.5 + cumsum(rexp(N, 100))
  hazard <- 0.15
  unit_prob <- 1 - exp(- hazard)
  prob <- 1 - exp(- hazard * time)
  y <- rbinom(N, size = 1, prob = prob)
  trial <- sample(1:5, size = N, replace = TRUE)
  x <- stan_map_patient_data(
    y = y,
    trial = trial,
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
  # Notably, 2 of these CIs include unit_prob but exclude hazard, so we have
  # demonstrated that the model is estimating probability and not hazard.

})
