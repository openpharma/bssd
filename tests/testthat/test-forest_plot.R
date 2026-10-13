test_that("forest_plot input and output is as expected.", {

  # create sample data for testing:
  # Historic patient-level data is made up of 150 patients (N) across
  # 3 trials (trial). By assuming a constant hazard model (hazard)
  # we calculate the probability of adverse event in a trial (prob)
  # given time at risk (time). We fit a Stan MAP generation model to this
  # synthetic patient level data to obtain MAP priors.

  set.seed(2024)
  alpha <- 0.05
  N <- 150
  time <- 0.5 + cumsum(rexp(N, 150))
  hazard <- 0.2
  prob <- 1 - exp(- hazard * time)
  y <- rbinom(N, size = 1, prob = prob)
  trial <- sample(1:3, size = N, replace = TRUE)
  x <- stan_map_patient_data(y = y, trial = trial, time = time,
                             a_alpha = 1 / 3, a_beta = 1 / 3,
                             b_alpha = 5, b_beta = 5,
                             seed = 2024) # declare seed to obtain
                                          # reproducible result when
                                          # calling a stan function

  # expect input is of class 'bssd_binary_map_pl', 'bssd_binary_map', 'bssd_map'
  expect_s3_class(x, c("bssd_binary_map_pl", "bssd_binary_map", "bssd_map"))

  # expect fit object is of class 'stanfit'
  expect_s4_class(x$fit, "stanfit")

  # obtain forest plot of input data
  forest_plot <- forest_plot(x, alpha = alpha)
  forest_plot

  # use layer_data to extract mean and quantile data from the plot
  # this is our observed data
  observed_data <- ggplot2::layer_data(forest_plot) %>%
    dplyr::mutate(level = c("MAP", "Trial 3", "Trial 2", "Trial 1")) %>%
    dplyr::select(level, x, xmin, xmax)
  colnames(observed_data) <- c("level", "mean", "l", "u")

  # select pi_h and pi_star data from input data
  input_df <- as.data.frame(x$fit, pars = "pi_h") %>%
    dplyr::mutate("MAP" = x$pi_star)

  # calculate mean and quantiles from input data
  # this is our computed data
  computed_data <- data.frame(
    level = c("MAP", "Trial 3", "Trial 2", "Trial 1"),
    mean = c(mean(input_df$MAP),
             mean(input_df$`pi_h[3]`),
             mean(input_df$`pi_h[2]`),
             mean(input_df$`pi_h[1]`)),
    l = c(quantile(input_df$MAP, alpha / 2),
          quantile(input_df$`pi_h[3]`, alpha / 2),
          quantile(input_df$`pi_h[2]`, alpha / 2),
          quantile(input_df$`pi_h[1]`, alpha / 2)),
    u = c(quantile(input_df$MAP, 1 - alpha / 2),
          quantile(input_df$`pi_h[3]`, 1 - alpha / 2),
          quantile(input_df$`pi_h[2]`, 1 - alpha / 2),
          quantile(input_df$`pi_h[1]`, 1 - alpha / 2)),
    stringsAsFactors = FALSE)

  # expect observed mean and quantile data from the plot to be equal to
  # computed mean and quantile data from input data
  expect_equal(computed_data, observed_data)
})

test_that("forest_plot breaks when it should.", {

  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")

  # create sample data for testing
  set.seed(2024)
  alpha <- 0.05
  N <- 150
  time <- 0.5 + cumsum(rexp(N, 150))
  hazard <- 0.2
  prob <- 1 - exp(- hazard * time)
  y <- rbinom(N, size = 1, prob = prob)
  trial <- sample(1:3, size = N, replace = TRUE)
  x <- stan_map_patient_data(y = y, trial = trial, time = time,
                             a_alpha = 1 / 3, a_beta = 1 / 3,
                             b_alpha = 5, b_beta = 5,
                             seed = 2024) # declare seed to obtain
                                          # reproducible result when
                                          # calling the stan function

  # expect error with unexpected class
  x2 <- list(fit = c(1, 0, 1), "random_character_1", "random_character_2")
  expect_error(forest_plot(x2, alpha = 0.05),
               "input must be of type bssd_binary_map")

  # expect error with NA alpha
  expect_error(forest_plot(x, alpha = NA),
               "alpha must be non-missing")

  # expect error with alpha > 1
  expect_error(forest_plot(x, alpha = 1.05),
               "alpha must be between 0 and 1")

  # expect error with alpha < 0
  expect_error(forest_plot(x, alpha = -0.05),
               "alpha must be between 0 and 1")
})
