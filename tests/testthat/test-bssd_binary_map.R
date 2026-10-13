test_that("test that bssd_binary_map input and output is as expected", {

  # create sample data for testing:
  # Historic summary-level data is made up of 10 trials (H). By assuming a
  # constant hazard model (hazard) we calculate the probability of adverse
  # event in a trial (prob) given time at risk (time). We calculate the number
  # of events in trials (n) and number of patients in trials (y) using poisson
  # and binomial distributions. We fit a Stan MAP generation model to this
  # synthetic summary level data to obtain MAP priors.

  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")

  set.seed(2024)
  H <- 10
  time <- 0.5 + cumsum(rexp(H, 5))
  hazard <- 0.15
  prob <- 1 - exp(- hazard * time)
  n <- rpois(H, lambda = 100 * time)
  y <- rbinom(H, size = n, prob = prob)

  stan_model <- stan_map_summary_data(y = y, n = n, time = time,
                                      a_alpha = 1 / 3, a_beta = 1 / 3,
                                      b_alpha = 5, b_beta = 5)

  # create an object representing the fit object obtained from the Stan model
  fit_obj <- stan_model$fit

  # create an object of class 'bssd_binary_map'
  bssd_obj <- bssd_binary_map(fit_obj)

  # expect input is of class 'stanfit'
  expect_s4_class(fit_obj, "stanfit")

  # expect output is of class 'bssd_binary_map' and 'bssd_map'
  expect_s3_class(bssd_obj, c("bssd_binary_map", "bssd_map"))

  # expect output contains elements 'fit', 'mean', 'ess' and 'pi_star'
  expect_identical(names(bssd_obj), c("fit", "mean", "ess", "pi_star"))
})

test_that("test that summary.bssd_binary_map output is as expected", {

  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")

  # create sample data for testing:
  set.seed(2024)
  H <- 10
  time <- 0.5 + cumsum(rexp(H, 5))
  hazard <- 0.15
  prob <- 1 - exp(- hazard * time)
  n <- rpois(H, lambda = 100 * time)
  y <- rbinom(H, size = n, prob = prob)

  stan_model <- stan_map_summary_data(y = y, n = n, time = time,
                                      a_alpha = 1 / 3, a_beta = 1 / 3,
                                      b_alpha = 5, b_beta = 5)

  # create an object representing the fit object obtained from the Stan model
  fit_obj <- stan_model$fit

  # create an object of class 'bssd_binary_map'
  bssd_obj <- bssd_binary_map(fit_obj)

  # expect output is a tibble
  expect_true(tibble::is_tibble(summary.bssd_binary_map(bssd_obj)))

  # expect tibble dim = 2x7
  expect_equal(dim(summary.bssd_binary_map(bssd_obj)), c(2, 7))

  # expect tibble has expected 'names' attribute
  expect_identical(attr(summary.bssd_binary_map(bssd_obj), "names"),
               c("variable", "mean", "median", "sd",
                 ".lower", ".upper", "alpha"))

  # expect tibble columns are of class 'character'/numeric'
  expect_identical(c(sapply(summary.bssd_binary_map(bssd_obj), class)[[1]],
                     sapply(summary.bssd_binary_map(bssd_obj), class)[[2]],
                     sapply(summary.bssd_binary_map(bssd_obj), class)[[3]],
                     sapply(summary.bssd_binary_map(bssd_obj), class)[[4]],
                     sapply(summary.bssd_binary_map(bssd_obj), class)[[5]],
                     sapply(summary.bssd_binary_map(bssd_obj), class)[[6]],
                     sapply(summary.bssd_binary_map(bssd_obj), class)[[7]]),
                   c("character", "numeric", "numeric", "numeric",
                     "numeric", "numeric", "numeric"))

  # expect tibble columns are of type 'character'/'double'
  expect_identical(c(sapply(summary.bssd_binary_map(bssd_obj), typeof)[[1]],
                     sapply(summary.bssd_binary_map(bssd_obj), typeof)[[2]],
                     sapply(summary.bssd_binary_map(bssd_obj), typeof)[[3]],
                     sapply(summary.bssd_binary_map(bssd_obj), typeof)[[4]],
                     sapply(summary.bssd_binary_map(bssd_obj), typeof)[[5]],
                     sapply(summary.bssd_binary_map(bssd_obj), typeof)[[6]],
                     sapply(summary.bssd_binary_map(bssd_obj), typeof)[[7]]),
                   c("character", "double", "double", "double",
                     "double", "double", "double"))
})


test_that("print.bssd_binary_map prints output to the console", {

  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")

  # create sample data for testing:
  set.seed(2024)
  H <- 10
  time <- 0.5 + cumsum(rexp(H, 5))
  hazard <- 0.15
  prob <- 1 - exp(- hazard * time)
  n <- rpois(H, lambda = 100 * time)
  y <- rbinom(H, size = n, prob = prob)

  stan_model <- stan_map_summary_data(y = y, n = n, time = time,
                                      a_alpha = 1 / 3, a_beta = 1 / 3,
                                      b_alpha = 5, b_beta = 5)

  # create an object representing the fit object obtained from the Stan model
  fit_obj <- stan_model$fit

  # create an object of class 'bssd_binary_map'
  bssd_obj <- bssd_binary_map(fit_obj)

  # expect output is printed to the console
  expect_output(print.bssd_binary_map(bssd_obj, mcmc = TRUE))
})

test_that("as.data.frame.bssd_binary_map returns a dataframe", {

  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")

  # create sample data for testing:
  set.seed(2024)
  H <- 10
  time <- 0.5 + cumsum(rexp(H, 5))
  hazard <- 0.15
  prob <- 1 - exp(- hazard * time)
  n <- rpois(H, lambda = 100 * time)
  y <- rbinom(H, size = n, prob = prob)

  stan_model <- stan_map_summary_data(y = y, n = n, time = time,
                                      a_alpha = 1 / 3, a_beta = 1 / 3,
                                      b_alpha = 5, b_beta = 5)

  # create an object representing the fit object obtained from the Stan model
  fit_obj <- stan_model$fit

  # create an object of class 'bssd_binary_map'
  bssd_obj <- bssd_binary_map(fit_obj)

  # expect output is a dataframe
  expect_true(
    is.data.frame(
      as.data.frame.bssd_binary_map(
        bssd_obj,
        pars = c("pi_h", "avg", "pi_star"))))
})

test_that("plot.bssd_binary_map returns a plot", {

  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")

  # create sample data for testing:
  set.seed(2024)
  H <- 10
  time <- 0.5 + cumsum(rexp(H, 5))
  hazard <- 0.15
  prob <- 1 - exp(- hazard * time)
  n <- rpois(H, lambda = 100 * time)
  y <- rbinom(H, size = n, prob = prob)

  stan_model <- stan_map_summary_data(y = y, n = n, time = time,
                                      a_alpha = 1 / 3, a_beta = 1 / 3,
                                      b_alpha = 5, b_beta = 5)

  # create an object representing the fit object obtained from the Stan model
  fit_obj <- stan_model$fit

  # create an object of class 'bssd_binary_map'
  bssd_obj <- bssd_binary_map(fit_obj)

  # store plot as an object
  plot <- plot.bssd_binary_map(bssd_obj)

  # expect output plot is of class 'ggplot'
  expect_s3_class(plot, "ggplot")

  # Check axis labels robustly
  built_plot <- ggplot2::ggplot_build(plot)

  # expect x-axis label is 'Prob(event)'
  expect_identical(built_plot$plot$labels$x, "Prob(event)")

  # expect y-axis label is 'density'
  expect_identical(as.character(built_plot$plot$labels$y), "density")

  # expect no error when printing the plot
  expect_error(print(plot$data), NA)
})
