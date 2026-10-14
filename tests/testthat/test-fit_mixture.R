test_that("fit_mixture input and output is as expected.", {

  # create sample data for testing:
  # Historic summary-level data is made up of 10 trials (H_1) for Example 1 and
  # 3 trials (H_2) for Example 2. By assuming a constant hazard model (hazard_1
  # and hazard_2) we calculate the probability of adverse event in a trial
  # (prob_1 and prob_2) given time at risk (time_1 and time_2). We calculate
  # the number of events in trials (n_1 and n_2) and number of patients in
  # trials (y_1 and y_2) using poisson and binomial distributions. We fit a
  # Stan MAP generation model to this synthetic summary level data to obtain
  # MAP priors.


  # Example 1 - sample data is fitted to a 1 component mixture
  seed_1 <- 2024
  set.seed(seed_1)
  H_1 <- 10
  time_1 <- 0.5 + cumsum(rexp(H_1, 5))
  hazard_1 <- 0.15
  prob_1 <- 1 - exp(- hazard_1 * time_1)
  n_1 <- rpois(H_1, lambda = 100 * time_1)
  y_1 <- rbinom(H_1, size = n_1, prob = prob_1)
  x_1 <- stan_map_summary_data(y = y_1, n = n_1, time = time_1,
                               a_alpha = 1 / 3, a_beta = 1 / 3,
                               b_alpha = 5, b_beta = 5,
                               seed = 2024) # declare seed to obtain
                                            # reproducible result when
                                            # calling a stan function

  # fit a mixture of conjugate distributions to MCMC samples - force 1 comp
  mix_1 <- fit_mixture(x_1, Nc = 1)
  mix_1

  # Create function that computes the average of a mixed beta
  mixture_mean_beta <- function(mix) {
    # mix is expected to be a matrix or data frame with rows: w, a, b
    # and columns for mixture components

    w <- mix["w", ]
    a <- mix["a", ]
    b <- mix["b", ]

    # Means of each component
    mu_k <- a / (a + b)

    # Mixture mean
    sum(w * mu_k)
  }

  # expect input is of class 'bssd_binary_map_pl', 'bssd_binary_map', 'bssd_map'
  expect_s3_class(x_1, c("bssd_binary_map_sl", "bssd_binary_map", "bssd_map"))

  # expect fit object is of class 'stanfit'
  expect_s4_class(x_1$fit, "stanfit")

  # expect output 'mix_1' is of class 'EM', 'EMbmm', 'betaMix', 'mix'
  expect_equal(class(mix_1), c("EM", "EMbmm", "betaMix", "mix"))

  # expect dimension names to be 'w', 'a', 'b'
  expect_equal(attr(mix_1, "dimnames")[[1]], c("w", "a", "b"))

  # following tests are specific to Example 1:

  # expect output 'mix_1' is of dimension 3x1
  expect_equal(attr(mix_1, "dim"), c(3, 1))

  # expect dimension name to be 'comp1'
  expect_equal(attr(mix_1, "dimnames")[[2]], "comp1")

  # expect number of components to be 1
  expect_equal(attr(mix_1, "Nc"), 1)

  # Test 1-component mixed beta parameters
  # by using seed_1, H_1 and hazard_1 values, expect the following
  # 'w', 'a' and 'b' values for 'comp1'
  if (seed_1 == 2024 & H_1 == 10 & hazard_1 == 0.15) {

    # mix_1 as computed in RVAL 3.6.2 using the seed 2024
    mix_1_reference <- rbind(
      w = c(1),
      a = c(51.90465),
      b = c(336.82782)
    )

    # Expect averages from mix_1 and mix_1_reference to closely match
    expect_equal(mixture_mean_beta(mix_1),
                 mixture_mean_beta(mix_1_reference),
                 tolerance = 0.01)

    # Expect weight of mix_1 close to 1
    expect_equal(mix_1[1,1],
                 1,
                 tolerance = 0.00001)

  }



  # Example 2 - sample data is fitted to a 2 component mixture
  seed_2 <- 2024
  set.seed(seed_2)
  H_2 <- 3
  time_2 <- 0.5 + cumsum(rexp(H_2, 5))
  hazard_2 <- c(0.3, 0.06, 0.01)
  prob_2 <- 1 - exp(- hazard_2 * time_2)
  n_2 <- rpois(H_2, lambda = 100 * time_2)
  y_2 <- rbinom(H_2, size = n_2, prob = prob_2)
  x_2 <- stan_map_summary_data(y = y_2, n = n_2, time = time_2,
                               a_alpha = 1 / 3, a_beta = 1 / 3,
                               b_alpha = 5, b_beta = 5,
                               seed = 2024) # declare seed to obtain
                                            # reproducible result when
                                            # calling a stan function

  # fit a mixture of conjugate distributions to MCMC samples - force 2 comp
  mix_2 <- fit_mixture(x_2, Nc = 2)
  mix_2

  # following tests are specific to Example 2:

  # expect output 'mix_2' is of dimension 3x2
  expect_equal(attr(mix_2, "dim"), c(3, 2))

  # expect dimension names to be 'comp1', 'comp2'
  expect_equal(attr(mix_2, "dimnames")[[2]], c("comp1", "comp2"))

  # expect number of components to be 2
  expect_equal(attr(mix_2, "Nc"), 2)

  # Test 2-component mixed beta parameters
  # by using seed_2, H_2 and hazard_2 values, expect the following
  # 'w', 'a' and 'b' values for 'comp1' and 'comp2'
  if (all(seed_2 == 2024 & H_2 == 3 & hazard_2 == c(0.3, 0.06, 0.01))) {

    # mix_2 as computed in RVAL 3.6.2 using seed 2024
    mix_2_reference <- rbind(
      w = c(0.6894116, 0.3105884),
      a = c(6.2149383, 2.0884288),
      b = c(43.5744007, 16.8450060)
    )

    # Expect averages from mix_2 and mix_2_reference to closely match
    expect_equal(mixture_mean_beta(mix_2),
                 mixture_mean_beta(mix_2_reference),
                 tolerance = 0.05)

    # Expect weights of mix_2 closely add-up to 1
    expect_equal(sum(mix_2_reference[1,]),
                 1,
                 tolerance = 0.00001)

  }

})
