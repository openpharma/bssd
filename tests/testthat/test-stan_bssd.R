
#- Data In ----------------------------------------------------------------
  set.seed(2021)
  prob_arm1 <- 0.333
  N <- 100
  time <- 0.5 + cumsum(rexp(N, 100))
  arm <- sample(1:2, size = N, replace = TRUE,
                prob = c(prob_arm1, 1 - prob_arm1))
  hazard1 <- 0.1
  hazard2 <- 0.2
  prob <- numeric(length = N)
  prob[arm == 1] <- 1 - exp(- hazard1 * time[arm == 1])
  prob[arm == 2] <- 1 - exp(- hazard2 * time[arm == 2])
  y <- rbinom(N, size = 1, prob = prob)
  w1 <- c(0.9, 0.1)
  pi1_alpha <- c(38.64074, 1)
  pi1_beta <- c(436.3206, 1)
  w2 <- 1
  pi2_alpha <- 0.3
  pi2_beta <- 0.3

#- Y testing ----------------------------------------------------------------
test_that("stan_bssd breaks when y has incorrect data.", {
  # Correct data
  y2 <- y
  y2[2] <- 1.5
  expect_error(
    stan_bssd(
      y = y2,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), "y should be a vector of 0s and 1s only"
  )

  y2 <- y
  y2[2] <- NA
  expect_error(
    stan_bssd(
      y = y2,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), "y should be a vector of 0s and 1s only"
  )
})

#- Time testing ----------------------------------------------------------------
test_that("stan_bssd breaks when time has incorrect data.", {

  time2 <- time
  time2[2] <- -1
  expect_error(
    stan_bssd(
      y = y,
      time = time2,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), "Each element of time should be strictly positive and non-missing."
  )

  time2 <- time
  time2[2] <- NA
  expect_error(
    stan_bssd(
      y = y,
      time = time2,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), "Each element of time should be strictly positive and non-missing."
  )
})

#- Params testing -----------------------------------------------------------
test_that("stan_bssd breaks when params have incorrect data.", {
  # w2
  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = -1,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
                "be strictly positive and non-missing.")
  )

  # pi2_alpha
  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = -pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  # pi2_beta
  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = -pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )


  # w1
  w1a <- w1
  w1a[1] <- -1
  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1a,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  # pi1_alpha
  pi1_alpha1 <- pi1_alpha
  pi1_alpha1[1] <- -1
  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha1,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  # pi1_beta
  pi1_beta1 <- pi1_beta
  pi1_beta1[1] <- -1
  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta1,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

})

#- Arm tmt testing -------------------------------------------------------------
test_that("stan_bssd breaks when arm have incorrect data.", {
  arm2 <- arm
  arm2[2] <- 0
  expect_error(
    stan_bssd(
      y = y,
      time = time,
      tmt = arm2,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), "tmt should be a vector of 1s and 2s only."
  )

  arm2 <- arm
  arm2[2] <- NA
  expect_error(
    stan_bssd(
      y = y,
      time = time,
      tmt = arm2,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), "tmt should be a vector of 1s and 2s only."
  )
})

#- Lengths params -------------------------------------------------------
test_that("stan_bssd breaks when lengths are incorrect.", {
  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1[-1],
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), "w1 must be the same length as pi1_alpha and pi1_beta"
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha[-1],
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), "pi1_alpha must be the same length as w1 and pi1_beta"
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta[-1],
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), "pi1_beta must be the same length as w1 and pi1_alpha"
  )


  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2[-1],
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), "w2 must be the same length as pi2_alpha and pi2_beta"
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha[-1],
      pi2_beta = pi2_beta,
      refresh = 0
    ), "pi2_alpha must be the same length as w2 and pi2_beta"
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta[-1],
      refresh = 0
    ), "pi2_beta must be the same length as w2 and pi2_alpha"
  )
})

#- tmt vs prob_arm1 --------------------------------------------------
test_that("stan_bssd breaks when tmt or prob_arm1 are incorrect.", {
  ### Both missing
  expect_error(
    stan_bssd(
      y = y,
      time = time,
      tmt = NULL,
      prob_arm1 = NULL,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), "Specify tmt or prob_arm1"
  )

  ### Both present
  expect_error(
    stan_bssd(
      y = y,
      time = time,
      tmt = arm,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), "Specify tmt or prob_arm1 but not both"
  )
  ### tmt
  # lengts
  expect_error(
    stan_bssd(
      y = y[-1],
      time = time,
      tmt = arm,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), "y must be the same length as time and tmt."
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time[-1],
      tmt = arm,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), "time must be the same length as y and tmt."
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      tmt = arm[-1],
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), "tmt must be the same length as y and time"
  )

  ### prob_arm1
  # correct data
  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = -0.1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), "prob_arm1 must be > 0 and <= 1"
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = 1.1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), "prob_arm1 must be > 0 and <= 1"
  )

  # y and time with prob_arm1
  expect_error(
    stan_bssd(
      y = y[-1],
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), "y and time must be the same length."
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time[-1],
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), "y and time must be the same length."
  )
})

# Erroneous hyperparams -----------------------------------------------------
test_that("stan_bssd breaks when params are incorrect.",  {

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = 0,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = 0,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = 0,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = 0,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = 0,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = 0,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = -1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = -1,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = -1,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = -1,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = -1,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = -1,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )


  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = NA,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = NA,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = NA,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = NA,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = NA,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = NA,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = NULL,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = NULL,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = NULL,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = NULL,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = NULL,
      pi2_beta = pi2_beta,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )

  expect_error(
    stan_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      w1 = w1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      w2 = w2,
      pi2_alpha = pi2_alpha,
      pi2_beta = NULL,
      refresh = 0
    ), paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
              "be strictly positive and non-missing.")
  )


})

#- small sample size ----------------------------------------------------
test_that("stan_bssd works as it should with small sample size.", {

  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")


  # Params
  seed <- 2021
  set.seed(seed)
  prob_arm1 <- 0.333
  N <- 100
  time <- 0.5 + cumsum(rexp(N, 100))
  arm <- sample(1:2, size = N, replace = TRUE,
                prob = c(prob_arm1, 1 - prob_arm1))
  hazard1 <- 0.1
  unit_prob1 <- 1 - exp(- hazard1)
  hazard2 <- 0.2
  unit_prob2 <- 1 - exp(- hazard2)
  unit_prob <- c(unit_prob1, unit_prob2)
  prob <- numeric(length = N)
  prob[arm == 1] <- 1 - exp(- hazard1 * time[arm == 1])
  prob[arm == 2] <- 1 - exp(- hazard2 * time[arm == 2])
  y <- rbinom(N, size = 1, prob = prob)
  w1 <- c(0.9, 0.1)
  pi1_alpha <- c(38.64074, 1)
  pi1_beta <- c(436.3206, 1)
  w2 <- 1
  pi2_alpha <- 0.3
  pi2_beta <- 0.3

  # Blinded analysis
# Suppress stan warnings because they are irrelevant to the test at stake.
  suppressWarnings(
    x <- stan_bssd(y = y, time = time, prob_arm1 = prob_arm1,
                   w1 = w1, pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
                   w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta,
                   refresh = 0, seed = seed)
  )

  # Estimated means for pi_h should be close to unit_prob
  epsilon <- 0.1
  pi_h <- as.data.frame(x) %>% colMeans()
  expect_true(all(abs(pi_h - unit_prob) < epsilon))

  # Estimated medians for pi_h should be close to unit_prob
  epsilon <- 0.1
  pi_h <- as.data.frame(x) %>% apply(2, median)
  expect_true(all(abs(pi_h - unit_prob) < epsilon))

  # 99% CIs for pi_h should contain unit_prob
  pi_h_ci <- as.data.frame(x) %>%
    apply(2, quantile, probs = c(0.005, 0.995)) %>%
    t() %>%
    as.data.frame()
  colnames(pi_h_ci) <- c("lower", "upper")
  all_covered <- pi_h_ci %>%
    dplyr::mutate(covered = lower < unit_prob & upper > unit_prob) %>%
    dplyr::summarise(all_covered = all(covered)) %>%
    dplyr::pull(all_covered)
  expect_true(all_covered)


  # Unblinded analysis
# Suppress stan warnings because they are irrelevant to the test at stake.
  suppressWarnings(
    x <- stan_bssd(y = y, time = time, tmt = arm,
                   w1 = w1, pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
                   w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta,
                   refresh = 0, seed = seed)
  )

  # Estimated means for pi_h should be close to unit_prob
  epsilon <- 0.1
  pi_h <- as.data.frame(x) %>% colMeans()
  expect_true(all(abs(pi_h - unit_prob) < epsilon))

  # Estimated medians for pi_h should be close to unit_prob
  epsilon <- 0.1
  pi_h <- as.data.frame(x) %>% apply(2, median)
  expect_true(all(abs(pi_h - unit_prob) < epsilon))

  # 99% CIs for pi_h should contain unit_prob
  pi_h_ci <- as.data.frame(x) %>%
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

#- modest sample size ----------------------------------------------------
test_that("stan_bssd works as it should with modest sample size.", {

  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")

  # Params
  seed <- 2021
  set.seed(seed)
  prob_arm1 <- 0.333
  N <- 500
  time <- 0.5 + cumsum(rexp(N, 100))
  arm <- sample(1:2, size = N, replace = TRUE,
                prob = c(prob_arm1, 1 - prob_arm1))
  hazard1 <- 0.1
  unit_prob1 <- 1 - exp(- hazard1)
  hazard2 <- 0.2
  unit_prob2 <- 1 - exp(- hazard2)
  unit_prob <- c(unit_prob1, unit_prob2)
  prob <- numeric(length = N)
  prob[arm == 1] <- 1 - exp(- hazard1 * time[arm == 1])
  prob[arm == 2] <- 1 - exp(- hazard2 * time[arm == 2])
  y <- rbinom(N, size = 1, prob = prob)
  w1 <- c(0.9, 0.1)
  pi1_alpha <- c(38.64074, 1)
  pi1_beta <- c(436.3206, 1)
  w2 <- 1
  pi2_alpha <- 0.3
  pi2_beta <- 0.3

  # Blinded analysis
# Suppress stan warnings because they are irrelevant to the test at stake.
  suppressWarnings(
    x <- stan_bssd(y = y, time = time, prob_arm1 = prob_arm1,
                   w1 = w1, pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
                   w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta,
                   refresh = 0, seed = seed)
  )

  # Estimated means for pi_h should be close to unit_prob
  epsilon <- 0.05
  pi_h <- as.data.frame(x) %>% colMeans()
  expect_true(all(abs(pi_h - unit_prob) < epsilon))

  # Estimated medians for pi_h should be close to unit_prob
  epsilon <- 0.05
  pi_h <- as.data.frame(x) %>% apply(2, median)
  expect_true(all(abs(pi_h - unit_prob) < epsilon))

  # 99% CIs for pi_h should contain unit_prob
  pi_h_ci <- as.data.frame(x) %>%
    apply(2, quantile, probs = c(0.005, 0.995)) %>%
    t() %>%
    as.data.frame()
  colnames(pi_h_ci) <- c("lower", "upper")
  all_covered <- pi_h_ci %>%
    dplyr::mutate(covered = lower < unit_prob & upper > unit_prob) %>%
    dplyr::summarise(all_covered = all(covered)) %>%
    dplyr::pull(all_covered)
  expect_true(all_covered)


  # Unblinded analysis
  # Suppress stan warnings because they are irrelevant to the test at stake.
  suppressWarnings(
    x <- stan_bssd(y = y, time = time, tmt = arm,
                   w1 = w1, pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
                   w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta,
                   refresh = 0, seed = seed)
  )

  # Estimated means for pi_h should be close to unit_prob
  epsilon <- 0.05
  pi_h <- as.data.frame(x) %>% colMeans()
  expect_true(all(abs(pi_h - unit_prob) < epsilon))

  # Estimated medians for pi_h should be close to unit_prob
  epsilon <- 0.05
  pi_h <- as.data.frame(x) %>% apply(2, median)
  expect_true(all(abs(pi_h - unit_prob) < epsilon))

  # 99% CIs for pi_h should contain unit_prob
  pi_h_ci <- as.data.frame(x) %>%
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

#- large sample size ----------------------------------------------------
test_that("stan_bssd works as it should with large sample size.", {

  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")

  # Params
  seed <- 2021
  set.seed(seed)
  prob_arm1 <- 0.333
  N <- 2000
  time <- 0.5 + cumsum(rexp(N, 100))
  arm <- sample(1:2, size = N, replace = TRUE,
                prob = c(prob_arm1, 1 - prob_arm1))
  hazard1 <- 0.1
  unit_prob1 <- 1 - exp(- hazard1)
  hazard2 <- 0.2
  unit_prob2 <- 1 - exp(- hazard2)
  unit_prob <- c(unit_prob1, unit_prob2)
  prob <- numeric(length = N)
  prob[arm == 1] <- 1 - exp(- hazard1 * time[arm == 1])
  prob[arm == 2] <- 1 - exp(- hazard2 * time[arm == 2])
  y <- rbinom(N, size = 1, prob = prob)
  w1 <- c(0.9, 0.1)
  pi1_alpha <- c(38.64074, 1)
  pi1_beta <- c(436.3206, 1)
  w2 <- 1
  pi2_alpha <- 0.3
  pi2_beta <- 0.3

  # Blinded analysis
# Suppress stan warnings because they are irrelevant to the test at stake.
  suppressWarnings(
    x <- stan_bssd(y = y, time = time, prob_arm1 = prob_arm1,
                   w1 = w1, pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
                   w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta,
                   refresh = 0, seed = seed)
  )

  # Estimated means for pi_h should be close to unit_prob
  epsilon <- 0.01
  pi_h <- as.data.frame(x) %>% colMeans()
  expect_true(all(abs(pi_h - unit_prob) < epsilon))

  # Estimated medians for pi_h should be close to unit_prob
  epsilon <- 0.01
  pi_h <- as.data.frame(x) %>% apply(2, median)
  expect_true(all(abs(pi_h - unit_prob) < epsilon))

  # 99% CIs for pi_h should contain unit_prob
  pi_h_ci <- as.data.frame(x) %>%
    apply(2, quantile, probs = c(0.005, 0.995)) %>%
    t() %>%
    as.data.frame()
  colnames(pi_h_ci) <- c("lower", "upper")
  all_covered <- pi_h_ci %>%
    dplyr::mutate(covered = lower < unit_prob & upper > unit_prob) %>%
    dplyr::summarise(all_covered = all(covered)) %>%
    dplyr::pull(all_covered)
  expect_true(all_covered)


  # Unblinded analysis
# Suppress stan warnings because they are irrelevant to the test at stake.
  suppressWarnings(
    x <- stan_bssd(y = y, time = time, tmt = arm,
                   w1 = w1, pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
                   w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta,
                   refresh = 0, seed = seed)
  )

  # Estimated means for pi_h should be close to unit_prob
  epsilon <- 0.01
  pi_h <- as.data.frame(x) %>% colMeans()
  expect_true(all(abs(pi_h - unit_prob) < epsilon))

  # Estimated medians for pi_h should be close to unit_prob
  epsilon <- 0.01
  pi_h <- as.data.frame(x) %>% apply(2, median)
  expect_true(all(abs(pi_h - unit_prob) < epsilon))

  # 99% CIs for pi_h should contain unit_prob
  pi_h_ci <- as.data.frame(x) %>%
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

#- dominated by the priors ----------------------------------------------
test_that("stan_bssd can be dominated by the priors", {

  # Params
  seed <- 2021
  set.seed(seed)
  prob_arm1 <- 0.5
  N <- 10
  time <- 0.5 + cumsum(rexp(N, 100))
  arm <- sample(1:2, size = N, replace = TRUE,
                prob = c(prob_arm1, 1 - prob_arm1))
  hazard1 <- 0.1
  unit_prob1 <- 1 - exp(- hazard1)
  hazard2 <- 0.2
  unit_prob2 <- 1 - exp(- hazard2)
  unit_prob <- c(unit_prob1, unit_prob2)
  prob <- numeric(length = N)
  prob[arm == 1] <- 1 - exp(- hazard1 * time[arm == 1])
  prob[arm == 2] <- 1 - exp(- hazard2 * time[arm == 2])
  y <- rbinom(N, size = 1, prob = prob)
  w1 <- 1
  pi1_alpha <- 400
  pi1_beta <- 1600
  prior_prob1 <- pi1_alpha / (pi1_alpha + pi1_beta)
  w2 <- 1
  pi2_alpha <- 600
  pi2_beta <- 1400
  prior_prob2 <- pi2_alpha / (pi2_alpha + pi2_beta)
  prior_prob <- c(prior_prob1, prior_prob2)

  # Blinded analysis
# Suppress stan warnings because they are irrelevant to the test at stake.
  suppressWarnings(
    x <- stan_bssd(y = y, time = time, prob_arm1 = prob_arm1,
                   w1 = w1, pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
                   w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta,
                   refresh = 0, seed = seed)
  )

  # Estimated means for pi_h should be close to prior_prob
  epsilon <- 0.01
  pi_h <- as.data.frame(x) %>% colMeans()
  expect_true(all(abs(pi_h - prior_prob) < epsilon))

  # Estimated medians for pi_h should be close to prior_prob
  epsilon <- 0.01
  pi_h <- as.data.frame(x) %>% apply(2, median)
  expect_true(all(abs(pi_h - prior_prob) < epsilon))

  # 99% CIs for pi_h should contain prior_prob
  pi_h_ci <- as.data.frame(x) %>%
    apply(2, quantile, probs = c(0.005, 0.995)) %>%
    t() %>%
    as.data.frame()
  colnames(pi_h_ci) <- c("lower", "upper")
  all_covered <- pi_h_ci %>%
    dplyr::mutate(covered = lower < prior_prob & upper > prior_prob) %>%
    dplyr::summarise(all_covered = all(covered)) %>%
    dplyr::pull(all_covered)
  expect_true(all_covered)


  # Unlinded analysis
# Suppress stan warnings because they are irrelevant to the test at stake.
  suppressWarnings(
    x <- stan_bssd(y = y, time = time, tmt = arm,
                   w1 = w1, pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
                   w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta,
                   refresh = 0, seed = seed)
  )

  # Estimated means for pi_h should be close to prior_prob
  epsilon <- 0.01
  pi_h <- as.data.frame(x) %>% colMeans()
  expect_true(all(abs(pi_h - prior_prob) < epsilon))

  # Estimated medians for pi_h should be close to prior_prob
  epsilon <- 0.01
  pi_h <- as.data.frame(x) %>% apply(2, median)
  expect_true(all(abs(pi_h - prior_prob) < epsilon))

  # 99% CIs for pi_h should contain prior_prob
  pi_h_ci <- as.data.frame(x) %>%
    apply(2, quantile, probs = c(0.005, 0.995)) %>%
    t() %>%
    as.data.frame()
  colnames(pi_h_ci) <- c("lower", "upper")
  all_covered <- pi_h_ci %>%
    dplyr::mutate(covered = lower < prior_prob & upper > prior_prob) %>%
    dplyr::summarise(all_covered = all(covered)) %>%
    dplyr::pull(all_covered)
  expect_true(all_covered)

})
