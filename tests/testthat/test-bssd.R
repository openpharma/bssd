test_that("bssd breaks when it should.", {

  # Params
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
  y <- sum(rbinom(N, size = 1, prob = prob))
  time <- sum(time)
  pi1_alpha <- 38.64074
  pi1_beta <- 436.3206
  pi2_alpha <- 0.3
  pi2_beta <- 0.3

  # Mismatched lengths
  expect_error({
    approx_bssd(
      y = y[-1],
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time[-1],
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha[-1],
      pi1_beta = pi1_beta,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta[-1],
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      pi2_alpha = pi2_alpha[-1],
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta[-1]
    )
  })

  expect_error({
    y2 <- y
    y2[2] <- 1.5
    approx_bssd(
      y = y2,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    y2 <- y
    y2[1] <- NA
    approx_bssd(
      y = y2,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    time2 <- time
    time2[1] <- -1
    approx_bssd(
      y = y,
      time = time2,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    time2 <- time
    time2[1] <- NA
    approx_bssd(
      y = y,
      time = time2,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta
    )
  })

  # Erroneous hyperparams
  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = 0,
      pi1_beta = pi1_beta,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = 0,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      pi2_alpha = 0,
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      pi2_alpha = pi2_alpha,
      pi2_beta = 0
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = -1,
      pi1_beta = pi1_beta,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = -1,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      pi2_alpha = -1,
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      pi2_alpha = pi2_alpha,
      pi2_beta = -1
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = NA,
      pi1_beta = pi1_beta,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = NA,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      pi2_alpha = NA,
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      pi2_alpha = pi2_alpha,
      pi2_beta = NA
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = NULL,
      pi1_beta = pi1_beta,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = NULL,
      pi2_alpha = pi2_alpha,
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      pi2_alpha = NULL,
      pi2_beta = pi2_beta
    )
  })

  expect_error({
    approx_bssd(
      y = y,
      time = time,
      prob_arm1 = prob_arm1,
      pi1_alpha = pi1_alpha,
      pi1_beta = pi1_beta,
      pi2_alpha = pi2_alpha,
      pi2_beta = NULL
    )
  })

})

test_that("bssd works as it should with small sample size.", {

  # Params
  seed <- 2021
  set.seed(seed)
  prob_arm1 <- 0.333
  N <- 20
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
  y <- sum(rbinom(N, size = 1, prob = prob))
  time <- sum(time)
  pi1_alpha <- 38.64074
  pi1_beta <- 436.3206
  pi2_alpha <- 0.3
  pi2_beta <- 0.3

  x <- approx_bssd(y = y, time = time, prob_arm1 = prob_arm1,
            pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
            pi2_alpha = pi2_alpha, pi2_beta = pi2_beta)
  expect_true(abs(x - 0.5) < 0.1)

})

test_that("bssd works as it should with modest sample size.", {

  # Params
  seed <- 2021
  set.seed(seed)
  prob_arm1 <- 0.333
  N <- 200
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
  y <- sum(rbinom(N, size = 1, prob = prob))
  time <- sum(time)
  pi1_alpha <- 38.64074
  pi1_beta <- 436.3206
  pi2_alpha <- 0.3
  pi2_beta <- 0.3

  x <- approx_bssd(y = y, time = time, prob_arm1 = prob_arm1,
            pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
            pi2_alpha = pi2_alpha, pi2_beta = pi2_beta)
  expect_true(abs(x - 0.9) < 0.1)

})

test_that("bssd works as it should with large sample size.", {

  # Params
  seed <- 2021
  set.seed(seed)
  prob_arm1 <- 0.333
  N <- 2000
  time <- 0.5 + cumsum(rexp(N, 1000))
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
  y <- sum(rbinom(N, size = 1, prob = prob))
  time <- sum(time)
  pi1_alpha <- 38.64074
  pi1_beta <- 436.3206
  pi2_alpha <- 0.3
  pi2_beta <- 0.3

  x <- approx_bssd(y = y, time = time, prob_arm1 = prob_arm1,
            pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
            pi2_alpha = pi2_alpha, pi2_beta = pi2_beta)
  expect_true(abs(x - 0.99) < 0.1)

})

test_that("bssd can be dominated by the priors", {

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
  y <- sum(rbinom(N, size = 1, prob = prob))
  time <- sum(time)

  # Definitely different
  pi1_alpha <- 400
  pi1_beta <- 1600
  pi2_alpha <- 600
  pi2_beta <- 1400
  x <- approx_bssd(y = y, time = time, prob_arm1 = prob_arm1,
                   pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
                   pi2_alpha = pi2_alpha, pi2_beta = pi2_beta)

  expect_true(abs(x - 0.99) < 0.05)

  # Definitely the same
  pi1_alpha <- 400
  pi1_beta <- 1600
  pi2_alpha <- 400
  pi2_beta <- 1600
  x <- approx_bssd(y = y, time = time, prob_arm1 = prob_arm1,
                   pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
                   pi2_alpha = pi2_alpha, pi2_beta = pi2_beta)

  expect_true(abs(x - 0.5) < 0.05)

})
