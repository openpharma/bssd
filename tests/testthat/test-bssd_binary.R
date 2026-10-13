# Create Input Data ------------------------------------------------------------

# Skip data generation steps if long tests are not enabled (default)
if (identical(Sys.getenv("RUNALLTESTS"), "TRUE")) {

# Using the code taken from stand_bssd example
set.seed(2024)
H <- 10
time_0 <- 0.5 + cumsum(rexp(H, 5))
# Use constant hazard model:
hazard <- 0.15
prob_0 <- 1 - exp(- hazard * time_0)
n_0 <- rpois(H, lambda = 100 * time_0)
y_0 <- rbinom(H, size = n_0, prob = prob_0)

# Generate MAP priors using summary data.
x <- stan_map_summary_data(y = y_0, n = n_0, time = time_0,
                           a_alpha = 1 / 3, a_beta = 1 / 3,
                           b_alpha = 5, b_beta = 5,
                           seed = 2024) # declare seed to obtain
                                        # reproducible result when
                                        # calling a stan function

# Use prior in blinded BSSD analysis
prob_arm1 <- 1 / 3
N <- 100
time <- 0.5 + cumsum(rexp(N, 100))
tmt <- sample(1:2, size = N, replace = TRUE,
              prob = c(prob_arm1, 1 - prob_arm1))
hazard1 <- 0.15
hazard2 <- 0.25
prob <- numeric(length = N)
prob[tmt == 1] <- 1 - exp(- hazard1 * time[tmt == 1])
prob[tmt == 2] <- 1 - exp(- hazard2 * time[tmt == 2])
y <- rbinom(N, size = 1, prob = prob)
mix <- fit_mixture(x)
w1 <- mix["w", ]
pi1_alpha <- mix["a", ]
pi1_beta <- mix["b", ]
w2 <- as.array(c(1))
pi2_alpha <- as.array(c(0.3))
pi2_beta <- as.array(c(0.3))

# Final Object
fit1 <- stan_bssd(y = y, time = time, prob_arm1 = prob_arm1,
                  w1 = w1, pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
                  w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta)

} else message("Please use bssd:::turn_on_long_tests() to enable long tests")

# Test with correct data -------------------------------------------------------
# fit1 object generated in previous section using stan_bssd function

test_that("Test: correct input data - fit1", {

  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")

  result <- bssd_binary(y = fit1$y,
                        time = fit1$time,
                        fit = fit1$fit)

  expect_true(is.integer(y))
  expect_true(is.numeric(time))
  expect_s3_class(result, c("bssd_binary", "bssd"))
  expect_s4_class(fit1$fit, c("stanfit"))
  expect_type(result, "list")

})


# Testing Param fit ------------------------------------------------------------
# This param contains several S4 objects. The only one to be tested is the
# par_dims, where mu = 2, and the other sub-elements are numeric(0)

# Test summary object ----------------------------------------------------------

test_that("Summary.bssd_binary output as tibble", {

  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")

    expect_true(tibble::is_tibble(summary.bssd_binary(fit1)))
})


test_that("Summary.bssd_binary output is dim 3x7", {

  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")

  expect_equal(dim(summary.bssd_binary(fit1)), c(3, 7))
})


test_that("Summary.bssd_binary output variable names", {

  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")

  expect_equal(attr(summary.bssd_binary(fit1), "names"),
             c("variable", "mean", "median", "sd", ".lower", ".upper", "alpha"))

})

test_that("Summary.bssd_binary output column attr", {

  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")

    expect_equal(c(sapply(summary.bssd_binary(fit1), class)[[1]],
                   sapply(summary.bssd_binary(fit1), class)[[2]],
                   sapply(summary.bssd_binary(fit1), class)[[3]],
                   sapply(summary.bssd_binary(fit1), class)[[4]],
                   sapply(summary.bssd_binary(fit1), class)[[5]],
                   sapply(summary.bssd_binary(fit1), class)[[6]],
                   sapply(summary.bssd_binary(fit1), class)[[7]]),
                 c("character", "numeric", "numeric", "numeric",
                   "numeric", "numeric", "numeric")

    )
})


# Test print object ------------------------------------------------------------
# Since we are supplying the seed value, we expect all results to be the same,
# and formatted correctly.

test_that("Verify the print.bssd_binary return expected format", {

  # Output as captured using RVIS 3.6.2 (for records only):
  # Prob(Event) in arm 1: 0.134 (95% HPDI 0.101, 0.169)
  # Prob(Event) in arm 2: 0.249 (95% HPDI 0.134, 0.368)
  # Prob(Event rate in arm 2 > arm 1): 0.975 (95% HPDI 0.972, 0.978)



  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")

  # Capture output printed out by bssd_binary method
  a1 <- capture.output(print.bssd_binary(fit1))

  # Check output structure (allow between 1 and 3 decimals, HPDI)
  # Arm 1
  expect_match(
    a1[1],
    paste0("^Prob\\(Event\\) in arm 1:\\s*\\d\\.\\d{1,3}\\s*\\(95% HPDI \\d\\.",
           "\\d{1,3},\\s*\\d\\.\\d{1,3}\\)\\s*$"),
    perl = TRUE
  )

  # Arm 2
  expect_match(
    a1[2],
    paste0("^Prob\\(Event\\) in arm 2:\\s*\\d\\.\\d{1,3}\\s*\\(95% HPDI \\d\\.",
           "\\d{1,3},\\s*\\d\\.\\d{1,3}\\)\\s*$"),
    perl = TRUE
  )

  # Comparison line (supports both '>' and '&gt;')
  expect_match(
    a1[3],
    paste0("^Prob\\(Event rate in arm 2 (>|&gt;) arm 1\\):\\s*\\d\\.\\d{1,3}",
           "\\s*\\(95% HPDI \\d\\.\\d{1,3},\\s*\\d\\.\\d{1,3}\\)\\s*$"),
    perl = TRUE
  )

  # Numeric extraction and tolerant checks
  get_nums <- function(s) {
    xs <- regmatches(s, gregexpr("\\d+\\.\\d+", s, perl = TRUE))[[1]]
    as.numeric(xs)
  }

  ar1 <- get_nums(a1[1])   # Arm 1
  ar2 <- get_nums(a1[2])   # Arm 2
  cmpl <- get_nums(a1[3])  # Comparison Arm 2 > Arm 1

  # Anchor to ballpark; tolerate platform/Stan tiny drift
  expect_equal(ar1[1], 0.133, tolerance = 0.03)
  expect_equal(ar2[1], 0.248, tolerance = 0.03)
  expect_equal(cmpl[1], 0.95, tolerance = 0.03)

  # Interval sanity checks in 4 steps
  # 1-Bounds and point estimates are probabilities in [0,1]
  for (x in list(ar1, ar2, cmpl)) {
    expect_gte(min(x[1:3]), 0)
    expect_lte(max(x[1:3]), 1)
  }

  # 2-Point estimate lies inside the HPDI (not just between 0 and 1)
  expect_lt(ar1[2], ar1[1])
  expect_lt(ar1[1], ar1[3])

  expect_lt(ar2[2], ar2[1])
  expect_lt(ar2[1], ar2[3])

  expect_lt(cmpl[2], cmpl[1])
  expect_lt(cmpl[1], cmpl[3])


  # 3-HPDI should not be absurdly wide; anchor to realistic range
  # (loosely enforce: HPDI width < 0.5 to catch broken outputs)
  expect_lt(ar1[3] - ar1[2], 0.5)
  expect_lt(ar2[3] - ar2[2], 0.5)
  expect_lt(cmpl[3] - cmpl[2], 0.5)

  # 4-HPDI should not collapse to a point (width > 0)
  expect_gt(ar1[3] - ar1[2], 0)
  expect_gt(ar2[3] - ar2[2], 0)
  expect_gt(cmpl[3] - cmpl[2], 0)

})

# Test that an output is produced with both choices of mcmc parameter ----------
test_that("mcmc ", {

  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")

  expect_output(print.bssd_binary(fit1, mcmc = TRUE))
  expect_output(print.bssd_binary(fit1, mcmc = FALSE))
})


# Test fit1$fit data frame -----------------------------------------------------
test_that("fit1$fit as data frame", {

  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")

  expect_true(is.data.frame(as.data.frame.bssd_binary(fit1,
                                                      pars = c("pi1", "pi2"))))
})

# Test plot object -------------------------------------------------------------

test_that("Plot object class", {

  testthat::skip_if_not(identical(Sys.getenv("RUNALLTESTS"), "TRUE"),
                        "Please use bssd:::turn_on_long_tests() to enable long tests")

  p <- plot.bssd_binary(fit1)
  expect_s3_class(p, "ggplot")
  expect_s3_class(p, "gg")

})
