# Testing with what should be correct numbers
test_that("Test with valid inputs.", {
 result <- approx_bssd(y = 10,
                       time = 100,
                       prob_arm1 = 0.5,
                       pi1_alpha = 30,
                       pi1_beta = 70,
                       pi2_alpha = 0.3,
                       pi2_beta = 0.3,
                       num_samples = 10000,
                       seed = 12)
 expect_true(is.numeric(result))
})


# Test for seed value checks
test_that("Test for invalid seed value.", {
  expect_error(approx_bssd(y = 10,
                           time = 100,
                           prob_arm1 = 0.5,
                           pi1_alpha = 40,
                           pi1_beta  = 60,
                           pi2_alpha = 0.3,
                           pi2_beta = 0.3,
                           num_samples = 10000,
                           seed = "not a number"))

  expect_error(approx_bssd(y = 10,
                           time = 100,
                           prob_arm1 = 0.5,
                           pi1_alpha = 25,
                           pi1_beta = 75,
                           pi2_alpha = 0.3,
                           pi2_beta = 0.3,
                           num_samples = 10000,
                           seed = -1))

  expect_error(approx_bssd(y = 10,
                           time = 100,
                           prob_arm1 = 0.5,
                           pi1_alpha = 200,
                           pi1_beta = 300,
                           pi2_alpha = 0.3,
                           pi2_beta = 0.3,
                           num_samples = 10000,
                           seed = 1.5))
})


# Seed Parameter Tests - Sameness test
test_that("Seed parameter sameness test.", {
  # Test with valid seed
  result1 <- approx_bssd(y = 10, time = 100, prob_arm1 = 0.5, pi1_alpha = 10,
   pi1_beta = 90, pi2_alpha = 0.3, pi2_beta = 0.3,
   num_samples = 10000, seed = 123)
  result2 <- approx_bssd(y = 10, time = 100, prob_arm1 = 0.5, pi1_alpha = 10,
   pi1_beta = 90, pi2_alpha = 0.3, pi2_beta = 0.3,
   num_samples = 10000, seed = 123)
  expect_equal(result1, result2)
})

# Test for valid inputs and output range
test_that("Valid inputs yield outputs within expected range.", {
  expect_silent({
    result <- approx_bssd(y = 5, time = 50, prob_arm1 = 0.5,
     pi1_alpha = 2, pi1_beta = 5, pi2_alpha = 1, pi2_beta = 1,
     num_samples = 1000, seed = 100)
  })
  expect_true(result >= 0 && result <= 1)
})

# Test for parameter type and range validation
test_that("Parameter validation catches errors.", {
  expect_error(approx_bssd(y = -1.2, time = 100, prob_arm1 = 0.5,
   pi1_alpha = 9, pi1_beta = 92, pi2_alpha = 0.3, pi2_beta = 0.3,
   num_samples = 500, seed = 123),
   "Y must be non-missing and a non-negative integer.")
  expect_error(approx_bssd(y = 10, time = -100, prob_arm1 = 0.5,
   pi1_alpha = 9, pi1_beta = 92, pi2_alpha = 0.3, pi2_beta = 0.3,
   num_samples = 500, seed = 123),
   "Time must be non-missing and a non-negative rational number.")
})

# Test (y > min(Nc, Na))
test_that("Parameter validation catches errors.", {
  expect_error(approx_bssd(y = 2000, time = 10, prob_arm1 = 0.5,
   pi1_alpha = 9, pi1_beta = 92, pi2_alpha = 0.3, pi2_beta = 0.3,
   num_samples = 500, seed = 123),
   paste0("The number of observed events y, cannot be larger than the ",
   "smallest treatment group."))
})

# Test for parameter time is handled correctly as a rational number
test_that("Function handles rational time values.", {
  # time parameter with decimal place present
  expect_true(approx_bssd(y = 2, time = 10.576, prob_arm1 = 0.5,
   pi1_alpha = 9, pi1_beta = 92, pi2_alpha = 0.3, pi2_beta = 0.3,
   num_samples = 500, seed = 123) >= 0)
  # time parameter with no decimal place present
  expect_true(approx_bssd(y = 2, time = 10, prob_arm1 = 0.5,
   pi1_alpha = 9, pi1_beta = 92, pi2_alpha = 0.3, pi2_beta = 0.3,
   num_samples = 500, seed = 123) >= 0)
})

# Test for parameter prob_arm incorrect probability entered
test_that("Parameter validation catches prob_arm1 errors", {
  expect_error(approx_bssd(y = 10, time = 100, prob_arm1 = 1.9,
   pi1_alpha = 9, pi1_beta = 92, pi2_alpha = 0.3, pi2_beta = 0.3,
   num_samples = 500, seed = 123), "Prob_arm1 must be between zero and one.")
})

# Test for extreme and edge cases
test_that("Function handles edge cases appropriately.", {
  # Edge case: No adverse events
  expect_true(approx_bssd(y = 0, time = 100, prob_arm1 = 0.5,
   pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1,
   num_samples = 1000, seed = 789) >= 0)
  # Edge case: Very high adverse events compared to time
  expect_true(approx_bssd(y = 49, time = 100, prob_arm1 = 0.5,
   pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1,
   num_samples = 1000, seed = 789) <= 1)
  # Test very skewed allocation probabilities
  expect_true(approx_bssd(y = 10, time = 100, prob_arm1 = 0.9,
   pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1,
   num_samples = 1000, seed = 101) <= 1)
})

# Test numerical stability and accuracy for a range of priors
test_that(paste0("Function is stable and accurate for a range ",
                 "of prior distributions."), {
  # Test with very strong prior belief in arm 1 efficacy
  expect_true(approx_bssd(y = 10, time = 100, prob_arm1 = 0.5,
   pi1_alpha = 50, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 50,
   num_samples = 1000, seed = 234) >= 0)
  # Test with very weak prior belief (almost uniform priors)
  expect_true(approx_bssd(y = 10, time = 100, prob_arm1 = 0.5,
   pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1,
   num_samples = 1000, seed = 345) <= 1)
})

# tsk403440_approx-bssd updates to explicitly test all stop() function calls
test_that("Seed parameter validation.", {
  expect_error(approx_bssd(seed = "string", y = 1, time = 1, prob_arm1 = 0.5,
    pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1, num_samples = 10),
               "Seed must be a non-negative integer.")
  expect_error(approx_bssd(seed = -1, y = 1, time = 1, prob_arm1 = 0.5,
    pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1, num_samples = 10),
               "Seed must be a non-negative integer.")
  expect_error(approx_bssd(seed = 1.5, y = 1, time = 1, prob_arm1 = 0.5,
    pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1, num_samples = 10),
               "Seed must be a non-negative integer.")
})

test_that("Y parameter validation", {
  expect_error(approx_bssd(seed = 1, y = "string", time = 1, prob_arm1 = 0.5,
    pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1, num_samples = 10),
               "Y must be non-missing and a non-negative integer.")
  expect_error(approx_bssd(seed = 1, y = -1, time = 1, prob_arm1 = 0.5,
    pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1, num_samples = 10),
               "Y must be non-missing and a non-negative integer.")
  expect_error(approx_bssd(seed = 1, y = NA, time = 1, prob_arm1 = 0.5,
    pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1, num_samples = 10),
               "Y must be non-missing and a non-negative integer.")
})

test_that("Time parameter validation.", {
  expect_error(approx_bssd(seed = 1, y = 1, time = "string", prob_arm1 = 0.5,
    pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1, num_samples = 10),
               "Time must be non-missing and a non-negative rational number.")
  expect_error(approx_bssd(seed = 1, y = 1, time = -1, prob_arm1 = 0.5,
    pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1, num_samples = 10),
               "Time must be non-missing and a non-negative rational number.")
  expect_error(approx_bssd(seed = 1, y = 1, time = NA, prob_arm1 = 0.5,
    pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1, num_samples = 10),
               "Time must be non-missing and a non-negative rational number.")
})

test_that("Prob_arm1 parameter validation.", {
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = "string",
    pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1, num_samples = 10),
               "Prob_arm1 must be between zero and one.")
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = -0.1,
    pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1, num_samples = 10),
               "Prob_arm1 must be between zero and one.")
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 1.1,
    pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1, num_samples = 10),
               "Prob_arm1 must be between zero and one.")
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = NA,
    pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1, num_samples = 10),
               "Prob_arm1 must be between zero and one.")
})

test_that("Pi1_alpha parameter validation.", {
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 0.5,
   pi1_alpha = "string", pi1_beta = 1, pi2_alpha = 1,
   pi2_beta = 1, num_samples = 10),
      "Pi1_alpha must be non-missing and a non-negative rational number.")
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 0.5,
   pi1_alpha = -1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1, num_samples = 10),
      "Pi1_alpha must be non-missing and a non-negative rational number.")
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 0.5,
   pi1_alpha = NA, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1, num_samples = 10),
      "Pi1_alpha must be non-missing and a non-negative rational number.")
})

test_that("Pi1_beta parameter validation.", {
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 0.5,
   pi1_alpha = 1, pi1_beta = "string", pi2_alpha = 1,
   pi2_beta = 1, num_samples = 10),
   "Pi1_beta must be non-missing and a non-negative rational number.")
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 0.5,
    pi1_alpha = 1, pi1_beta = -1, pi2_alpha = 1,
    pi2_beta = 1, num_samples = 10),
    "Pi1_beta must be non-missing and a non-negative rational number.")
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 0.5,
  pi1_alpha = 1, pi1_beta = NA, pi2_alpha = 1, pi2_beta = 1, num_samples = 10),
             "Pi1_beta must be non-missing and a non-negative rational number.")
})

test_that("Pi2_alpha parameter validation.", {
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 0.5,
   pi1_alpha = 1, pi1_beta = 1, pi2_alpha = "string",
   pi2_beta = 1, num_samples = 10),
   "Pi2_alpha must be non-missing and a non-negative rational number.")
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 0.5,
   pi1_alpha = 1, pi1_beta = 1, pi2_alpha = -1, pi2_beta = 1, num_samples = 10),
   "Pi2_alpha must be non-missing and a non-negative rational number.")
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 0.5,
   pi1_alpha = 1, pi1_beta = 1, pi2_alpha = NA, pi2_beta = 1, num_samples = 10),
   "Pi2_alpha must be non-missing and a non-negative rational number.")
})

test_that("Pi2_beta parameter validation.", {
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 0.5,
   pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = "string",
   num_samples = 10),
   "Pi2_beta must be non-missing and a non-negative rational number.")
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 0.5,
   pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = -1, num_samples = 10),
   "Pi2_beta must be non-missing and a non-negative rational number.")
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 0.5,
   pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = NA, num_samples = 10),
   "Pi2_beta must be non-missing and a non-negative rational number.")
})

test_that("Num_samples parameter validation.", {
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 0.5,
   pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1,
   num_samples = "string"),
   "Num_samples must be a non-negative integer.")
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 0.5,
   pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1, num_samples = -1),
   "Num_samples must be a non-negative integer.")
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 0.5,
   pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1, num_samples = 1.5),
   "Num_samples must be a non-negative integer.")
})

test_that("Positive pi parameters validation.", {
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 0.5,
   pi1_alpha = 0, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1, num_samples = 10),
   "Pi1_alpha, pi1_beta, pi2_alpha, and pi2_beta should be strictly positive.")
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 0.5,
   pi1_alpha = 1, pi1_beta = 0, pi2_alpha = 1, pi2_beta = 1, num_samples = 10),
   "Pi1_alpha, pi1_beta, pi2_alpha, and pi2_beta should be strictly positive.")
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 0.5,
   pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 0, pi2_beta = 1, num_samples = 10),
   "Pi1_alpha, pi1_beta, pi2_alpha, and pi2_beta should be strictly positive.")
  expect_error(approx_bssd(seed = 1, y = 1, time = 1, prob_arm1 = 0.5,
   pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 0, num_samples = 10),
   "Pi1_alpha, pi1_beta, pi2_alpha, and pi2_beta should be strictly positive.")
})

test_that("Y parameter not exceeding minimum treatment group.", {
  expect_error(approx_bssd(seed = 1, y = 5, time = 1, prob_arm1 = 0.1,
   pi1_alpha = 1, pi1_beta = 1, pi2_alpha = 1, pi2_beta = 1, num_samples = 10),
   paste0("The number of observed events y, cannot be larger than ",
          "the smallest treatment group.")
  )
})
