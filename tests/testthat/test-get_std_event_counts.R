test_that("get_std_event_counts works", {

  # Example 1 - trial saw 1-in-10 events and followed-up twice as long:
  testthat::expect_equal(
    get_std_event_counts(y = 1, n = 10, time = 2, time_std = 1),
    0.513, tolerance = 0.001
  )

  # Example 2 - trial saw 1-in-10 events and followed-up half as long:
  testthat::expect_equal(
    get_std_event_counts(y = 1, n = 10, time = 0.5, time_std = 1),
    1.9, tolerance = 0.001
  )

  # Example 3 - above examples as data.frame:
  x <- get_std_event_counts(y = 1, n = 10, time = c(2, 0.5), time_std = 1,
                            as_data_frame = TRUE)
  expect_true(is.data.frame(x))
  expect_equal(x$y, c(1, 1))
  expect_equal(x$n, c(10, 10))
  expect_equal(x$time, c(2, 0.5))
  expect_equal(x$time_std, c(1, 1))
  expect_equal(x$y_std, c(0.513, 1.9), tolerance = 0.001)
})


# The following two examples create sample study data and compute
# standardized AE counts using a standardization formula.
# This AE count is compared with the AE count computed with
# the get_std_event_counts() function in the BSSD package.
# Results are compared using testthat.

## Example 1
# Create sample study data
# STDFUTIME is the standardized study follow up time
# y is the number of event in the trials
# n is the (effective) number of patients in the trials
ex1 <- tibble::tibble(STUDYID = 1:4,
                      STDFUTIME = c(1, 1, 1, 2),
                      y = c(30, 25, 15, 30),
                      neff = c(100, 50, 50, 100))

# Standard study duration
T_std <- 1
# Compute standardized AE counts using the standardization formula
ex1 <- ex1 %>%
  dplyr::mutate(
    y_std_formula = neff * (1 - (1 - (y / neff))^ ((T_std) / STDFUTIME)))

# Compute standardized AE counts using the bssd function
# (not needed for chi_squ_prior())
y_std_ex1 <- get_std_event_counts(y = ex1 %>% dplyr::pull(y),
                                  n = ex1 %>% dplyr::pull(neff),
                                  time = ex1 %>% dplyr::pull(STDFUTIME),
                                  time_std = 1)

# Add standardized AE counts to ex1 for comparison
ex1 <- dplyr::bind_cols(ex1, y_std_bssd = y_std_ex1)

## Example 2
# Create sample study data
# STDFUTIME is the standardized study follow up time
# y is the number of event in the trials
# n is the (effective) number of patients in the trials
ex2 <- tibble::tibble(STUDYID = 1:4,
                      STDFUTIME = c(1, 1, 1, 1),
                      y = c(10, 40, 100, 100),
                      neff = c(200, 200, 2000, 2600))

# Standard study duration
T_std <- 1

# Compute standardized AE counts using the standardization formula
ex2 <- ex2 %>%
  dplyr::mutate(
    y_std_formula = neff * (1 - (1 - (y / neff))^ ((T_std) / STDFUTIME)))

# Compute standardized AE counts using the bssd function
y_std_ex2 <- get_std_event_counts(y = ex2 %>% dplyr::pull(y),
                                  n = ex2 %>% dplyr::pull(neff),
                                  time = ex2 %>% dplyr::pull(STDFUTIME),
                                  time_std = 1)

# Add standardized AE counts to ex2 for comparison
ex2 <- dplyr::bind_cols(ex2, y_std_bssd = y_std_ex2)

## Create list of examples
examples <- list("Example1" = ex1, "Example2" = ex2)

## Compare results obtained with formula vs get_std_event_counts() output
## using test_that()
test_fun <- function(id, data, tol=0) {
  return_val <- try(
    test_that(paste0("ScenarioID ", id, ": Comparison of standardized AE ",
                     "counts (formula) vs get_std_event_counts() output"), {

      actual <- data %>%
        dplyr::select(y_std_bssd)

      expected <- data %>%
        dplyr::select(y_std_formula)

      expect_equal(dplyr::pull(actual), dplyr::pull(expected),  tolerance = tol)

      }))

  if (class(return_val) == "try-error") {
    res <- "Test failed"
    } else {
      res <- "Test passed"
    }
  return(res)
}

# Apply the testing function above to the the two examples created earlier
sapply(c("Example1", "Example2"), function(id) {
  test_fun(id = id, data = examples[[id]])
  })
