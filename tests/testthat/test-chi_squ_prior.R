test_that("chi_squ_prior breaks when it should.", {

  # create sample data for testing
  y <- c(1, 0, 5)
  n <- c(10, 5, 55)
  time <- c(1, 0.5, 1.2)

  # expect error if y and n lengths do not match
  expect_error(
    chi_squ_prior(
      y = y,
      n = n[-1],
      time = time
    ), "n and y must be the same length")

  # expect error if n and time lengths do not match
  expect_error(
    chi_squ_prior(
      y = y,
      n = n,
      time = time[-1]
    ), "n and time must be the same length")

  # expect error if y is negative
  expect_error({
    y2 <- y
    y2[2] <- -1
    chi_squ_prior(
      y = y2,
      n = n,
      time = time
    )}, "y must be non-negative")

  # expect error if y is NA
  expect_error({
    y2 <- y
    y2[2] <- NA
    chi_squ_prior(
      y = y2,
      n = n,
      time = time
    )}, "n and y should be non-missing")

  # expect error if y > n
  expect_error({
    y2 <- y
    y2[2] <- n[2] + 1
    chi_squ_prior(
      y = y2,
      n = n,
      time = time
    )}, "Elements in y must be no more than the congruent element in n")

  # expect error if n is NA
  expect_error({
    n2 <- n
    n2[2] <- NA
    chi_squ_prior(
      y = y,
      n = n2,
      time = time
    )}, "n and y should be non-missing")

  expect_error({
    n2 <- n
    n2[2] <- 0
    chi_squ_prior(
      y = y,
      n = n2,
      time = time
    )}, "Each element of n must be at least 1")

  # expect error if y > n
  expect_error({
    n2 <- n
    n2[2] <- -1
    chi_squ_prior(
      y = y,
      n = n2,
      time = time
    )}, "Elements in y must be no more than the congruent element in n")

  # expect error if time is NA
  expect_error({
    time2 <- time
    time2[2] <- NA
    chi_squ_prior(
      y = y,
      n = n,
      time = time2
    )}, "Each element of time should be strictly positive and non-missing")

  # expect error if time is not positive
  expect_error({
    time2 <- time
    time2[2] <- 0
    chi_squ_prior(
      y = y,
      n = n,
      time = time2
    )}, "Each element of time should be strictly positive and non-missing")

  # expect error if time is not positive
  expect_error({
    time2 <- time
    time2[2] <- -8
    chi_squ_prior(
      y = y,
      n = n,
      time = time2
    )}, "Each element of time should be strictly positive and non-missing")

})

test_that("chi_squ_prior works", {

  # Test 1
  y <- c(10, 300, 0)
  n <- c(95, 1050, 5)
  time <- c(1, 3, 0.1)
  x <- chi_squ_prior(y = y, n = n, time = time)
  testthat::expect_equal(x$alpha, 121.4013, tolerance = 0.001)
  testthat::expect_equal(x$beta, 1028.599, tolerance = 0.001)

  # Test 2 - no heterogeneity
  y <- c(10, 100, 5)
  n <- c(100, 1000, 50)
  time <- c(1, 1, 1)
  x <- chi_squ_prior(y = y, n = n, time = time)
  testthat::expect_equal(x$alpha, sum(y))
  testthat::expect_equal(x$beta, sum(n) - sum(y))

  # Test 3 - blatant outlier
  y <- c(10, 100, 5, 999)
  n <- c(100, 1000, 50, 1000)
  time <- c(1, 1, 1, 3)
  x <- chi_squ_prior(y = y, n = n, time = time)
  testthat::expect_equal(x$alpha, sum(y[1:3]))
  testthat::expect_equal(x$beta, sum(n[1:3]) - sum(y[1:3]))
  testthat::expect_false(x$alpha == sum(y))
  testthat::expect_false(x$beta == sum(n) - sum(y))


})



# The following two examples create sample study data and chi-squared
# parameters are computed manually from the data. These manually computed
# values are compared with the values computed with chi_squ_prior() function
# in the BSSD package. Results are compared using testthat.

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
    y_std_formula = neff * (1 - (1 - (y / neff)) ^ ((T_std) / STDFUTIME)))


## Example 2
# Create sample study data
# STDFUTIME is the standardized study follow up time
# y is the number of event in the trials
# n is the (effective) number of patients in the trials
ex2 <- tibble::tibble(STUDYID = 1:4,
                      STDFUTIME = c(1, 1, 1, 1),
                      y = c(10, 40, 100, 100),
                      neff = c(200, 200, 2000, 2600))

## Standard study duration
T_std <- 1

## Compute standardized AE counts using the standardization formula
ex2 <- ex2 %>%
  dplyr::mutate(
    y_std_formula = neff * (1 - (1 - (y / neff)) ^ ((T_std) / STDFUTIME)))

## Create list of examples
examples <- list("Example1" = ex1, "Example2" = ex2)

## Create a function to manually compute chi-squared parameters for
## validation purposes
chisq_fun <- function(id, data, alpha = 0.01) {
  mat <- data %>%
    dplyr::mutate(NoAE = neff - y_std_formula) %>%
    dplyr::select(y_std_formula, NoAE)
  chisq_res <- chisq.test(mat)
  while (chisq_res$p.value <= alpha) {
    chi_comp <- (chisq_res$observed - chisq_res$expected)^2 / chisq_res$expected
    chi_comp <- cbind(chi_comp, "Total" = rowSums(chi_comp))
    ind <- which.max(chi_comp[, "Total"])
    mat <- mat[-ind, ]
    chisq_res <- chisq.test(mat)
  }
  res <- colSums(mat)
  names(res) <- c("alpha", "beta")
  return(res)
  }


## Apply BSSD chi_squ_prior function to obtain alpha and beta values for
## each example
chi_res_bssd_ex <- sapply(c("Example1", "Example2"), function(id) {
  data <- examples[[id]]
  chi_squ_prior(y = data %>% dplyr::pull(y),
                n = data %>% dplyr::pull(neff),
                time = data %>% dplyr::pull(STDFUTIME),
                time_std = 1,
                alpha = 0.01)
  })


## Apply validation chisq_fun function to obtain alpha and beta values for
## each example
chi_res_ex <- sapply(c("Example1", "Example2"), function(id) {
  chisq_fun(id = id, data = examples[[id]])
})

## Create a function to compare parameters obtained with basic chisq.test()
## vs chi_squ_prior output using test_that()
test_fun <- function(id, data_act, data_exp, tol = 0.01) {
  return_val <- try(
    test_that(
      paste0(
        "ScenarioID ",
        id,
        ": Comparison of chi-square parameters (manual) ",
        "vs chi_squ_prior() output"), {

          actual <- unlist(data_act[, id])

          expected <- data_exp[, id]

          expect_equal(actual, expected, tolerance = tol)

        }
      )
    )

if (class(return_val) == "try-error") {
  res <- "Test failed"
} else {
  res <- "Test passed"
}
return(res)
}


## Apply the comparison function to each example
sapply(c("Example1", "Example2"), function(id) {
  test_fun(id = id, data_act = chi_res_bssd_ex, data_exp = chi_res_ex)
})
