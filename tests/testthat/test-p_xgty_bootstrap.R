test_that("p_xgty_bootstrap output is as expected.", {

  # create sample data for testing
  set.seed(2024)
  x <- rbeta(n = 1000, shape1 = 0.4, shape2 = 0.6)
  y <- rbeta(n = 1000, shape1 = 0.2, shape2 = 0.7)
  alpha <- 0.05

  # expect output is a tibble
  expect_true(tibble::is_tibble(p_xgty_bootstrap(x, y)))

  # expect tibble has expected 'names' attribute
  expect_equal(attr(p_xgty_bootstrap(x, y), "names"),
               c("mean", "median", "sd", ".lower", ".upper"))

  # expect tibble columns are of class 'numeric'
  expect_equal(c(sapply(p_xgty_bootstrap(x, y), class)[[1]],
                 sapply(p_xgty_bootstrap(x, y), class)[[2]],
                 sapply(p_xgty_bootstrap(x, y), class)[[3]],
                 sapply(p_xgty_bootstrap(x, y), class)[[4]],
                 sapply(p_xgty_bootstrap(x, y), class)[[5]]),
               c("numeric", "numeric", "numeric", "numeric", "numeric"))

  # expect tibble columns are of type 'double'
  expect_equal(c(sapply(p_xgty_bootstrap(x, y), typeof)[[1]],
                 sapply(p_xgty_bootstrap(x, y), typeof)[[2]],
                 sapply(p_xgty_bootstrap(x, y), typeof)[[3]],
                 sapply(p_xgty_bootstrap(x, y), typeof)[[4]],
                 sapply(p_xgty_bootstrap(x, y), typeof)[[5]]),
               c("double", "double", "double", "double", "double"))

  # if alpha is null then tibble dim = 1x3
  # if alpha is not null then tibble dim = 1x5
  if (is.null(alpha)) {
    expect_equal(dim(p_xgty_bootstrap(x, y, alpha = NULL)), c(1, 3))
    } else {
      expect_equal(dim(p_xgty_bootstrap(x, y, alpha = 0.05)), c(1, 5))
    }
})

test_that("p_xgty_boostrap breaks when it should.", {

  # create sample data for testing
  set.seed(2024)
  x <- rbeta(n = 1000, shape1 = 0.4, shape2 = 0.6)
  y <- rbeta(n = 1000, shape1 = 0.2, shape2 = 0.7)
  alpha <- 0.05

  # expect error with mismatched lengths
  expect_error(p_xgty_bootstrap(x, y[-1]),
               "x and y must be the same length")

  # expect error with NA input
  x2 <- x
  x2[5] <- NA
  expect_error(p_xgty_bootstrap(x2, y),
               "Each element of x and y should be non-missing")

  # expect error with alpha > 1
  expect_error(p_xgty_bootstrap(x, y, alpha = 1.05),
               "non-null alpha must be between 0 and 1")

  # expect error with alpha < 0
  expect_error(p_xgty_bootstrap(x, y, alpha = -0.025),
               "non-null alpha must be between 0 and 1")
})
