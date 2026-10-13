test_that("turn_off_long_tests sets the environment variable", {
  turn_off_long_tests()
  expect_equal(Sys.getenv("RUNALLTESTS"), "FALSE")
})

test_that("turn_on_long_tests sets the environment variable", {
  turn_on_long_tests()
  expect_equal(Sys.getenv("RUNALLTESTS"), "TRUE")
  turn_off_long_tests()
})

