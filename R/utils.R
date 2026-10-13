#' Turn on long-running internal tests
#'
#' This function enables long or computationally expensive tests by
#' setting the environment variable `RUNALLTESTS` to `"TRUE"`.
#'
#' @return Invisibly returns TRUE
#' @keywords internal
#' @noRd
turn_on_long_tests <- function() {
  Sys.setenv(RUNALLTESTS = "TRUE")
  invisible(TRUE)
}

#' Turn off long-running internal tests
#'
#' This function disables long or computationally expensive tests by
#' setting the environment variable `RUNALLTESTS` to `"FALSE"`.
#'
#' @return Invisibly returns FALSE
#' @keywords internal
#' @noRd
turn_off_long_tests <- function() {
  Sys.setenv(RUNALLTESTS = "FALSE")
  invisible(FALSE)
}
