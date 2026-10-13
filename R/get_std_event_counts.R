#' Get standardized adverse event counts.
#'
#' @description Get standardized adverse event counts.
#'
#' @details To maintain consistency with other functions that create priors,
#' such as \code{\link{stan_map_summary_data}}, we use the variable names
#' \code{y}, \code{n} and \code{time}. It is recommended to use the total
#' number of events in the trial (one event per subject is counted) and
#' effective sample size (the number of subjects who completed the study plus
#' fractional dropouts) as the number of patients in the trial (\code{n}).
#' The planned study duration of the blinded ongoing study is used for
#' standardization (\code{time_std}).
#'
#' The returned data-frame includes the standardized adverse event counts using
#' the following formula:
#'
#' \eqn{y^* = n * (1 - (1 - (y/n))^{time_{std} / time})}
#'
#' Note also that when any is a vector, \code{y}, \code{n}, \code{time_std},
#' and \code{time} are recycled using default R and tibble recycling
#' behaviors.
#'
#' @param y numeric vector, number of events in the trials.
#' @param n numeric vector, number of patients in the trials (either the total
#' sample size or effective sample size, see Details for more information).
#' @param time numeric vector, strictly positive, time at risk in the trials.
#' @param time_std numeric, the unit of time we standardize to.
#' @param as_data_frame logical, TRUE to get results in a data-frame.
#'
#' @return a data-frame if \code{as_data_frame = TRUE}; else a numeric vector.
#'
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' # In a trial of ten patients, one experienced the event, with follow-up twice
#' # the length of our reference period:
#' get_std_event_counts(y = 1, n = 10, time = 2, time_std = 1)
#'
#' # In a trial of ten patients, one experienced the event, with follow-up half
#' # the length of our reference period:
#' get_std_event_counts(y = 1, n = 10, time = 0.5, time_std = 1)
#'
#' # Those two examples in one call, with data-frame output:
#' get_std_event_counts(y = 1, n = 10, time = c(2, 0.5), time_std = 1,
#'   as_data_frame = TRUE)
get_std_event_counts <- function(y, n, time, time_std = 1,
                                 as_data_frame = FALSE) {

  y_std <- n * (1 - (1 - (y / n))^ (time_std / time))
  if (as_data_frame) {
    tibble(y, n, time, time_std, y_std)
  } else {
    return(y_std)
  }
}
