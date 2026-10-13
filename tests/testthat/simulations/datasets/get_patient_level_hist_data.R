#' Get simulated patient-level historical trial data
#'
#' @details Recruitment is assumed to occur at a constant rate, i.e.
#' intra-patient arrival times follow an exponential distribution, with expected
#' time to recruit specified by the user.
#'
#' @param trial_data data-frame with columns:
#'   * trial_id, containing trial identifiers;
#'   * sample_size, containing integer sample sizes of the arm of interest;
#'   * follow_up_time, expected duration of patient follow-up;
#'   * lambda, hazard parameters that prevail in the trials.
#' @param get_prob_func Function like \code{\link{get_prob_constant_hazard}}
#' that takes as arguments time and lambda, and returns the probability of
#' event.
#' @param get_time_func Function like \code{\link{complete_follow_up}} that
#' takes as arguments sample_size and follow_up_time and returns the a vector
#' of follow-up times for the patients.
#' @param dp integer, number of decimal places to use for numeric values.
#'
#' @return data-frame with columns:
#'   * trial_id, identifying trials;
#'   * patient_id, identifying patients;
#'   * time, containing patient-level follow-up time;
#'   * prob, containing patient-level probability of event;
#'   * y, containing binary event indicators.
#'
#' @importFrom dplyr %>% tibble
#' @importFrom purrr pmap reduce
#' @export
#'
#' @examples
#' trial_data <- tibble(
#'   trial_id = 1:3,
#'   sample_size = c(50, 50, 50),
#'   follow_up_time = c(1, 1, 2),
#'   lambda = c(0.2, 0.19, 0.21)
#' )
#' get_time_func <- complete_follow_up
#' get_prob_func = get_prob_constant_hazard
#' df <- get_patient_level_hist_data(trial_data, get_prob_func, get_time_func)
get_patient_level_hist_data <- function(trial_data,
                                        get_prob_func,
                                        get_time_func = NULL,
                                        dp = 3) {

  if (is.null(get_time_func)) get_time_func <- complete_follow_up

  pmap(
    .l = trial_data,
    .f = function(trial_id, sample_size, follow_up_time, lambda, ...) {

      # Get follow-up times
      time <- get_time_func(sample_size, follow_up_time)
      # Prob(event) is a function of follow-up time:
      prob <- get_prob_func(time, lambda)
      # Binary events:
      y <- rbinom(sample_size, size = 1, prob = prob)
      tibble(
        trial_id = as.integer(trial_id),
        patient_id = as.integer(1:sample_size),
        time,
        prob,
        y = as.integer(y)
      )
    }
  ) %>%
    reduce(bind_rows) %>%
    round(dp)
}
