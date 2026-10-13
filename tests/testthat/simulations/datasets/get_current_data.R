#' Get simulated patient-level current trial data.
#'
#' @details Note that although arm is returned, the goal of a blinded assessment
#'   is to make useful inference without using the arm variable. Arm is returned
#'   to facilitate benchmarking analyses, i.e. how well could you have done.
#'
#' @inheritParams get_patient_level_hist_data
#' @param n_control integer, sample size in placebo arm. Must be > 0.
#' @param n_active integer, sample size in active arm. Must be > 0.
#' @param follow_up_time numeric, expected length of follow-up time.
#' @param lambda_control numeric, value for risk parameter lambda for placebo
#'   patients.
#' @param lambda_active numeric, value for risk parameter lambda for active
#'   patients.
#' @param get_prob_func_active Function like
#'   \code{\link{get_prob_constant_hazard}} that takes as arguments time and
#'   lambda, and returns the probability of event for patients only in the
#'   active arm. If NULL (by default), then get_prob_func is used for both
#'   placebo and active patients.
#'
#' @return data-frame with columns:
#'   * arm, identifying arm;
#'   * time, containing patient-level follow-up time;
#'   * prob, containing patient-level probability of event;
#'   * y, containing binary event indicators.
#'
#' @importFrom stats rbinom
#' @importFrom dplyr tibble %>%
#' @export
#'
#' @examples
#' get_current_data(n_control = 25, n_active = 25, recruitment_length = 0.8,
#'                  lambda_control = 0.25, lambda_active = 0.33,
#'                  get_prob_func = get_prob_constant_hazard)
get_current_data <- function(n_control,
                             n_active,
                             follow_up_time,
                             lambda_control,
                             lambda_active,
                             get_prob_func,
                             get_prob_func_active = NULL,
                             get_time_func = NULL,
                             min_time = 0.001,
                             dp = 3) {

  if (n_control <= 0 | n_active <= 0) {
    stop("Require n_control > 0 and n_active > 0.")
  }
  if (is.null(get_prob_func_active)) get_prob_func_active <- get_prob_func
  if (is.null(get_time_func)) get_time_func <- complete_follow_up

  n <- n_control + n_active
  time <- get_time_func(n, follow_up_time)
  arm <- sample(x = c(rep(0, n_control), rep(1, n_active)),
                size = n,
                replace = FALSE)

  # Prob(event) is a function of follow-up time and arm:
  prob <- numeric(length = n)
  prob[arm == 0] <- get_prob_func(time[arm == 0], lambda_control)
  prob[arm == 1] <- get_prob_func_active(time[arm == 1], lambda_active)

  # Binary events:
  y <- rbinom(n = n, size = 1, prob = prob)

  tibble(arm, time, prob, y) %>% round(dp)
}
