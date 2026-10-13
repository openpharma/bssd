#' Fit a Stan MAP prior generation model to summary-level data.
#'
#' @description Fit a Stan MAP prior generation model to summary-level data.
#'
#' @details This routine fits the meta-analytical model equation:
#' \deqn{ log(-log(1 - p_{i})) = \alpha_{i} + log(T_{i}) }
#' where \eqn{p_{i}} is the probability of event in trial i, adjusted for the
#' intended or average time-at-risk, \eqn{T_{i}}.
#' The model is equivalent to:
#' \deqn{ log(\lambda_{i}) = \alpha_{i} }
#' where
#' \deqn{ \lambda_{i} = -log(1 - p_{i}) / T_{i} }
#' is the hazard of the event, assumed constant through time, and \eqn{p_{i}} is
#' the probability of event in unit time in trial i.
#' Thus this is a random-effects model that assumes the log-hazard terms are
#' exchangeable across trials, and the \eqn{ \alpha_{i} } terms are
#' trial-specific intercepts.
#' To complete the meta-analytical model, we apply the hierarchical distribution
#' \deqn{ p_{j} \sim Beta(abN, (1-a)bN) }
#' where N is the total number of patients in the historical trials, and a and b
#' are hyperparameters.
#' To illustrate the role of a and b, consider that the effective
#' sample size of this distribution is \eqn{abN + (1-a)bN) = bN}, i.e. b is a
#' discount to the historical data, taking values on (0, 1).
#' Furthermore, the mean of this distribution is \eqn{ abN / bN = a}.
#' Thus, a is the expected probability of event in unit time, and once again
#' a is bound to the range (0, 1).
#' Hyperpriors \eqn{ a \sim Beta(a_{\alpha}, a_{\beta}) } and
#' \eqn{ b \sim Beta(b_{\alpha}, b_{\beta}) } are natural choices to enforce
#' these ranges.
#' This method creates a prior from summary-level data, requiring only the
#' intended or average times-at-risk and total counts of patients with an
#' adverse event in each trial. It is likely aggregate statistics like this can
#' be derived from publications and/or filings. However, if you have
#' patient-level times-at-risk and adverse event statuses,
#' \code{\link{stan_map_patient_data}} provides a similar method to create a
#' prior from patient-level data. The patient-level approach is generally
#' preferable because it allows adjustment for how long each individual patient
#' spent at risk.
#'
#' This model is effectively the same as that fit by
#' \code{\link{stan_map_patient_data}} with the notable exception that since
#' patient-level times-at-risk are not known, the intended trial follow-up
#' times, \eqn{ T_{j} }, are used instead.
#'
#' Note: setting a seed in R does not affect the state of the
#' random generator in stan. If a reproducible output is desired, then the
#' optional parameter \code{seed} needs to be populated. For additional
#' information about this parameter, see \code{\link[rstan]{sampling}}.
#'
#' @inheritParams stan_map_patient_data
#' @param y integer vector, number of events in the trials.
#' @param n integer vector, number of patients in the trials.
#' @param time, numeric vector, strictly positive, time at risk in the trials.
#' @param ... extra parameters are passed to \code{\link[rstan]{sampling}}.
#'
#' @return object of type \code{\link{bssd_binary_map}}.
#'
#' @importFrom rstan sampling
#' @importFrom stats rbeta
#'
#' @export
#'
#' @examples
#' set.seed(2021)
#' H <- 10
#' time <- 0.5 + cumsum(rexp(H, 5))
#' # Use constant hazard model:
#' hazard <- 0.15
#' prob <- 1 - exp(- hazard * time)
#' n <- rpois(H, lambda = 100 * time)
#' y <- rbinom(H, size = n, prob = prob)
#' x <- stan_map_summary_data(y = y, n = n, time = time,
#'                            a_alpha = 1 / 3, a_beta = 1 / 3,
#'                            b_alpha = 5, b_beta = 5)
#' x
stan_map_summary_data <- function(y, n, time,
                                  a_alpha, a_beta, b_alpha, b_beta,
                                  ...) {

  # Test data conformity.
  if (any(y < 0) | any(is.na(y))) {
    stop("y must be non-negative and non-missing.")
  }
  if (any(n < 1) | any(is.na(n))) {
    stop("Each element of n must be at least 1 and non-missing.")
  }
  if (any(time <= 0) | any(is.na(time))) {
    stop("Each element of time should be strictly positive and non-missing.")
  }
  if (any(c(a_alpha, a_beta, b_alpha, b_beta) <= 0) |
     any(is.na(c(a_alpha, a_beta, b_alpha, b_beta))) |
     is.null(a_alpha) | is.null(a_beta) | is.null(b_alpha) | is.null(b_beta)) {
    stop(paste0("a_alpha, a_beta, b_alpha, b_beta should be ",
                "strictly positive and non-missing."))
  }

  # Comparing lengths for n, y and time.
  yn <- length(y) == length(n)
  yt <- length(y) == length(time)
  tn <- length(time) == length(n)

  if (yn == FALSE && yt == FALSE) {
    stop("y must be the same length as n and time.")
  }
  if (yn == FALSE && tn == FALSE) {
    stop("n must be the same length as y and time.")
  }
  if (yt == FALSE && tn == FALSE) {
    stop("time must be the same length as y and n.")
  }

  # Testing that the number of events reported in a study is equal to or
  # less than the number of patients in this study.
  if (any(y > n)) {
    stop(paste0("Each element of y must equal to or less than ",
                "the corresponding element in n."))
  }

  # Function
  N <- sum(n)
  dat <- list(
    H = length(n),
    n = n,
    y = y,
    non_events = n - y,
    time = time,
    a_alpha = a_alpha,
    a_beta = a_beta,
    b_alpha = b_alpha,
    b_beta = b_beta
  )
  fit <- sampling(object = stanmodels$map_prior_summary_data,
                  data = dat,
                  ...)
  l <- bssd_binary_map(fit)
  class(l) <- c("bssd_binary_map_sl", class(l))
  l
}
