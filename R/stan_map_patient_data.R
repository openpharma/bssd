#' Fit a Stan MAP prior generation model to historical patient-level data.
#'
#' @description Fit a Stan MAP prior generation model to historical
#' patient-level data.
#'
#' @details This routine fits the meta-analytical model equation:
#' \deqn{ log(-log(1 - p_{ij})) = \alpha_{j} + log(T_{ij})  }
#' where \eqn{p_{ij}} is the probability of event in trial j, adjusted for the
#' time-at-risk of patient i, \eqn{T_{ij}}.
#' The model is equivalent to:
#' \deqn{ log(\lambda_{ij}) = \alpha_{j} }
#' where
#' \deqn{ \lambda_{ij} = -log(1 - p_{j}) / T_{ij} }
#' is the hazard of the event, assumed constant through time, and \eqn{p_{j}} is
#' the probability of event in unit time in trial j.
#' Thus this is a random-effects model that assumes the log-hazard terms are
#' exchangeable across trials, and the \eqn{ \alpha_{j} } terms are
#' trial-specific intercepts.
#' To complete the meta-analytical model, we apply the hierarchical distribution
#' \deqn{ p_{j} \sim Beta(abN, (1-a)bN)}
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
#' If you do not have patient-level times-at-risk and adverse event statuses,
#' \code{\link{stan_map_summary_data}} provides a similar method to create a
#' prior from summary-level data, requiring only the intended or average
#' times-at-risk and total counts of patients with the event in each trial. It
#' is likely aggregate statistics like this can be derived from publications
#' and/or filings. The patient-level approach is generally preferable, however,
#' because it allows adjustment for how long each individual patient spent at
#' risk.
#'
#' Note: setting a seed in R does not affect the state of the
#' random generator in stan. If a reproducible output is desired, then the
#' optional parameter \code{seed} needs to be populated. For additional
#' information about this parameter, see \code{\link[rstan]{sampling}}.
#'
#' @param y integer vector, 0 for no event, 1 for event, for all patients.
#' @param trial integer vector, denoting which trials patients belong to.
#' @param time numeric vector, strictly positive, times at risk for patients.
#' @param a_alpha numeric, first shape param of beta prior on a. See Details.
#' @param a_beta numeric, second shape param of beta prior on a. See Details.
#' @param b_alpha numeric, first shape param of beta prior on b. See Details.
#' @param b_beta numeric, second shape param of beta prior on b. See Details.
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
#' N <- 100
#' time <- 0.5 + cumsum(rexp(N, 100))
#' # Use constant hazard model:
#' hazard <- 0.15
#' prob <- 1 - exp(- hazard * time)
#' y <- rbinom(N, size = 1, prob = prob)
#' trial <- sample(1:5, size = N, replace = TRUE)
#' x <- stan_map_patient_data(y = y, trial = trial, time = time,
#'                            a_alpha = 1 / 3, a_beta = 1 / 3,
#'                            b_alpha = 5, b_beta = 5)
#' x
stan_map_patient_data <- function(y, trial, time,
                                  a_alpha, a_beta, b_alpha, b_beta,
                                  ...) {

  # Correct data
  if (!all(y %in% 0:1) | any(is.na(y))) {
    stop("y should be a vector of 0s and 1s only, and cannot be missing.")
  }

  if (any(is.na(trial))) {
    stop("Each element of trial should be non-missing.")
  }
  if (!inherits(trial, "integer") | any(trial < 1)) {
    stop("Each element of trial should be a positive integer.")
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

  # Lengths
  yn <- length(y) == length(trial)
  yt <- length(y) == length(time)
  tn <- length(time) == length(trial)

  if (yn == FALSE && yt == FALSE) {
    stop("y must be the same length as trial and time.")
  }
  if (yn == FALSE && tn == FALSE) {
    stop("trial must be the same length as y and time.")
  }
  if (yt == FALSE && tn == FALSE) {
    stop("time must be the same length as y and trial.")
  }

  H <- length(unique(trial))
  N <- length(y)
  dat <- list(
    N = N,
    H = H,
    y = y,
    trial = trial,
    time = time,
    a_alpha = a_alpha,
    a_beta = a_beta,
    b_alpha = b_alpha,
    b_beta = b_beta
  )
  fit <- sampling(object = stanmodels$map_prior_patient_data,
                  data = dat,
                  ...)
  l <- bssd_binary_map(fit)
  class(l) <- c("bssd_binary_map_pl", class(l))
  l
}
