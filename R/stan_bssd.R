#' Fit a Bayesian safety signal detection analysis for a two-arm RCT using Stan.
#'
#' @description If you know the treatment allocations, provide those as a vector
#' of 1s and 2s via the \code{tmt} argument. If you do not know the treatment
#' allocations (i.e. the trial remains blinded), specify the \code{prob_arm1}
#' argument. Exactly one of \code{tmt} and \code{prob_arm1} must be specified.
#' Note: setting a seed in R does not affect the state of the random generator
#' in stan. If a reproducible output is desired, then the optional parameter
#' \code{seed} needs to be populated. For additional information about this
#' parameter, see \code{\link[rstan]{sampling}}.
#'
#' @param y integer vector, 0 for no event, 1 for event, for all patients.
#' @param time numeric vector, strictly positive, time covariate.
#' @param tmt integer vector, 1 for placebo (arm1), 2 for active (arm2), for
#' all patients. Specify this for an unblinded analysis (i.e. \code{prob_arm1}
#' is NULL).
#' @param prob_arm1 numeric, probability of being randomized to placebo (arm1).
#' Specify this for a blinded analysis (i.e. \code{tmt} is NULL).
#' @param w1 numeric vector, weights of constituent beta distributions in
#' mixture prior on unit-time event probability in placebo (arm1). Should sum
#' to 1.
#' @param pi1_alpha numeric vector, first shape params of constituent beta
#' distributions in mixture prior on unit-time event probability in placebo
#' (arm1).
#' @param pi1_beta numeric vector, second shape params of constituent beta
#' distributions in mixture prior on unit-time event probability in placebo
#' (arm1).
#' @param w2 numeric vector, weights of constituent beta distributions in
#' mixture prior on unit-time event probability in active (arm2). Should sum
#' to 1.
#' @param pi2_alpha numeric vector, first shape params of constituent beta
#' distributions in mixture prior on unit-time event probability in active
#' (arm2).
#' @param pi2_beta numeric vector, second shape params of constituent beta
#' distributions in mixture prior on unit-time event probability in active
#' (arm2).
#' @param ... extra parameters are passed to \code{\link[rstan]{sampling}}.
#'
#' @return object of type \code{\link{bssd_binary}}.
#'
#' @importFrom rstan sampling
#'
#' @export
#'
#' @examples
#' # Create prior
#' set.seed(2021)
#' H <- 10
#' time_0 <- 0.5 + cumsum(rexp(H, 5))
#' # Use constant hazard model:
#' hazard <- 0.15
#' prob_0 <- 1 - exp(- hazard * time_0)
#' n <- rpois(H, lambda = 100 * time_0)
#' y <- rbinom(H, size = n, prob = prob_0)
#' x <- stan_map_summary_data(y = y, n = n, time = time_0,
#'                            a_alpha = 1 / 3, a_beta = 1 / 3,
#'                            b_alpha = 5, b_beta = 5)
#'
#' # Use prior in blinded BSSD analysis
#' prob_arm1 <- 0.333
#' N <- 100
#' time <- 0.5 + cumsum(rexp(N, 100))
#' tmt <- sample(1:2, size = N, replace = TRUE,
#'               prob = c(prob_arm1, 1 - prob_arm1))
#' hazard1 <- 0.15
#' hazard2 <- 0.25
#' prob <- numeric(length = N)
#' prob[tmt == 1] <- 1 - exp(- hazard1 * time[tmt == 1])
#' prob[tmt == 2] <- 1 - exp(- hazard2 * time[tmt == 2])
#' y <- rbinom(N, size = 1, prob = prob)
#' mix <- fit_mixture(x)
#' w1 <- mix["w",]
#' pi1_alpha <- mix["a",]
#' pi1_beta <- mix["b",]
#' w2 <- as.array(c(1))
#' pi2_alpha <- as.array(c(0.3))
#' pi2_beta <- as.array(c(0.3))
#' fit1 <- stan_bssd(y = y, time = time, prob_arm1 = prob_arm1,
#'                   w1 = w1, pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
#'                   w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta)
#' fit1
#'
#' # Use the same prior in an unblinded BSSD analysis
#' fit2 <- stan_bssd(y = y, time = time, tmt = tmt,
#'                   w1 = w1, pi1_alpha = pi1_alpha, pi1_beta = pi1_beta,
#'                   w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta)
#' fit2
stan_bssd <- function(y, time, tmt = NULL, prob_arm1 = NULL,
                      w1 = c(1), pi1_alpha = c(1), pi1_beta = c(1),
                      w2 = c(1), pi2_alpha = c(1), pi2_beta = c(1),
                      ...) {


  # Correct data
  if (!all(y %in% 0:1)) {
    stop("y should be a vector of 0s and 1s only.")
  }
  if (any(time <= 0) | any(is.na(time))) {
    stop("Each element of time should be strictly positive and non-missing.")
  }

  if (any(w1 <= 0) | any(pi1_alpha <= 0) | any(pi1_beta <= 0) |
      any(w2 <= 0) | any(pi2_alpha <= 0) | any(pi2_beta <= 0) |
      any(is.na(w1)) | any(is.na(pi1_alpha)) | any(is.na(pi1_beta)) |
      any(is.na(w2)) | any(is.na(pi2_alpha)) | any(is.na(pi2_beta)) |
      is.null(w1) | is.null(pi1_alpha) | is.null(pi1_beta) |
      is.null(w2) | is.null(pi2_alpha) | is.null(pi2_beta)) {
    stop(paste0("w1, pi1_alpha, pi1_beta, w2, pi2_alpha, and pi2_beta should ",
                "be strictly positive and non-missing."))
  }


  ### Correct lengths
  wa1 <- length(w1) == length(pi1_alpha)
  wb1 <- length(w1) == length(pi1_beta)
  ab1 <- length(pi1_alpha) == length(pi1_beta)

  if (wa1 == FALSE && wb1 == FALSE) {
    stop("w1 must be the same length as pi1_alpha and pi1_beta")
  }
  if (wa1 == FALSE && ab1 == FALSE) {
    stop("pi1_alpha must be the same length as w1 and pi1_beta")
  }
  if (wb1 == FALSE && ab1 == FALSE) {
    stop("pi1_beta must be the same length as w1 and pi1_alpha")
  }

  wa2 <- length(w2) == length(pi2_alpha)
  wb2 <- length(w2) == length(pi2_beta)
  ab2 <- length(pi2_alpha) == length(pi2_beta)

  if (wa2 == FALSE && wb2 == FALSE) {
    stop("w2 must be the same length as pi2_alpha and pi2_beta")
  }
  if (wa2 == FALSE && ab2 == FALSE) {
    stop("pi2_alpha must be the same length as w2 and pi2_beta")
  }
  if (wb2 == FALSE && ab2 == FALSE) {
    stop("pi2_beta must be the same length as w2 and pi2_alpha")
  }

  ### tmt or prob_arm1 - only one should exist
  if (is.null(tmt) & is.null(prob_arm1)) {
    stop("Specify tmt or prob_arm1")
  }

  if (!is.null(tmt) & !is.null(prob_arm1)) {
    stop("Specify tmt or prob_arm1 but not both")
  }

  # tmt
  if (!is.null(tmt)  & is.null(prob_arm1)) {
      # correct data
      if (!all(tmt %in% 1:2)) {
        stop("tmt should be a vector of 1s and 2s only.")
      }
      # lengths
      ytime <- length(y) == length(time)
      ytmt <- length(y) == length(tmt)
      timetmt <- length(time) == length(tmt)

      if (ytime == FALSE && ytmt == FALSE) {
        stop("y must be the same length as time and tmt.")
      }
      if (ytime == FALSE && timetmt == FALSE) {
        stop("time must be the same length as y and tmt.")
      }
      if (ytmt == FALSE && timetmt == FALSE) {
        stop("tmt must be the same length as y and time")
      }
  }
  # prob_arm1
  if (is.null(tmt)  & !is.null(prob_arm1)) {
     if (prob_arm1 <= 0 | prob_arm1 > 1) {
       stop("prob_arm1 must be > 0 and <= 1")
     }
     ytime <- length(y) == length(time)
     if (ytime == FALSE) {
       stop("y and time must be the same length.")
     }
  }

  N <- length(y)
  if (!is.null(tmt)) tmt <- as.array(tmt)
  dat <- list(
    N = N,
    y = as.array(y),
    time = as.array(time),
    tmt = tmt,
    prob_arm1 = prob_arm1,
    K1 = length(w1),
    w1 = as.array(w1),
    pi1_alpha = as.array(pi1_alpha),
    pi1_beta = as.array(pi1_beta),
    K2 = length(w2),
    w2 = as.array(w2),
    pi2_alpha = as.array(pi2_alpha),
    pi2_beta = as.array(pi2_beta)
  )
  if (is.null(tmt)) {
    # Blinded analysis
    fit <- sampling(object = stanmodels$bssd,
                    data = dat,
                    ...)
  } else {
    # Unblinded analysis
    fit <- sampling(object = stanmodels$bssd_unblinded,
                    data = dat,
                    ...)
  }

  return(bssd_binary(y, time, fit))
 }
