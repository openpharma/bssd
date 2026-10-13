
# Mimic this function call of an existing bssd funciton that generates a prior ----

#' Fit a Stan MAP prior generation model to summary-level data.
#'
#' @details
#' This routine fits the meta-analytical model equation:
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
#' \deqn{ p_{j} ~ Beta(abN, (1-a)bN)}
#' where N is the total number of patients in the historic trials, and a and b
#' are hyperparameters.
#' To illustrate the role of a and b, consider that the effective
#' sample size of this distribution is \eqn{abN + (1-a)bN) = bN}, i.e. b is a
#' discount to the historic data, taking values on (0, 1).
#' Furthermore, the mean of this distribution is \eqn{ abN / bN = a}.
#' Thus, a is the expected probability of event in unit time, and once again
#' a is bound to the range (0, 1).
#' Hyperpriors \eqn{ a ~ Beta(a_{\alpha}, a_{\beta}) } and
#' \eqn{ b ~ Beta(b_{\alpha}, b_{\beta}) } are natural choices to enforce these
#' ranges.
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
#' @details
#' This model is effectively the same as that fit by
#' \code{\link{stan_map_patient_data}} with the notable exception that since
#' patient-level times-at-risk are not known, the intended trial follow-up
#' times, \eqn{ T_{j} }, are used instead.
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
#' \dontrun{
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
#' }
# stan_map_summary_data <- function(y, n, time,
#                                   a_alpha, a_beta, b_alpha, b_beta,
#                                   ...) {
#
#   # Be vigilant:
#   if(length(n) != length(y)) {
#     stop("n and y must be the same length.")
#   }
#   if(length(n) != length(time)) {
#     stop("n and time must be the same length.")
#   }
#   if(any(y < 0)) {
#     stop("y must be non-negative")
#   }
#   if(any(y > n)) {
#     stop("Elements in y must be no more than the congruent element in n.")
#   }
#   if(any(n < 1)) {
#     stop("Each element of n must be at least 1.")
#   }
#   if(any(time <= 0) | any(is.na(time))) {
#     stop("Each element of time should be strictly positive and non-missing.")
#   }
#   if(any(c(a_alpha, a_beta, b_alpha, b_beta) <= 0) |
#      is.null(a_alpha) | is.null(a_beta) | is.null(b_alpha) | is.null(b_beta)) {
#     stop("a_alpha, a_beta, b_alpha, b_beta should be strictly positive.")
#   }
#
#   N <- sum(n)
#   dat <- list(
#     H = length(n),
#     n = n,
#     y = y,
#     non_events = n - y,
#     time = time,
#     a_alpha = a_alpha,
#     a_beta = a_beta,
#     b_alpha = b_alpha,
#     b_beta = b_beta
#   )
#   fit <- sampling(object = stanmodels$map_prior_summary_data,
#                   data = dat,
#                   ...)
#   l <- bssd_binary_map(fit)
#   class(l) <- c("bssd_binary_map_sl", class(l))
#   l
# }

# Develop chi-squ prior-generating function ----
library(tibble)
get_std_event_counts <- function(y, n, time, time_std = 1,
                                 as_data_frame = FALSE) {

  # To keep this consistent with the functions that creates MAP priors, I use
  # the variable names: y, n and time. Generally, the team use names starting
  # with R, N and T. So:
  # y -> R
  # n -> N
  # time -> T_act
  # similar for the returned variable:
  # y_std -> R_std

  # Note, when any is a vector, y, n, time_std, and time_act are recycled using
  # default R and tibble recycling behaviours.

  y_std <- n * (1 - (1 - (y / n))^(time_std / time))
  if(as_data_frame) {
    tibble(y, n, time, time_std, y_std)
  } else {
    return(y_std)
  }
}
# Examples of that function
# Trial saw 1-in-10 events but followed-up twice as long
testthat::expect_equal(
  get_std_event_counts(y = 1, n = 10, time = 2, time_std = 1),
  0.513, tolerance = 0.001
)
# Trial saw 1-in-10 events but followed-up half as long
testthat::expect_equal(
  get_std_event_counts(y = 1, n = 10, time = 0.5, time_std = 1),
  1.9, tolerance = 0.001
)
# As data.frame:
get_std_event_counts(y = 1, n = 10, time = c(2, 0.5), time_std = 1,
                     as_data_frame = TRUE)

chi_squ_prior <- function(y, n, time, time_std = 1, verbose = FALSE, ...) {

  # Be vigilant:
  if(length(n) != length(y)) {
    stop("n and y must be the same length.")
  }
  if(length(n) != length(time)) {
    stop("n and time must be the same length.")
  }
  if(any(y < 0)) {
    stop("y must be non-negative")
  }
  if(any(y > n)) {
    stop("Elements in y must be no more than the congruent element in n.")
  }
  if(any(n < 1)) {
    stop("Each element of n must be at least 1.")
  }
  if(any(time <= 0) | any(is.na(time))) {
    stop("Each element of time should be strictly positive and non-missing.")
  }

  # Get standardized event counts
  dat_std <- get_std_event_counts(y = y, n = n, time = time,
                                  time_std = time_std, as_data_frame = TRUE)
  y_std <- dat_std$y_std
  include <- rep(TRUE, length(y))
  n_t <- sum(dat_std$n[include])
  y_t <- sum(y_std[include])
  while(sum(include) > 1) {
      n_t <- sum(dat_std$n[include])
      y_t <- sum(y_std[include])
      y_exp <- dat_std$n * (y_t / n_t)
      y_exp[!include] <- NA
      Ci <- (y_std - y_exp)^2 * ( 1 / y_exp + 1 / (dat_std$n - y_exp) )
      C_t <- sum(Ci, na.rm = TRUE)
      M <- sum(include)
      crit <- qchisq(p = 0.01, df = M - 1, lower.tail = FALSE)
      if(C_t > crit) {
        # Homogeneity is rejected. Remove outlier and try again
        if(sum(Ci == max(Ci, na.rm = TRUE), na.rm = TRUE) > 1) {
          # In the case of ties, exclude the study with the highest AE rate.
          # KB ALERT - surely this biases AE rates downwards? Removing the
          # smaller study would make more sense, in my view.
          removal_candidate_i <- seq_along(Ci)[Ci == max(Ci)]
          ae_rates <- y_std / dat_std$n
          remove_i <- removal_candidate_i[
            which.max(ae_rates[removal_candidate_i])]

        } else {
          remove_i <- which.max(Ci)
        }

        if(verbose) print(paste0("Removing study ", remove_i))
        include[remove_i] <- FALSE
        next
      } else {
        # Success
        break
      }
  }

  return(list(alpha = y_t, beta = n_t - y_t))
}

# Tests ----

# Test 1
y <- c(10, 300, 0)
n <- c(95, 1050, 5)
time <- c(1, 3, 0.1)
x <- chi_squ_prior(y = y, n = n, time = time)
testthat::expect_equal(x$alpha, 121.4013, tolerance = 0.001)
testthat::expect_equal(x$beta, 1028.599, tolerance = 0.001)
# OK

# Test 2 - Event 12 on std data
y <- c(39.7830785231592, 45.1027618957793, 73.5365801017635, 5.74639066207418,
       184.673429984089, 189.128443516407, 153.637104497277, 176.397865938221,
       25.1074931650067, 156, 158, 23.52)
n <- c(78.6318681318681, 65.6868131868132, 117.675824175824, 8.68131868131868,
       318.412087912088, 301.475274725275, 226.855263157895, 261.552197802198,
       37.9880952380952, 233.848214285714, 241.113095238095, 43.75)
time <- rep(1, length(y))
z <- get_std_event_counts(y, n, time, as_data_frame = TRUE)
x <- chi_squ_prior(y = y, n = n, time = time)
testthat::expect_equal(x$alpha, 1230.633, tolerance = 0.001)
testthat::expect_equal(x$beta, 705.0369, tolerance = 0.001)

# Test 3 - Marianna's analysis - Event 12
n <- c(78.63, 65.69, 117.68, 8.68, 318.41, 301.48	, 226.86, 261.55, 37.99,
       233.85, 241.11, 43.75)
y <- c(42, 47, 77, 6, 194, 198, 189, 184, 9, 156, 158, 14)
days <- c(rep(364, 6), 532, 364, 84, 336, 336, 168)
z <- get_std_event_counts(y = y, n = n, time = days, as_data_frame = TRUE,
                          time_std = 336)
z
# Marianna's table
y_std_mg <- c(39.78, 45.1, 73.54, 5.75, 184.67, 189.13, 153.64, 176.40, 25.11,
              156.00, 158.00, 23.52)
plot(z$y_std, y_std_mg)
tibble(z$y_std, y_std_mg)
lm(z$y_std ~ y_std_mg)
plot(z$y_std + y_std_mg, z$y_std - y_std_mg, ylim = c(-1, 1))

# MG gets alpha = 1230.64 & beta = 705.04
x <- chi_squ_prior(y = y, n = n, time = days, time_std = 336, verbose = TRUE)
testthat::expect_equal(x$alpha, 1230.64, tolerance = 0.001)
testthat::expect_equal(x$beta, 705.04, tolerance = 0.001)


# test 4 - Marianna's Event 14
n <- c(78.63, 65.69, 117.68, 8.68, 318.41, 301.48	, 226.86, 261.55, 37.99,
       233.85, 241.11, 43.75)
y <- c(12, 6, 22, 1, 76, 64, 84, 104, 8, 58, 76, 6)
days <- c(rep(364, 6), 532, 364, 84, 336, 336, 168)
z <- get_std_event_counts(y = y, n = n, time = days, as_data_frame = TRUE,
                          time_std = 336)
z
x <- chi_squ_prior(y = y, n = n, time = days, time_std = 336, verbose = TRUE)
# Removes studies 9, 8 and 11, as MG's does
testthat::expect_equal(x$alpha, 295.2, tolerance = 0.001)
testthat::expect_equal(x$beta, 1099.83, tolerance = 0.001)
