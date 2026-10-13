#' Derive a beta prior for an AE from an homogeneous set of historical studies.
#'
#' @description The historical studies are assumed to be heterogeneous. That
#' assumption is tested using sequential chi-squared tests. If homogeneity is
#' detected, the largest outlier study is removed from the historical set and
#' the test rerun. This cycle continues until: i) heterogeneity is not
#' rejected; or ii) there is only one historical study remaining.
#'
#' @details It is recommended to use the total number of events in the trial
#' (one event per subject is counted) and effective sample size (the number of
#' subjects who completed the study plus fractional dropouts) as the number of
#' patients in the trial (\code{n}). The planned study duration of the blinded
#' ongoing study is used for standardization (\code{time_std}).
#'
#' @param y numeric vector, number of events in the trials.
#' @param n numeric vector, number of patients in the trials (either the total
#' sample size or effective sample size, see Details for more information).
#' @param time numeric vector, strictly positive, time at risk in the trials.
#' @param time_std numeric, the unit of time we standardize to.
#' @param alpha the one-sided percentile to use in the chi-squared test of
#' heterogeneity.
#' @param verbose logical, TRUE to see log messages about heterogeneous studies
#' being dropped.
#'
#' @return a list with elements alpha and beta, the shape parameters of a beta
#' distribution that defines the prior.
#'
#' @importFrom stats qchisq
#' @export
#'
#' @examples
#' y <- c(10, 300, 0)
#' n <- c(95, 1050, 5)
#' time <- c(1, 3, 0.1)
#' chi_squ_prior(y = y, n = n, time = time)
chi_squ_prior <- function(y, n, time, time_std = 1, alpha = 0.01,
                          verbose = FALSE) {

  # Be vigilant:
  if (length(n) != length(y)) {
    stop("n and y must be the same length")
  }
  if (any(is.na(n)) | any(is.na(y))) {
    stop("n and y should be non-missing")
  }
  if (length(n) != length(time)) {
    stop("n and time must be the same length")
  }
  if (any(y < 0)) {
    stop("y must be non-negative")
  }
  if (any(y > n)) {
    stop("Elements in y must be no more than the congruent element in n")
  }
  if (any(n < 1)) {
    stop("Each element of n must be at least 1")
  }
  if (any(time <= 0) | any(is.na(time))) {
    stop("Each element of time should be strictly positive and non-missing")
  }

  # Get standardized event counts
  dat_std <- get_std_event_counts(y = y, n = n, time = time,
                                  time_std = time_std, as_data_frame = TRUE)
  y_std <- dat_std$y_std
  include <- rep(TRUE, length(y))
  n_t <- sum(dat_std$n[include])
  y_t <- sum(y_std[include])
  y_exp <- dat_std$n * (y_t / n_t)
  y_exp[!include] <- NA
  Ci <- (y_std - y_exp)^2 * (1 / y_exp + 1 / (dat_std$n - y_exp))
  C_t <- sum(Ci, na.rm = TRUE)
  M <- sum(include)
  crit <- qchisq(p = alpha, df = M - 1, lower.tail = FALSE)
  while (sum(include) > 1) {
    n_t <- sum(dat_std$n[include])
    y_t <- sum(y_std[include])
    y_exp <- dat_std$n * (y_t / n_t)
    y_exp[!include] <- NA
    Ci <- (y_std - y_exp)^2 * (1 / y_exp + 1 / (dat_std$n - y_exp))
    C_t <- sum(Ci, na.rm = TRUE)
    M <- sum(include)
    crit <- qchisq(p = alpha, df = M - 1, lower.tail = FALSE)
    if (C_t > crit) {
      # Homogeneity is rejected. Remove outlier and try again
      if (sum(Ci == max(Ci, na.rm = TRUE), na.rm = TRUE) > 1) {
        # "In the case of ties, exclude the study with the highest AE rate."
        # KB ALERT - surely this biases AE rates downwards? Removing the
        # smaller study would make more sense, in my view.
        removal_candidate_i <- seq_along(Ci)[Ci == max(Ci, na.rm = TRUE)]
        ae_rates <- y_std / dat_std$n
        remove_i <- removal_candidate_i[
          which.max(ae_rates[removal_candidate_i])]

      } else {
        remove_i <- which.max(Ci)
      }

      if (verbose) print(paste0("Removing study ", remove_i))
      include[remove_i] <- FALSE
      n_t <- sum(dat_std$n[include])
      y_t <- sum(y_std[include])
      y_exp <- dat_std$n * (y_t / n_t)
      y_exp[!include] <- NA
      Ci <- (y_std - y_exp)^2 * (1 / y_exp + 1 / (dat_std$n - y_exp))
      C_t <- sum(Ci, na.rm = TRUE)
      M <- sum(include)
      crit <- qchisq(p = alpha, df = M - 1, lower.tail = FALSE)
      next
    } else {
      # Success
      break
    }
  }

  return(list(alpha = y_t, beta = n_t - y_t))
}
