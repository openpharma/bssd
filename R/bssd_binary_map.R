#' Creator for class \code{bssd_binary_map}.
#'
#' @description The object in element \code{fit} is an object of type
#' \code{stanfit} from the rstan package. The \code{pi_star} variable contains
#' MCMC samples of the probability of at least one event in a single unit of
#' time, suitable to form the prior for a new trial that is exchangeable with
#' the historical trials. The \code{ess} variable contains samples for the
#' effective sample size of the distribution of pi_star. \code{avg} contains
#' samples for the mean of pi_star. The \code{pi_h[i]} variables contains
#' samples for the probability of at least one event in a single unit of time
#' for historical trials 1, 2...
#'
#' @param fit instance of class \code{stanfit}.
#'
#' @return A list of class \code{bssd_binary_map} (also inheriting from
#' \code{bssd_map}), containing the fitted Stan model \code{fit} and posterior
#' draws of the predictive distribution's mean (\code{mean}), effective
#' sample size (\code{ess}), and new-trial event probability (\code{pi_star}).
#'
#' @export
#'
#' @seealso [stan_map_summary_data()] and [stan_map_patient_data()]
#' for examples.
bssd_binary_map <- function(fit) {

  samp <- as.data.frame(fit)
  l <- list(
    fit = fit,
    # alpha = samp$pi_star_alpha,
    # beta = samp$pi_star_beta,
    mean = samp$avg,
    ess = samp$ess,
    pi_star = samp$pi_star
  )
  class(l) <- c("bssd_binary_map", "bssd_map")
  l
}

#' Summary statistics of a meta-analytic predictive BSSD model.
#'
#' @param object A \code{bssd_binary_map} object.
#' @param alpha Complement of the interval mass; 0.05 gives a 95% interval.
#' @param ... Unused.
#'
#' @return A tibble with two rows, identified by the \code{variable} column:
#' \code{mean} summarizes draws of the mean of the predictive distribution, and
#' \code{pi_star} summarizes draws of the unit-time event probability in a new
#' exchangeable trial. Columns \code{mean}, \code{median}, and \code{sd} give
#' posterior summaries; \code{.lower} and \code{.upper} give highest-density
#' interval bounds; and \code{alpha} gives the complement of the interval mass.
#'
#' @importFrom stats sd
#' @importFrom HDInterval hdi
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @keywords internal
#' @export summary.bssd_binary_map
#' @export
summary.bssd_binary_map <- function(object, alpha = 0.05, ...) {
  x <- object
  credMass <- 1 - alpha
  df <- as.data.frame(x$fit)
  hdi1_avg <- hdi(df$avg, credMass = credMass)
  hdi1_pi_star <- hdi(df$pi_star, credMass = credMass)
  bind_rows(
    tibble(variable = "mean",
           mean = mean(df$avg),
           median = median(df$avg),
           sd = sd(df$avg),
           .lower = hdi1_avg[1],
           .upper = hdi1_avg[2],
           alpha = alpha),
    tibble(variable = "pi_star",
           mean = mean(df$pi_star),
           median = median(df$pi_star),
           sd = sd(df$pi_star),
           .lower = hdi1_pi_star[1],
           .upper = hdi1_pi_star[2],
           alpha = alpha),
  )
}


#' Print a meta-analytic predictive BSSD model summary.
#'
#' @param x A \code{bssd_binary_map} object.
#' @param mcmc If \code{TRUE}, also print the fitted Stan model.
#' @param alpha Complement of the interval mass; 0.05 gives a 95% interval.
#' @param ... Unused.
#'
#' @return Invisibly returns \code{NULL}. Called for the side effect of printing
#' the posterior means and highest-density intervals for the mean of the
#' predictive distribution and the unit-time event probability in a new
#' exchangeable trial. If \code{mcmc} is \code{TRUE}, the fitted Stan model is
#' printed as well.
#'
#' @importFrom magrittr %>%
#' @importFrom HDInterval hdi
#' @keywords internal
#' @export print.bssd_binary_map
#' @export
print.bssd_binary_map <- function(x, mcmc = FALSE, alpha = 0.05, ...) {

  credMass <- 1 - alpha
  df <- as.data.frame(x$fit)
  mean_avg <- mean(df$avg)
  hdi1_avg <- hdi(df$avg, credMass = credMass)
  mean_pi_star <- mean(df$pi_star)
  hdi1_pi_star <- hdi(df$pi_star, credMass = credMass)

  cat("Prob(Event): Mean ", mean_avg %>% round(3), " (",
      round(100 * credMass, 1), "% HPDI ",
      hdi1_avg[1] %>% round(3), ", ",
      hdi1_avg[2] %>% round(3), ")\n", sep = "")
  cat("MAP Prob(Event): Mean ", mean_pi_star %>% round(3), " (",
      round(100 * credMass, 1), "% HPDI ",
      hdi1_pi_star[1] %>% round(3), ", ",
      hdi1_pi_star[2] %>% round(3), ")\n", sep = "")

  if (mcmc) {
    cat("\n")
    print(x$fit, digits = 3)
    cat("\n")
  }
}

#' Extract posterior draws from a meta-analytic predictive BSSD model.
#'
#' @param x A \code{bssd_binary_map} object.
#' @param row.names Unused.
#' @param optional Unused.
#' @param ... The optional \code{pars} argument selects Stan parameters to
#' extract; other arguments are unused.
#'
#' @return A data frame with one row per retained MCMC draw, pooled across
#' chains. By default, columns \code{pi_h[1]}, \code{pi_h[2]}, and so on
#' contain unit-time event probabilities for the historical trials. Other
#' columns contain \code{a} (the predictive mean), \code{b} (the fraction of
#' historical data borrowed), \code{ess} (the effective sample size),
#' \code{avg} (the predictive mean), and \code{pi_star} (the unit-time event
#' probability in a new exchangeable trial). Supplying \code{pars} selects
#' different parameters from the fitted Stan model.
#'
#' @keywords internal
#' @export as.data.frame.bssd_binary_map
#' @export
as.data.frame.bssd_binary_map <- function(x, row.names = NULL,
                                          optional = FALSE, ...) {
  args <- list(...)
  pars <- if ("pars" %in% names(args)) args$pars else c("pi_h", "a", "b",
                                                        "ess", "avg", "pi_star")
  as.data.frame(x$fit, pars = pars)
}

#' Plot meta-analytic predictive event probabilities.
#'
#' @param x A \code{bssd_binary_map} object.
#' @param ... Unused.
#'
#' @return A \code{ggplot} object showing density curves for posterior draws
#' of \code{avg}, the mean of the predictive distribution, and \code{pi_star},
#' the unit-time event probability in a new exchangeable trial. Print the
#' object to display the plot.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_density labs
#' @keywords internal
#' @export plot.bssd_binary_map
#' @export
plot.bssd_binary_map <- function(x, ...) {
  avg <- pi_star <- value <- name <- NULL

  as.data.frame(x$fit) %>%
    select(avg, pi_star) %>%
    pivot_longer(cols = c("avg", "pi_star")) %>%
    ggplot(aes(x = value, fill = name)) +
    geom_density(alpha = 0.8) +
    labs(x = "Prob(event)", fill = "")
}
