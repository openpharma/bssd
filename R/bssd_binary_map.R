#' Creator for class \code{bssd_binary_map}.
#'
#' @description The object in slot \code{fit} is an object of type
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
#' @return list-like object with type \code{bssd_binary_map}.
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

#' Summary statistics of the stan BSSD model.
#'
#' @param object stanfit object.
#' @param alpha if not null, 1 - alpha confidence interval is calculated.
#' @param ... extra parameters are passed to \code{\link{stan_bssd}}.
#'
#' @importFrom stats sd
#' @importFrom HDInterval hdi
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @keywords internal         # hide from the manual index
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


#' Print estimated mean event rate in each arm, and the posterior probability
#' that the true event rate is greater in the active arm.
#'
#' @param x stanfit object.
#' @param mcmc MCMC flag.
#' @param alpha if not null, 1 - alpha confidence interval is calculated.
#' @param ... extra parameters are passed to \code{\link{stan_bssd}}.
#'
#' @importFrom magrittr %>%
#' @importFrom HDInterval hdi
#' @keywords internal         # hide from the manual index
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

#' Store pi1 and pi2 values from the stan BSSD model in a dataframe.
#'
#' @param x stanfit object.
#' @param row.names row names flag.
#' @param optional optional flag.
#' @param ... extra parameters are passed to \code{\link{stan_bssd}}.
#'
#' @keywords internal         # hide from the manual index
#' @export as.data.frame.bssd_binary_map
#' @export
as.data.frame.bssd_binary_map <- function(x, row.names = NULL,
                                          optional = FALSE, ...) {
  args <- list(...)
  pars <- if ("pars" %in% names(args)) args$pars else c("pi_h", "a", "b",
                                                        "ess", "avg", "pi_star")
  as.data.frame(x$fit, pars = pars)
}

#' Plot stan BSSD model inferences.
#'
#' @param x stanfit object.
#' @param ... extra parameters are passed to \code{\link{stan_bssd}}.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_density labs
#' @keywords internal         # hide from the manual index
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
