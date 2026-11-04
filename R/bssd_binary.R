#' Creator for class \code{bssd_binary}.
#'
#' @description The object in element \code{fit} is an object of type
#' \code{stanfit} from the rstan package. It contains samples for variables
#' \code{pi1} and \code{pi2}. These are the probabilities of at least one event
#' in a single unit of time in arms one and two.
#'
#' @param y instance of class \code{integer}. y integer vector, 0 for no event,
#' 1 for event, for all patients.
#' @param time instance of class \code{numeric}. Time numeric vector, strictly
#' positive, time covariate.
#' @param fit instance of class \code{stanfit}.
#'
#' @return A list of class \code{bssd_binary} (also inheriting from
#' \code{bssd}), containing the observed event indicators \code{y}, follow-up
#' times \code{time}, and the fitted Stan model \code{fit}.
#'
#' @export
#'
#' @seealso [stan_bssd()] for examples.
bssd_binary <- function(y, time, fit) {

  l <- list(y = y,
            time = time,
            fit = fit
  )

  class(l) <- c("bssd_binary", "bssd")
  l
}

#' Summary statistics of a BSSD model.
#'
#' @param object A \code{bssd_binary} object.
#' @param alpha Complement of the interval mass; 0.05 gives a 95% interval.
#' @param ... Unused.
#'
#' @return A tibble with three rows of summary statistics. The \code{variable}
#' column identifies the event probabilities in arms 1 and 2 (\code{pi1} and
#' \code{pi2}) and the probability that the event rate in arm 2 is greater than
#' that in arm 1 (\code{prob_higher}). The \code{mean}, \code{median}, and
#' \code{sd} columns contain the corresponding estimates, while \code{.lower}
#' and \code{.upper} give the lower and upper bounds of the highest posterior
#' density interval. The \code{alpha} column gives the complement of the
#' interval mass.
#'
#' @importFrom stats sd
#' @importFrom HDInterval hdi
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @keywords internal
#' @export summary.bssd_binary
#' @export
summary.bssd_binary <- function(object, alpha = 0.05, ...) {
  x <- object
  credMass <- 1 - alpha
  pi1 <- as.data.frame(x$fit) %>% pull(pi1)
  hdi_pi1 <- hdi(pi1, credMass = credMass)
  pi2 <- as.data.frame(x$fit) %>% pull(pi2)
  hdi_pi2 <- hdi(pi2, credMass = credMass)
  prob_higher <- p_xgty_bootstrap(x = pi2, y = pi1, alpha = alpha)
  .lower <- .upper <- NULL
  bind_rows(
    tibble(variable = "pi1",
           mean = mean(pi1),
           median = median(pi1),
           sd = sd(pi1),
           .lower = hdi_pi1[1],
           .upper = hdi_pi1[2],
           alpha = alpha),
    tibble(variable = "pi2",
           mean = mean(pi2),
           median = median(pi2),
           sd = sd(pi2),
           .lower = hdi_pi2[1],
           .upper = hdi_pi2[2],
           alpha = alpha),
    tibble(variable = "prob_higher",
           mean = prob_higher %>% pull(mean),
           median = prob_higher %>% pull(median),
           sd = prob_higher %>% pull(sd),
           .lower = prob_higher %>% pull(.lower),
           .upper = prob_higher %>% pull(.upper),
           alpha = alpha)
  )
}


#' Print a BSSD model summary.
#'
#' @param x A \code{bssd_binary} object.
#' @param mcmc If \code{TRUE}, also print the fitted Stan model.
#' @param alpha Complement of the interval mass; 0.05 gives a 95% interval.
#' @param ... Unused.
#'
#' @return No return value, called for side effects. The method prints the
#' posterior mean event probabilities in both arms and the posterior
#' probability that the event rate in arm 2 is greater than that in arm 1,
#' together with their highest posterior density intervals. If
#' \code{mcmc = TRUE}, the underlying Stan fit is also printed.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select summarise rename pull n
#' @importFrom HDInterval hdi
#' @keywords internal
#' @export print.bssd_binary
#' @export
print.bssd_binary <- function(x, mcmc = FALSE, alpha = 0.05, ...) {
  credMass <- 1 - alpha
  .lower <- .upper <- NULL
  pi1 <- as.data.frame(x$fit) %>% pull(pi1)
  mean_pi1 <- mean(pi1)
  hdi_pi1 <- hdi(pi1, credMass = credMass)
  pi2 <- as.data.frame(x$fit) %>% pull(pi2)
  mean_pi2 <- mean(pi2)
  hdi_pi2 <- hdi(pi2, credMass = credMass)
  prob_higher <- p_xgty_bootstrap(x = pi2, y = pi1, alpha = alpha)

  cat("Prob(Event) in arm 1: ", mean_pi1 %>% round(3), " (",
      round(100 * credMass, 1), "% HPDI ",
      hdi_pi1[1] %>% round(3), ", ",
      hdi_pi1[2] %>% round(3), ")\n", sep = "")
  cat("Prob(Event) in arm 2: ", mean_pi2 %>% round(3), " (",
      round(100 * credMass, 1), "% HPDI ",
      hdi_pi2[1] %>% round(3), ", ",
      hdi_pi2[2] %>% round(3), ")\n", sep = "")
  cat("Prob(Event rate in arm 2 > arm 1): ",
      prob_higher %>% pull(mean) %>% round(3), " (",
      round(100 * credMass, 1), "% HPDI ",
      prob_higher %>% pull(.lower) %>% round(3), ", ",
      prob_higher %>% pull(.upper) %>% round(3), ")\n", sep = "")

  if (mcmc) {
    cat("\n")
    print(x$fit, digits = 3)
    cat("\n")
  }
}

#' Extract posterior draws from a BSSD model.
#'
#' @param x A \code{bssd_binary} object.
#' @param row.names Unused.
#' @param optional Unused.
#' @param ... The optional \code{pars} argument selects Stan parameters to
#' extract; other arguments are unused.
#'
#' @return A data frame containing posterior draws from the Stan model.
#' By default, the data frame contains \code{pi1} and \code{pi2}, representing
#' the probabilities of at least one event in a single unit of time in arms 1
#' and 2, respectively. Alternative Stan parameters can be selected using
#' \code{pars} in \code{...}.
#'
#' @keywords internal
#' @export as.data.frame.bssd_binary
#' @export
as.data.frame.bssd_binary <- function(x, row.names = NULL,
                                      optional = FALSE, ...) {
  args <- list(...)
  pars <- if ("pars" %in% names(args)) args$pars else c("pi1", "pi2")
  as.data.frame(x$fit, pars = pars)
}


#' Plot posterior event probabilities from a BSSD model.
#'
#' @param x A \code{bssd_binary} object.
#' @param ... Unused.
#'
#' @return A \code{ggplot} object showing the posterior density distributions
#' of \code{pi1} and \code{pi2}, the probabilities of at least one event in a
#' single unit of time in arms 1 and 2, respectively.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_recode
#' @importFrom ggplot2 ggplot aes geom_density labs
#' @keywords internal
#' @export plot.bssd_binary
#' @export
plot.bssd_binary <- function(x, ...) {
  pi1 <- pi2 <- value <- name <- NULL

  as.data.frame(x$fit) %>%
    select(pi1, pi2) %>%
    pivot_longer(cols = c("pi1", "pi2")) %>%
    mutate(name = fct_recode(name, `Arm 1` = "pi1", `Arm 2` = "pi2")) %>%
    ggplot(aes(x = value, fill = name)) +
    geom_density(alpha = 0.8) +
    labs(x = "Prob(event)", fill = "")
}
