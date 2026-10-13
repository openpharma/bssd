#' Creator for class \code{bssd_binary}.
#'
#' @description The object in slot \code{fit} is an object of type
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
#' @return object of type \code{bssd_binary}.
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


#' Print estimated mean event rate in each arm, and the posterior probability
#' that the true event rate is greater in the active arm.
#'
#' @param x stanfit object.
#' @param mcmc MCMC flag.
#' @param alpha if not null, 1 - alpha confidence interval is calculated.
#' @param ... extra parameters are passed to \code{\link{stan_bssd}}.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select summarise rename pull n
#' @importFrom HDInterval hdi
#' @keywords internal         # hide from the manual index
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

#' Store pi1 and pi2 values from the stan BSSD model in a dataframe.
#'
#' @param x stanfit object.
#' @param row.names row names flag.
#' @param optional optional flag.
#' @param ... extra parameters are passed to \code{\link{stan_bssd}}.
#'
#' @keywords internal         # hide from the manual index
#' @export as.data.frame.bssd_binary
#' @export
as.data.frame.bssd_binary <- function(x, row.names = NULL,
                                      optional = FALSE, ...) {
  args <- list(...)
  pars <- if ("pars" %in% names(args)) args$pars else c("pi1", "pi2")
  as.data.frame(x$fit, pars = pars)
}


#' Plot stan BSSD model inferences.
#'
#' @param x stanfit object.
#' @param ... extra parameters are passed to \code{\link{stan_bssd}}.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate
#' @importFrom tidyr pivot_longer
#' @importFrom forcats fct_recode
#' @importFrom ggplot2 ggplot aes geom_density labs
#' @keywords internal         # hide from the manual index
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
