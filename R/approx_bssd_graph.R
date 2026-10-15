#' Graphical representation of potential safety signal detection for a given
#' ongoing trial sample size.
#'
#' @description This function creates a graph that represents all possible
#' occurrences of observed events for an ongoing trial given a fixed sample
#' size. The x-axis represents the number of participants (also equivalent to
#' time since time can be expressed in a completed-trial-participant unit).
#' The y-axis represents the number of participants with one AE. Risk levels,
#' i.e., Pr(active AE rate > placebo AE rate), are expressed via color coding:
#' green, yellow, and red representing risk levels that would warrant no action,
#' a warning, and an alert, respectively. These three levels are delimited by
#' two thresholds that are set to 80% and 90% by default. This function
#' assumes that all the enrolled patients are completers (no dropout). The
#' threshold levels can be changed.
#'
#' @param time numeric, total time at risk (units = study duration). It
#' can also be construed as the actual number of participants in the ongoing
#' trial, and not the effective number of patients. This parameter expects a
#' positive whole number.
#' @param prob_arm1 numeric, probability of being randomized to placebo (arm1).
#' @param pi1_alpha numeric, first shape param of beta prior distribution on
#'   unit-time event probability in placebo (arm1).
#' @param pi1_beta numeric, second shape param of beta prior distribution on
#'   unit-time event probability in placebo (arm1).
#' @param pi2_alpha numeric, first shape param of beta prior distribution on
#'   unit-time event probability in active (arm2).
#' @param pi2_beta numeric, second shape param of beta prior distribution on
#'   unit-time event probability in active (arm2).
#' @param num_samples integer, number of replication of the simulated samples.
#'   More is better, up to a point. The default is 50000 and gives robust
#'   estimations of Pr(active AE rate > placebo AE rate). This parameter can be
#'   changed to a smaller number to fasten the computation time, but the
#'   estimation will be less accurate.
#'
#' @return list, ggplot2 graph.
#'
#' @importFrom dplyr filter case_when
#' @importFrom ggplot2 geom_point geom_abline scale_color_manual
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous ggtitle theme
#' @importFrom ggplot2 element_text
#' @importFrom stringr str_wrap
#'
#' @export
#'
#' @examples
#' # Note: Set seed for reproducible graph.
#' set.seed(2021)
#' approx_bssd_graph(
#'   time = 80, prob_arm1 = 1 / 3,
#'   pi1_alpha = 151.3025005, pi1_beta = 1784.3675521, pi2_alpha = 0.3,
#'   pi2_beta = 0.3, num_samples = 1000
#' )
approx_bssd_graph <- function(time,
                              prob_arm1,
                              pi1_alpha,
                              pi1_beta,
                              pi2_alpha = 0.3,
                              pi2_beta = 0.3,
                              num_samples = 50000) {
  # Be vigilant:
  if (time != round(time)) {
    stop(paste0("time should be an integer. ",
         "The x-axis in this graph is discontinuous."))
  }


  ##############################################################################
  # Graphical representation of the planned safety signal detection            #
  # (non-informative prior for Active treatment)                               #
  ##############################################################################

  # Plot showing the number of AEs needed to trigger a signal for different
  # total time at risk in units of equivalent completed-study-participants.
  # N is the number of participants in the test study.
  N <- time

  # Prior for placebo
  probs <- matrix(rep(-1, (N + 1) * N), nrow = (N + 1), ncol = N)

  for (iCol in seq(5, N, 5)) {
    for (iRow in 1:(iCol + 1)) {

      # Skip iterations where the number of events is larger than the number of
      # patients in smallest group. The method not valid under these conditions.
      time <- iCol
      Nc <- round(time * prob_arm1)
      Na <- round(time * (1 - prob_arm1))
      if (iRow - 1 > min(Nc, Na)) next

      tempo <- approx_bssd(
        y = iRow - 1,
        time = iCol,
        prob_arm1 = prob_arm1,
        pi1_alpha = pi1_alpha,
        pi1_beta = pi1_beta,
        pi2_alpha = pi2_alpha,
        pi2_beta = pi2_beta,
        num_samples = num_samples
      )

      probs[iRow, iCol] <- tempo

      if (probs[iRow, iCol] > 0.99) {
        # Save time by skipping very high probabilities
        break
      }
    }
  }


  circle_size <- 2

  # Initialize global variables to avoid check warning
  R <- N <- Signal <- value <- NULL

  probs_df <-
    tibble(
      R = rep(1:nrow(probs), times = ncol(probs)) - 1,
      N = rep(1:ncol(probs), each = nrow(probs)),
      value = as.vector(probs)
    ) %>%
    mutate(Signal = case_when(
      value < 0.8 ~ "none",
      value < 0.9 ~ "warn", TRUE ~ "alert"
    ) %>%
      factor(levels = c("none", "warn", "alert")))


  ssdplot <- probs_df %>%
    filter(value != -1) %>%
    ggplot(aes(x = N, y = R, col = Signal)) +
    geom_point(size = circle_size) +
    geom_abline(
      intercept = 0,
      slope = pi1_alpha / (pi1_alpha + pi1_beta),
      linewidth = 1.5,
      col = "black"
    ) +
    geom_abline(
      intercept = 0,
      slope = (2 * (1 - prob_arm1) + prob_arm1) * pi1_alpha
        / (pi1_alpha + pi1_beta),
      linewidth = 1.5,
      col = "brown"
    ) +
    geom_abline(
      intercept = 0,
      slope = (3 * (1 - prob_arm1) + prob_arm1) * pi1_alpha
        / (pi1_alpha + pi1_beta),
      linewidth = 1.5,
      col = "blue"
    ) +
    scale_color_manual(values = c("darkgreen", "gold", "red")) +
    scale_x_continuous(breaks = seq(0, nrow(probs), 25)) +
    scale_y_continuous(breaks = seq(0, ncol(probs), 2)) +
    labs(y = "Incidence of observed AEs", x = "Effective Sample Size") +
    ggtitle(
      str_wrap(
        paste0(
          "Risk levels as a function of effective sample size ",
          "and number of observed AEs"
        ),
        width = 50
      )
    ) +
    theme(plot.title = element_text(hjust = 0.5))


  ssdplot
}
