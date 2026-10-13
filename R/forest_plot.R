#' Forest-plot of MAP analysis.
#'
#' @description Create a forest-plot of MAP analysis.
#'
#' @param x Object of type \code{bssd_binary_map}.
#' @param alpha 0.05 to use 95% CIs, etc.
#' @param ... ignored.
#'
#' @return ggplot plot.
#'
#' @importFrom tidyr gather
#' @importFrom dplyr mutate select bind_rows tibble group_by summarise
#' @importFrom stringr str_extract
#' @importFrom forcats fct_inorder fct_rev
#' @importFrom stats quantile
#' @importFrom ggplot2 ggplot aes geom_pointrange labs
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
#' forest_plot(x)
#' forest_plot(x, alpha = 0.1)
forest_plot <- function(x, alpha = 0.05, ...) {

  if (!(any(class(x) %in% c("bssd_binary_map_sl",
                            "bssd_binary_map_pl",
                            "bssd_binary_map",
                            "bssd_map")))) {
    stop("input must be of type bssd_binary_map")
  }
  if (is.na(alpha)) {
    stop("alpha must be non-missing")
  }
  if (!(alpha >= 0 & alpha <= 1)) {
    stop("alpha must be between 0 and 1")
  }

  key <- level <- value <- trial <- l <- u <- NULL
  y <- as.data.frame(x$fit, pars = "pi_h") %>%
    gather() %>%
    mutate(
      trial = str_extract(key, "\\d+"),
      level = paste0("Trial ", trial)
    ) %>%
    select(level, value)
  y <- y %>%
    bind_rows(tibble(level = "MAP", value = x$pi_star)) %>%
    mutate(level = fct_inorder(level) %>% fct_rev)
  y %>%
    group_by(level) %>%
    summarise(
      mean = mean(value),
      l = quantile(value, alpha / 2),
      u = quantile(value, 1 - alpha / 2),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = mean, y = level)) +
    geom_pointrange(aes(xmin = l, xmax = u)) +
    labs(y = "", x = "Prob(Event)")
}
