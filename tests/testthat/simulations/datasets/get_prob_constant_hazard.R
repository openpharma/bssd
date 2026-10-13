
#' Get probability of event, where hazard scales linearly in time.
#'
#' @param time numeric, vector of times
#' @param lambda numeric, value for risk parameter lambda
#'
#' @return numeric, vector of probabilities
#' @export
#'
#' @examples
#' time <- seq(0.5, 2, 0.5)
#' lambda <- 0.15
#' prob <- get_prob_constant_hazard(time, lambda)
get_prob_constant_hazard <- function(time, lambda) {
  1 - exp(- lambda * time)
}
