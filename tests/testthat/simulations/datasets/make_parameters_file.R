#' Make parameters file for simulations
#'
#' @param params_filename Character, name of parameters file
#' @param seed Integer, seed number
#' @param num_sims Integer, number of simulation runs
#' @param hist_sample_size Numeric, containing integer sample sizes of
#' historical trials.
#' @param hist_follow_up_time Numeric, containing expected duration of patient
#' follow-up of historical trials.
#' @param hist_lambda Numeric, containing hazard parameters that prevail in
#' historical trials.
#' @param get_time_func Function like \code{\link{complete_follow_up}} that
#' takes as arguments sample_size and follow_up_time and returns the a vector
#' of follow-up times for the patients.
#' @param get_time_func_hist
#' @param get_prob_func Function like \code{\link{get_prob_constant_hazard}}
#' that takes as arguments time and lambda, and returns the probability of
#' event.
#' @param get_prob_func_active Function like
#'   \code{\link{get_prob_constant_hazard}} that takes as arguments time and
#'   lambda, and returns the probability of event for patients only in the
#'   active arm. If NULL (by default), then get_prob_func is used for both
#'   placebo and active patients.
#' @param lambda_control numeric, value for risk parameter lambda for placebo
#'   patients.
#' @param lambda_active numeric, value for risk parameter lambda for active
#'   patients.
#' @param n_control integer, sample size in placebo arm. Must be > 0.
#' @param n_active integer, sample size in active arm. Must be > 0.
#' @param follow_up_time numeric, expected length of follow-up time.
#' @param output_stan_bssd_fits
#'
#' @return
#' @export
#'
#' @examples
make_parameters_file <- function(
  params_filename, seed,
  num_sims, hist_sample_size,
  hist_follow_up_time, hist_lambda,
  get_time_func = "complete_follow_up",
  get_time_func_hist = "complete_follow_up",
  get_prob_func = "get_prob_constant_hazard",
  get_prob_func_active = "get_prob_constant_hazard",
  lambda_control, lambda_active,
  n_control, n_active,
  follow_up_time = 1,
  output_stan_bssd_fits = FALSE
) {
  txt <- glue("
MAX_INT <- 2^31
seed <- {seed}
num_sims <- {num_sims}
output_stan_bssd_fits <- {output_stan_bssd_fits}

trial_data <- tibble(
  sample_size = {hist_sample_size},
  follow_up_time = {hist_follow_up_time},
  lambda = {hist_lambda}
) %>% mutate(trial_id = as.integer(rownames(.)))
get_time_func <- {get_time_func}
get_time_func_hist <- {get_time_func_hist}
get_prob_func <- {get_prob_func}
get_prob_func_active <- {get_prob_func_active}

get_hist_data_func <- get_patient_level_hist_data
patient_level <- TRUE

n_control <- {n_control}
lambda_control <- {lambda_control}
n_active <- {n_active}
lambda_active <- {lambda_active}

follow_up_time <- {follow_up_time}

# Hyperprior parameters for historical analysis
a_alpha <- 1 / 3
a_beta <- 1 / 3
b_alpha <- 5
b_beta <- 5

# Prior parameters for active arm in current analysis
active_alpha <- 1 / 3
active_beta <- 1 / 3
")
  txt

  writeLines(txt, con = glue("{sc_root_dir}{params_filename}"))
}
