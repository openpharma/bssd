#' Create datasets for simulations
#'
#' @param scenarioID character, simulation scenario ID
#'
#' @return
#' @export
#'
#' @examples
create_datasets <- function(scenarioID) {
  source(glue("{sc_root_dir}{'Parameters_'}{scenarioID}"))

  set.seed(seed)
  seeds <- sample(x = 1:MAX_INT, size = num_sims, replace = FALSE)

  for (i in 1:num_sims) {

    this_seed <- seeds[i]
    print(paste0("Running ", this_seed))

    # The parameter get_prob_func_active was added after many Parameters files
    # were created. get_current_data is null-tolerant so set
    # get_prob_func_active to NULL if not already defined
    if (!exists("get_prob_func_active")) {
      get_prob_func_active <- NULL
    }
    if (!exists("get_time_func_hist")) {
      get_time_func_hist <- NULL
    }

    # Get historic dataset
    set.seed(this_seed)
    hist_df <- get_hist_data_func(trial_data, get_prob_func, get_time_func_hist)
    hist_df <- subset(hist_df, select = -c(prob))
    names(hist_df) <- c("STUDYID", "SUBJID", "STDFUTIME", "y")
    hist_df <- hist_df %>% add_column(STUDYCAT = "Historical",
                                      .before = "STUDYID")

    # Get current dataset
    curr_df <- get_current_data(n_control = n_control,
                                n_active = n_active,
                                follow_up_time = follow_up_time,
                                lambda_control = lambda_control,
                                lambda_active = lambda_active,
                                get_prob_func = get_prob_func,
                                get_prob_func_active = get_prob_func_active,
                                get_time_func = get_time_func)

    curr_df <- subset(curr_df, select = -c(arm, prob))
    names(curr_df) <- c("STDFUTIME", "y")
    curr_df <- curr_df %>% add_column(STUDYCAT = "Test",
                                      STUDYID = max(hist_df$STUDYID) + 1,
                                      SUBJID = 1:nrow(curr_df),
                                      .before = "STDFUTIME")

    combn_df <- rbind(hist_df, curr_df)
    combn_df <- combn_df %>% add_column(USUBJID = 1:nrow(combn_df),
                                        .before = "STUDYID")

    # Create out directory if not present
    out_dir <- paste0(sc_root_dir, "/")
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE)
    }

    # Save
    write.csv(x = combn_df, file = paste0(out_dir, "df_", scenarioID, ".csv"),
              row.names = FALSE)

  }
}
