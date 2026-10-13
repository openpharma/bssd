library("dplyr")
library("haven")
library(bssd)
library(testthat)

dir_source <- paste0(rprojroot::find_rstudio_root_file(),
                     "/tests/testthat/simulations/datasets")
a <- paste0("0", 1:8, "a")
b <- paste0("0", 1:8, "b")

all_ids <- sort(c(a, b))



get_std_fun <- function(scenario_id) {

  ## read in data
  input_data <- read.csv(paste0(dir_source, "/df_ScenarioID",
                                scenario_id, ".csv"))

  ## split data into historical and test data
  input_data_historical <- input_data %>% filter(STUDYCAT == "Historical")
  input_data_test <- input_data %>% filter(STUDYCAT == "Test")

  neff_hist_by_trial <- input_data_historical %>%
    group_by(STUDYID) %>%
    summarise(neff = n_distinct(USUBJID))

  ## Use AE counts by trial for 'y' input
  y_hist_by_trial <-  input_data_historical %>%
    group_by(STUDYID) %>%
    summarise(y = sum(y))


  ## DURPLN (planned duration) by trial as 'time' input parameter
  time_by_trial <- input_data_historical %>%
    group_by(STUDYID) %>%
    distinct(STDFUTIME)

  ## Compute standardized AE counts (not needed as input parameter)
  T_std <- 1
  hist_df_by_trial <-  left_join(time_by_trial, y_hist_by_trial,
                                 by = "STUDYID") %>%
    left_join(., neff_hist_by_trial, by = "STUDYID") %>%
    mutate(y_std_formula = neff * (1 - (1 - (y / neff))^ ((T_std) /
                                                            STDFUTIME))) %>%
    ungroup()

  ## Compute standardized AE counts (not needed for chi_squ_prior())
  y_std <- get_std_event_counts(y = hist_df_by_trial %>% pull(y),
                                n = hist_df_by_trial %>% pull(neff),
                                time = hist_df_by_trial %>% pull(STDFUTIME),
                                time_std = 1)

  names(y_std) <- hist_df_by_trial %>% pull(STUDYID)
  hist_df_by_trial <- cbind(hist_df_by_trial,  "y_std_bssd" = y_std)

  return(hist_df_by_trial)
}

system.time(
  std_event_counts <-  lapply(all_ids, function(id) {
    get_std_fun(scenario_id = id)
  })
)

names(std_event_counts) <- all_ids

## Compare results obtaoned with formula vs get_std_event_counts()
## output using test_that()
test_fun <- function(id, data, tol=0) {
  return_val <- try(
    test_that(paste0("ScenarioID ", id, ": Comparison of standardized AE ",
                     "counts (formula) vs get_std_event_counts() output"), {

      actual <- data %>%
        select(y_std_bssd)

      expected <- data %>%
        select(y_std_formula)

      expect_equal(pull(actual), pull(expected),  tolerance = tol)

    }))

  if (class(return_val) == "try-error") {
    res <- "Test failed"
  } else {
    res <- "Test passed"
  }
  return(res)
}

sapply(all_ids, function(id) {
  test_fun(id = id, data = std_event_counts[[id]])
})
