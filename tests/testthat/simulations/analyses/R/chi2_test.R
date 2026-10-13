library(dplyr)
library(haven)
library(bssd)
library(testthat)

## find BSSD project root directory
dir_source <- paste0(rprojroot::find_rstudio_root_file(),
                     "/tests/testthat/simulations/datasets")

## create character simulation IDs
a <- paste0("0", 1:8, "a")
b <- paste0("0", 1:8, "b")
all_ids <- sort(c(a, b))

## Create a function to manually compute chi-squared parameters for
## validation purposes
chisq_fun <- function(id, data, alpha=0.01) {
  mat <- data %>%
         mutate(NoAE = neff - y_std_formula) %>%
         select(y_std_formula, NoAE)
  chisq_res <- chisq.test(mat)
  while (chisq_res$p.value <= alpha) {
   chi_comp <- (chisq_res$observed - chisq_res$expected)^2 / chisq_res$expected
   chi_comp <- cbind(chi_comp, "Total" = rowSums(chi_comp))
   ind <- which.max(chi_comp[, "Total"])
   mat <- mat[-ind, ]
   chisq_res <- chisq.test(mat)
  }
  res <- colSums(mat)
  names(res) <- c("alpha", "beta")
 return(res)
}

## Create a function to compute standardized AE counts
get_std_fun <- function(scenario_id) {

  ## bind STUDYCAT variable locally to the function
  ## pre-define STUDYCAT to NULL to remove R CMD note regarding an
  ## undefined global variable
  STUDYCAT <- NULL

  ## read in data
  input_data <- read.csv(paste0(dir_source,
                                "/df_ScenarioID",
                                scenario_id,
                                ".csv"))

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

  ## Create merged dataset with input parameters
  T_std <- 1
  hist_df_by_trial <-  left_join(time_by_trial,
                                 y_hist_by_trial,
                                 by = "STUDYID") %>%
    left_join(., neff_hist_by_trial, by = "STUDYID") %>%
    mutate(y_std_formula = neff * (
      1 - (1 - (y / neff)) ^ ((T_std) / STDFUTIME))) %>%
    ungroup()

  return(hist_df_by_trial)
}

## Compute standardized AE counts for all simulations IDs
system.time(
  std_event_counts <-  lapply(all_ids, function(id) {
    get_std_fun(scenario_id = id)
  })
)
## Rename list elements to be simulation IDs
names(std_event_counts) <- all_ids


## Apply BSSD chi_squ_prior function to obtain alpha and beta values for
## each simulation ID
chi_res_bssd_sim <- sapply(all_ids, function(id) {
  data <- std_event_counts[[id]]
  chi_squ_prior(y = data %>% pull(y),
                n = data %>% pull(neff),
                time = data %>% pull(STDFUTIME),
                time_std = 1,
                alpha = 0.01)
})


## Apply validation chisq_fun function to obtain alpha and beta values for
## each simulation ID
chi_res_sim <- sapply(all_ids, function(id) {
  chisq_fun(id = id, data = std_event_counts[[id]])
})


## Create a function to compare parameters obtained with basic chisq.test()
## vs chi_squ_prior output using test_that()
test_fun <- function(id, data_act, data_exp, tol = 0.01) {
  return_val <- try(
    test_that(
      paste0(
        "ScenarioID ",
        id,
        ": Comparison of chi-square parameters (manual) ",
        "vs chi_squ_prior() output"), {

          actual <- unlist(data_act[, id])

          expected <- data_exp[, id]

          expect_equal(actual, expected, tolerance = tol)

        }
      )
    )

  if (class(return_val) == "try-error") {
    res <- "Test failed"
  } else {
    res <- "Test passed"
  }
  return(res)
}


## Apply the comparison function to Simulation scenarios
sapply(all_ids, function(id) {
  test_fun(id = id, data_act = chi_res_bssd_sim, data_exp = chi_res_sim)
})
