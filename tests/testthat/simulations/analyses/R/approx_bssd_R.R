library("dplyr")
library("haven")
library(bssd)

# bssd project directory
proj_source <- rprojroot::find_rstudio_root_file()

dir_source <- "tests/testthat/simulations/datasets"
a <- paste0("0", 1:8, "a")
b <- paste0("0", 1:8, "b")

all_ids <- sort(c(a, b))

## MAP Prior derivation
approx_bssd_res_fun <- function(scenario_id) {

  ## read in data
  input_data <- read.csv(paste0(proj_source, "/", dir_source, "/df_ScenarioID",
                                scenario_id, ".csv"))

  ## plit data into historical and test data
  input_data_historical <- input_data %>% filter(STUDYCAT == "Historical")
  input_data_test <- input_data %>% filter(STUDYCAT == "Test")

  ## specify randomization ratio
  Pbo2TOT <- 1 / 4

  ## MAP Prior derivation
  ## Fitting STAN MAP prior to historical data
  map_prior <- stan_map_patient_data(y = input_data_historical %>% pull(y),
                                     trial = input_data_historical %>%
                                       pull(STUDYID),
                                     time = input_data_historical %>%
                                       pull(STDFUTIME),
                                     a_alpha = 1 / 3, a_beta = 1 / 3,
                                     b_alpha = 5, b_beta = 5, cores = 4,
                                     refresh = 0, iter = 4000, seed = 020224)


  # Convert multiple-component MAP prior to 1-component MAP prior

  # one component beta prior
  map_mix_1 <- fit_mixture(map_prior, constrain_gt1 = FALSE, Nc = 1)

  ## Use uninformative prior for active arm (arm2)
  w2 <- as.array(c(1))
  pi2_alpha <- as.array(c(1 / 3))
  pi2_beta <- as.array(c(1 / 3))

  approx_map <- approx_bssd(
    y = sum(input_data_test %>% pull(y)),
    time = sum(input_data_test %>% pull(STDFUTIME)),
    prob_arm1 = Pbo2TOT,
    pi1_alpha = map_mix_1[2, ],
    pi1_beta = map_mix_1[3, ],
    pi2_alpha = pi2_alpha,
    pi2_beta = pi2_beta,
    num_samples = 50000
  )

  approx_bssd_res <- data.frame(scenarioid = scenario_id, mean = approx_map)
  return(approx_bssd_res)
}


## all scenarios
set.seed(020224)
#system.time(
  approx_bssd_results <-  lapply(all_ids, function(id) {
    approx_bssd_res_fun(scenario_id = id)
  })
#)

names(approx_bssd_results) <- all_ids


## Create result dataset
approx_bssd_results_R <- do.call(rbind.data.frame, approx_bssd_results)
rownames(approx_bssd_results_R) <- 1:nrow(approx_bssd_results_R)
