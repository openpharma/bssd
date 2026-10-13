library("dplyr")
library("haven")
library(bssd)

# bssd project directory
proj_source <- rprojroot::find_rstudio_root_file()

dir_source <- "tests/testthat/simulations/datasets"
a <- paste0("0", 1:8, "a")
b <- paste0("0", 1:8, "b")

all_ids <- sort(c(a, b))


bssd_res_fun <- function(scenario_id) {

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
                                     refresh = 0, verbose = TRUE, iter = 4000,
                                     seed = 020224)

  ## Fit mixture to MCMC samples of MAP prior
  map_mix <- fit_mixture(map_prior, constrain_gt1 = FALSE, k = 2)


  ## Use MAP prior for placebo (arm1)
  w1_map <- map_mix["w", ]
  pi1_map_alpha <- map_mix["a", ]
  pi1_map_beta <- map_mix["b", ]


  ## Use uninformative prior for active arm (arm2)
  w2 <- as.array(c(1))
  pi2_alpha <- as.array(c(1 / 3))
  pi2_beta <- as.array(c(1 / 3))

  ## MCMC model using MAP priors
  fit_map <- stan_bssd(y = input_data_test %>% pull(y),
                     time = input_data_test %>% pull(STDFUTIME),
                     prob_arm1 = Pbo2TOT,
                     w1 = w1_map, pi1_alpha = pi1_map_alpha,
                     pi1_beta = pi1_map_beta,
                     w2 = w2, pi2_alpha = pi2_alpha, pi2_beta = pi2_beta,
                     refresh = 0, verbose = TRUE, iter = 4000, seed = 020224)


  sumtab <- summary(fit_map)
  sumtab <- as.data.frame(sumtab)


  sumtab$scenarioid <- scenario_id
  return(sumtab)
}



## all scenarios
set.seed(020224)
#system.time(
  bssd_results <-  lapply(all_ids, function(id) {
    bssd_res_fun(scenario_id = id)
  })
#)
names(bssd_results) <- all_ids



## Create result dataset
bssd_results_R <- do.call(rbind.data.frame, bssd_results)
rownames(bssd_results_R) <- 1:nrow(bssd_results_R)

bssd_results_R <- bssd_results_R %>%
  mutate(parameter = case_when(
    variable == "pi1" ~ "p_control",
    variable == "pi2" ~ "p_active",
    variable == "prob_higher" ~ "p_agtc"
  )) %>%
    mutate(sort_param = case_when(
      parameter == "p_active" ~ 1,
      parameter == "p_control" ~ 2,
      parameter == "p_agtc" ~ 3
    )) %>%
  arrange(scenarioid, sort_param) %>%
  relocate(scenarioid, .before = mean) %>%
  relocate(parameter, .before = mean) %>%
  select(-variable, -alpha, -sort_param)


colnames(bssd_results_R) <- c("scenarioid", "parameter", "mean", "median",
                              "std", "lower",  "upper")
