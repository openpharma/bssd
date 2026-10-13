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
## Fitting STAN MAP prior to historical data
map_prior_mean_fun <- function(scenario_id) {

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
                                     refresh = 0, seed = 020224, iter = 4000)

  pi_star_mean_samples <- pull(summary(map_prior)[2, "mean"])
  # # Fit mixture to MCMC samples of MAP prior
  map_mix <- fit_mixture(map_prior, k = 2, constrain_gt1 = FALSE)


  ## Use MAP prior for placebo (arm1)
  w1_map <- map_mix["w", ]
  pi1_map_alpha <- map_mix["a", ]
  pi1_map_beta <- map_mix["b", ]

  prior_param_tab_map <- data.frame(scenarioid = scenario_id,
                                    component = 1:ncol(map_mix),
                                    cbind("weight" = w1_map,
                                          "alpha" = pi1_map_alpha,
                                          "beta" = pi1_map_beta)
  )
  rownames(prior_param_tab_map) <- c()


  # Convert multiple-component MAP prior to 1-component MAP prior

  # one component beta prior
  map_mix_1 <- fit_mixture(map_prior, Nc = 1, k = 2, constrain_gt1 = FALSE)

  prior_param_tab_map_mix_1 <- data.frame(scenarioid = scenario_id,
                                          component = 1:ncol(map_mix_1),
                                          cbind("weight" = map_mix_1[1, ],
                                                "alpha" = map_mix_1[2, ],
                                                "beta" = map_mix_1[3, ])
  )

  rownames(prior_param_tab_map_mix_1) <- c()

  ## Compute mean using formula
  ## Compute mean of 1-comp prior:
  prior_mean_1_comp <- map_mix_1[2, 1] / (map_mix_1[2, 1] + map_mix_1[3, 1])

  ## Compute mean of multiple components prior
  prior_mean <- sum(map_mix[1, ] * map_mix[2, ] / (map_mix[2, ] + map_mix[3, ]))

  prior_mean_res <- data.frame(scenarioid = scenario_id,
                               "pi_star_samples_mean" = pi_star_mean_samples,
                               "mean" = prior_mean,
                               "mean_1_comp" = prior_mean_1_comp)


  return(list(map_prior = prior_param_tab_map,
              map_prior_1 = prior_param_tab_map_mix_1,
              map_prior_mean = prior_mean_res))
}

## all scenarios
set.seed(020224)
#system.time(
 map_priors <-  lapply(all_ids, function(id) {
    map_prior_mean_fun(scenario_id = id)
  })
#)

names(map_priors) <- all_ids


## Create result dataset
means_vec <- lapply(map_priors, function(x) x$map_prior_mean)
map_priors_mean_R <- do.call(rbind.data.frame, means_vec)
rownames(map_priors_mean_R) <- 1:nrow(map_priors_mean_R)
