library(glue)
library(purrr)
library(tibble)
library(dplyr)

sc_root_dir <- "tests/testthat/simulations/datasets/"

source(paste0(sc_root_dir, "make_parameters_file.R"))
source(paste0(sc_root_dir, "create_datasets.R"))
source(paste0(sc_root_dir, "complete_follow_up.R"))
source(paste0(sc_root_dir, "get_prob_constant_hazard.R"))
source(paste0(sc_root_dir, "get_patient_level_hist_data.R"))
source(paste0(sc_root_dir, "get_current_data.R"))

# ScenarioID01a
scenarioID <- "ScenarioID01a"
make_parameters_file(
  params_filename = paste0("Parameters_", scenarioID),
  seed = 1697686361,
  num_sims = 1,
  hist_sample_size = "c(40, 40, 40)",
  hist_follow_up_time = "c(0.5, 1, 1.5)",
  hist_lambda = "c(0.05, 0.1, 0.15)",
  get_time_func = "complete_follow_up",
  get_prob_func = "get_prob_constant_hazard",
  get_prob_func_active = "get_prob_constant_hazard",
  lambda_control = 0.1,
  lambda_active = 0.2,
  n_control = 12,
  n_active = 38,
  follow_up_time = 1)

create_datasets(
  scenarioID = scenarioID
)

# ScenarioID01b
scenarioID <- "ScenarioID01b"
make_parameters_file(
  params_filename = paste0("Parameters_", scenarioID),
  seed = 1697686362,
  num_sims = 1,
  hist_sample_size = "c(40, 40, 40)",
  hist_follow_up_time = "c(0.5, 1, 1.5)",
  hist_lambda = "c(0.05, 0.1, 0.15)",
  get_time_func = "complete_follow_up",
  get_prob_func = "get_prob_constant_hazard",
  get_prob_func_active = "get_prob_constant_hazard",
  lambda_control = 0.1,
  lambda_active = 0.1,
  n_control = 12,
  n_active = 38,
  follow_up_time = 1)

create_datasets(
  scenarioID = scenarioID
)

# ScenarioID02a
scenarioID <- "ScenarioID02a"
make_parameters_file(
  params_filename = paste0("Parameters_", scenarioID),
  seed = 1697686363,
  num_sims = 1,
  hist_sample_size = "c(40, 40, 40)",
  hist_follow_up_time = "c(0.5, 1, 1.5)",
  hist_lambda = "c(0.05, 0.1, 0.15)",
  get_time_func = "complete_follow_up",
  get_prob_func = "get_prob_constant_hazard",
  get_prob_func_active = "get_prob_constant_hazard",
  lambda_control = 0.1,
  lambda_active = 0.2,
  n_control = 125,
  n_active = 375,
  follow_up_time = 1)

create_datasets(
  scenarioID = scenarioID
)

# ScenarioID02b
scenarioID <- "ScenarioID02b"
make_parameters_file(
  params_filename = paste0("Parameters_", scenarioID),
  seed = 1697686364,
  num_sims = 1,
  hist_sample_size = "c(40, 40, 40)",
  hist_follow_up_time = "c(0.5, 1, 1.5)",
  hist_lambda = "c(0.05, 0.1, 0.15)",
  get_time_func = "complete_follow_up",
  get_prob_func = "get_prob_constant_hazard",
  get_prob_func_active = "get_prob_constant_hazard",
  lambda_control = 0.1,
  lambda_active = 0.1,
  n_control = 125,
  n_active = 375,
  follow_up_time = 1)

create_datasets(
  scenarioID = scenarioID
)

# ScenarioID03a
scenarioID <- "ScenarioID03a"
make_parameters_file(
  params_filename = paste0("Parameters_", scenarioID),
  seed = 1697686365,
  num_sims = 1,
  hist_sample_size = "c(50, 50, 50, 100, 100, 100, 200, 200, 200)",
  hist_follow_up_time = "c(0.5, 1, 1.5, 0.5, 1, 1.5, 0.5, 1, 1.5)",
  hist_lambda = "c(0.05, 0.1, 0.15, 0.05, 0.1, 0.15, 0.05, 0.1, 0.15)",
  get_time_func = "complete_follow_up",
  get_prob_func = "get_prob_constant_hazard",
  get_prob_func_active = "get_prob_constant_hazard",
  lambda_control = 0.1,
  lambda_active = 0.2,
  n_control = 12,
  n_active = 38,
  follow_up_time = 1)

create_datasets(
  scenarioID = scenarioID
)

# ScenarioID03b
scenarioID <- "ScenarioID03b"
make_parameters_file(
  params_filename = paste0("Parameters_", scenarioID),
  seed = 1697686366,
  num_sims = 1,
  hist_sample_size = "c(50, 50, 50, 100, 100, 100, 200, 200, 200)",
  hist_follow_up_time = "c(0.5, 1, 1.5, 0.5, 1, 1.5, 0.5, 1, 1.5)",
  hist_lambda = "c(0.05, 0.1, 0.15, 0.05, 0.1, 0.15, 0.05, 0.1, 0.15)",
  get_time_func = "complete_follow_up",
  get_prob_func = "get_prob_constant_hazard",
  get_prob_func_active = "get_prob_constant_hazard",
  lambda_control = 0.1,
  lambda_active = 0.1,
  n_control = 12,
  n_active = 38,
  follow_up_time = 1)

create_datasets(
  scenarioID = scenarioID
)

# ScenarioID04a
scenarioID <- "ScenarioID04a"
make_parameters_file(
  params_filename = paste0("Parameters_", scenarioID),
  seed = 1697686367,
  num_sims = 1,
  hist_sample_size = "c(50, 50, 50, 100, 100, 100, 200, 200, 200)",
  hist_follow_up_time = "c(0.5, 1, 1.5, 0.5, 1, 1.5, 0.5, 1, 1.5)",
  hist_lambda = "c(0.05, 0.1, 0.15, 0.05, 0.1, 0.15, 0.05, 0.1, 0.15)",
  get_time_func = "complete_follow_up",
  get_prob_func = "get_prob_constant_hazard",
  get_prob_func_active = "get_prob_constant_hazard",
  lambda_control = 0.1,
  lambda_active = 0.2,
  n_control = 125,
  n_active = 375,
  follow_up_time = 1)

create_datasets(
  scenarioID = scenarioID
)

# ScenarioID04b
scenarioID <- "ScenarioID04b"
make_parameters_file(
  params_filename = paste0("Parameters_", scenarioID),
  seed = 1697686368,
  num_sims = 1,
  hist_sample_size = "c(50, 50, 50, 100, 100, 100, 200, 200, 200)",
  hist_follow_up_time = "c(0.5, 1, 1.5, 0.5, 1, 1.5, 0.5, 1, 1.5)",
  hist_lambda = "c(0.05, 0.1, 0.15, 0.05, 0.1, 0.15, 0.05, 0.1, 0.15)",
  get_time_func = "complete_follow_up",
  get_prob_func = "get_prob_constant_hazard",
  get_prob_func_active = "get_prob_constant_hazard",
  lambda_control = 0.1,
  lambda_active = 0.1,
  n_control = 125,
  n_active = 375,
  follow_up_time = 1)

create_datasets(
  scenarioID = scenarioID
)

# ScenarioID05a
scenarioID <- "ScenarioID05a"
make_parameters_file(
  params_filename = paste0("Parameters_", scenarioID),
  seed = 1697686369,
  num_sims = 1,
  hist_sample_size = "c(40, 40, 40)",
  hist_follow_up_time = "c(0.5, 1, 1.5)",
  hist_lambda = "c(0.01, 0.02, 0.03)",
  get_time_func = "complete_follow_up",
  get_prob_func = "get_prob_constant_hazard",
  get_prob_func_active = "get_prob_constant_hazard",
  lambda_control = 0.02,
  lambda_active = 0.04,
  n_control = 12,
  n_active = 38,
  follow_up_time = 1)

create_datasets(
  scenarioID = scenarioID
)

# ScenarioID05b
scenarioID <- "ScenarioID05b"
make_parameters_file(
  params_filename = paste0("Parameters_", scenarioID),
  seed = 1697686370,
  num_sims = 1,
  hist_sample_size = "c(40, 40, 40)",
  hist_follow_up_time = "c(0.5, 1, 1.5)",
  hist_lambda = "c(0.01, 0.02, 0.03)",
  get_time_func = "complete_follow_up",
  get_prob_func = "get_prob_constant_hazard",
  get_prob_func_active = "get_prob_constant_hazard",
  lambda_control = 0.02,
  lambda_active = 0.02,
  n_control = 12,
  n_active = 38,
  follow_up_time = 1)

create_datasets(
  scenarioID = scenarioID
)

# ScenarioID06a
scenarioID <- "ScenarioID06a"
make_parameters_file(
  params_filename = paste0("Parameters_", scenarioID),
  seed = 1697686371,
  num_sims = 1,
  hist_sample_size = "c(40, 40, 40)",
  hist_follow_up_time = "c(0.5, 1, 1.5)",
  hist_lambda = "c(0.01, 0.02, 0.03)",
  get_time_func = "complete_follow_up",
  get_prob_func = "get_prob_constant_hazard",
  get_prob_func_active = "get_prob_constant_hazard",
  lambda_control = 0.02,
  lambda_active = 0.04,
  n_control = 125,
  n_active = 375,
  follow_up_time = 1)

create_datasets(
  scenarioID = scenarioID
)

# ScenarioID06b
scenarioID <- "ScenarioID06b"
make_parameters_file(
  params_filename = paste0("Parameters_", scenarioID),
  seed = 1697686372,
  num_sims = 1,
  hist_sample_size = "c(40, 40, 40)",
  hist_follow_up_time = "c(0.5, 1, 1.5)",
  hist_lambda = "c(0.01, 0.02, 0.03)",
  get_time_func = "complete_follow_up",
  get_prob_func = "get_prob_constant_hazard",
  get_prob_func_active = "get_prob_constant_hazard",
  lambda_control = 0.02,
  lambda_active = 0.02,
  n_control = 125,
  n_active = 375,
  follow_up_time = 1)

create_datasets(
  scenarioID = scenarioID
)

# ScenarioID07a
scenarioID <- "ScenarioID07a"
make_parameters_file(
  params_filename = paste0("Parameters_", scenarioID),
  seed = 1697686373,
  num_sims = 1,
  hist_sample_size = "c(50, 50, 50, 100, 100, 100, 200, 200, 200)",
  hist_follow_up_time = "c(0.5, 1, 1.5, 0.5, 1, 1.5, 0.5, 1, 1.5)",
  hist_lambda = "c(0.01, 0.02, 0.03, 0.01, 0.02, 0.03, 0.01, 0.02, 0.03)",
  get_time_func = "complete_follow_up",
  get_prob_func = "get_prob_constant_hazard",
  get_prob_func_active = "get_prob_constant_hazard",
  lambda_control = 0.02,
  lambda_active = 0.04,
  n_control = 12,
  n_active = 38,
  follow_up_time = 1)

create_datasets(
  scenarioID = scenarioID
)

# ScenarioID07b
scenarioID <- "ScenarioID07b"
make_parameters_file(
  params_filename = paste0("Parameters_", scenarioID),
  seed = 1697686374,
  num_sims = 1,
  hist_sample_size = "c(50, 50, 50, 100, 100, 100, 200, 200, 200)",
  hist_follow_up_time = "c(0.5, 1, 1.5, 0.5, 1, 1.5, 0.5, 1, 1.5)",
  hist_lambda = "c(0.01, 0.02, 0.03, 0.01, 0.02, 0.03, 0.01, 0.02, 0.03)",
  get_time_func = "complete_follow_up",
  get_prob_func = "get_prob_constant_hazard",
  get_prob_func_active = "get_prob_constant_hazard",
  lambda_control = 0.02,
  lambda_active = 0.02,
  n_control = 12,
  n_active = 38,
  follow_up_time = 1)

create_datasets(
  scenarioID = scenarioID
)

# ScenarioID08a
scenarioID <- "ScenarioID08a"
make_parameters_file(
  params_filename = paste0("Parameters_", scenarioID),
  seed = 1697686375,
  num_sims = 1,
  hist_sample_size = "c(50, 50, 50, 100, 100, 100, 200, 200, 200)",
  hist_follow_up_time = "c(0.5, 1, 1.5, 0.5, 1, 1.5, 0.5, 1, 1.5)",
  hist_lambda = "c(0.01, 0.02, 0.03, 0.01, 0.02, 0.03, 0.01, 0.02, 0.03)",
  get_time_func = "complete_follow_up",
  get_prob_func = "get_prob_constant_hazard",
  get_prob_func_active = "get_prob_constant_hazard",
  lambda_control = 0.02,
  lambda_active = 0.04,
  n_control = 125,
  n_active = 375,
  follow_up_time = 1)

create_datasets(
  scenarioID = scenarioID
)

# ScenarioID08b
scenarioID <- "ScenarioID08b"
make_parameters_file(
  params_filename = paste0("Parameters_", scenarioID),
  seed = 1697686376,
  num_sims = 1,
  hist_sample_size = "c(50, 50, 50, 100, 100, 100, 200, 200, 200)",
  hist_follow_up_time = "c(0.5, 1, 1.5, 0.5, 1, 1.5, 0.5, 1, 1.5)",
  hist_lambda = "c(0.01, 0.02, 0.03, 0.01, 0.02, 0.03, 0.01, 0.02, 0.03)",
  get_time_func = "complete_follow_up",
  get_prob_func = "get_prob_constant_hazard",
  get_prob_func_active = "get_prob_constant_hazard",
  lambda_control = 0.02,
  lambda_active = 0.02,
  n_control = 125,
  n_active = 375,
  follow_up_time = 1)

create_datasets(
  scenarioID = scenarioID
)
