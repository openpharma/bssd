library(data.table)
library(readxl)
library(dplyr)

mypath <- paste0("/Biocdm/Products/Not-Product-specific/BayesSSDTesting/",
                 "biostat/programs/Chen/BSSDValidation/NUTS_delta/")

IDs <- c("01a", "01b", "02a", "02b", "03a", "03b", "04a", "04b",
         "05a", "05b", "06a", "06b", "07a", "07b", "08a", "08b")

# map prior results
map_priors <- list()
map_priors_mean <- data.frame(matrix(0, length(IDs), 2))
colnames(map_priors_mean) <- c("scenarioid", "mean")

for (i in 1:length(IDs)) {
  map_priors[[i]] <- read.csv(paste0(mypath, "MAP_beta_priors_ScenarioID",
                                     IDs[i], ".csv"))
  map_priors[[i]]$scenarioid <- IDs[i]

  map_priors_mean$scenarioid[i] <- IDs[i]
  map_priors_mean$mean[i] <- sum(
    map_priors[[i]]$weight * map_priors[[i]]$alpha /
      (map_priors[[i]]$alpha + map_priors[[i]]$beta))
  map_priors_mean$mean[i] <- round(map_priors_mean$mean[i], digits = 4)
}

map_priors <- rbindlist(map_priors)
map_priors <- map_priors[, c("scenarioid", "component", "weight",
                             "alpha", "beta")]

write.csv(map_priors,
          "tests/testthat/simulations/analyses/sas/map_priors.csv",
          row.names = FALSE)
write.csv(map_priors_mean,
          "tests/testthat/simulations/analyses/sas/map_priors_mean.csv",
          row.names = FALSE)

# bssd results
bssd_summary <- list()
bssd_interval <- list()
for (i in 1:length(IDs)) {
  bssd_summary[[i]] <- read_excel(paste0(mypath, "bssd_report_ScenarioID",
                                         IDs[i], ".xlsx"), sheet = 4)
  bssd_interval[[i]] <- read_excel(paste0(mypath, "bssd_report_ScenarioID",
                                          IDs[i], ".xlsx"), sheet = 5)

  bssd_summary[[i]] <- bssd_summary[[i]][c(5, 6, 7), c(1, 3, 4, 6)]
  colnames(bssd_summary[[i]]) <- c("parameter", "mean", "std", "median")
  bssd_summary[[i]]$scenarioid <- IDs[i]

  bssd_interval[[i]] <- bssd_interval[[i]][c(4, 5, 6), c(5, 6)]
  colnames(bssd_interval[[i]]) <- c("lower", "upper")
}

bssd_res <- cbind(rbindlist(bssd_summary), rbindlist(bssd_interval))
bssd_res <- bssd_res[, c("scenarioid", "parameter", "mean", "std", "median",
                         "lower", "upper")]
bssd_res <- bssd_res %>% mutate_at(c("mean", "std", "median", "lower", "upper"),
                                   as.numeric)

write.csv(bssd_res, "tests/testthat/simulations/analyses/sas/bssd_results.csv",
          row.names = FALSE)
