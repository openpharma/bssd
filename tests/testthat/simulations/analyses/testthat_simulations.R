library(bssd)
library(testthat)


dir_R <- "tests/testthat/simulations/analyses/R"
dir_sas <- "tests/testthat/simulations/analyses/sas"

## Comparison of map priors R vs SAS
## R results
source(paste0(dir_R, "/map_priors_R.R"))


## SAS results
map_prior_mean_sas <- read.csv(file = paste0(dir_sas, "/map_priors_mean.csv"),
                               header = TRUE)


test_fun_map <- function(id, tol) {
  return_val <- try(
    test_that(paste0("ScenarioID ", id, ": Comparison of R results vs SAS ",
                     "results for prior means"), {

      actual <- map_priors_mean_R %>%
        filter(scenarioid %in% id) %>%
        select(mean)

      expected <- map_prior_mean_sas %>%
        filter(scenarioid %in% id) %>%
        select(mean)

      temp <- data.frame("ID" = id, "R" = actual, "SAS" = expected)
      colnames(temp) <- c("ID", "R", "SAS")
      expect_true(abs(pull(actual) - pull(expected)) < tol, info = temp)
    })
  )
  if (class(return_val) == "try-error") {
    res <- "Test failed"
  } else {
    res <- "Test passed"
  }
  return(res)
}

sapply(all_ids, function(id) {
  test_fun_map(id = id, tol = 0.01)
})

## Comparison of bssd results (mcmc) R vs SAS
## R results
source(paste0(dir_R, "/bssd_results_R.R"))

## SAS results
bssd_results_sas <- read.csv(file = paste0(dir_sas, "/bssd_results.csv"),
                             header = TRUE)


test_fun_bssd <- function(id, arm, param, tol) {
  return_val <- try(
    test_that(paste0("ScenarioID ", id, ": Comparison of R bssd results ",
                     "(mean) vs SAS bssd results (mean)"), {

      actual <- bssd_results_R %>%
        filter(scenarioid %in% id & parameter %in% arm) %>%
        select(param)

      expected <- bssd_results_sas %>%
        filter(scenarioid %in% id & parameter %in% arm)  %>%
        select(param)

      temp <- data.frame("ID" = id, "R" = actual, "SAS" = expected)
      colnames(temp) <- c("ID", "R", "SAS")
      expect_true(abs(pull(actual) - pull(expected)) < tol, info = temp)

    }))

  if (class(return_val) == "try-error") {
    res <- "Test failed"
  } else {
    res <- "Test passed"
  }
  return(res)
}

## Comparison of Pr(A > C)
## Specify tolerance level
tol.level <- 0.1
sapply(all_ids, function(id) {
  test_fun_bssd(id = id, arm = "p_agtc", param = "mean", tol = tol.level)
})


## Comparison of mean posterior probability and 95%-HDI confidence intervals in
## active arm
## Specify tolerance level
tol.level <- 0.05
sapply(all_ids, function(id) {
  test_fun_bssd(id = id, arm = "p_active", param = "mean", tol = tol.level)
})

sapply(all_ids, function(id) {
  test_fun_bssd(id = id, arm = "p_active", param = "lower", tol = tol.level)
})

sapply(all_ids, function(id) {
  test_fun_bssd(id = id, arm = "p_active", param = "upper", tol = tol.level)
})

## Comparison of mean posterior probability and 95%-HDI confidence intervals in
## control arm
sapply(all_ids, function(id) {
  test_fun_bssd(id = id, arm = "p_control", param = "mean", tol = tol.level)
})


sapply(all_ids, function(id) {
  test_fun_bssd(id = id, arm = "p_control", param = "lower", tol = tol.level)
})

sapply(all_ids, function(id) {
  test_fun_bssd(id = id, arm = "p_control", param = "upper", tol = tol.level)
})



## Comparison of bssd results (mcmc) R vs results from approximation method R
## Approx R results
source(paste0(dir_R, "/approx_bssd_R.R"))

## Compare MC results with MCMC results using test_that()
test_fun_approx <- function(id, tol) {
  return_val <- try(
    test_that(paste0("ScenarioID ", id, ": Comparison of MCMC results vs ",
                     "MC results for means"), {

      actual <- approx_bssd_results_R %>%
        filter(scenarioid %in% id) %>%
        select(mean)

      expected <- bssd_results_R %>%
        filter(scenarioid %in% id & parameter %in% "p_agtc") %>%
        select(mean)

      temp <- data.frame("ID" = id, "R" = actual, "SAS" = expected)
      colnames(temp) <- c("ID", "R", "SAS")
      expect_true(abs(pull(actual) - pull(expected)) < tol, info = temp)

    }))

  if (class(return_val) == "try-error") {
    res <- "Test failed"
  } else {
    res <- "Test passed"
  }
  return(res)
}

## Compare all scenarios
sapply(all_ids, function(id) {
  test_fun_approx(id = id, tol = 0.1)
})
