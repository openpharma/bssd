
# Document ----
roxygen2::roxygenise()
# roxygen2::roxygenise(clean = TRUE) # Do all
rstudioapi::previewRd('man/stan_map_patient_data.Rd')

# Build ----
# To rebuild and reinstall, click Build -> Clean and Rebuild.
# Perhaps can also run:
# devtools::install(build = TRUE)
# But that fails

# To just reinstall (no rebuild), click Build -> Load All.
# Perhaps can also run:
# devtools::install(build = FALSE)
# But that fails


# usethis ----
usethis::use_testthat()
usethis::use_test(name = "stan_map_patient_data")
usethis::use_test(name = "stan_map_summary_data")
usethis::use_news_md()
usethis::use_readme_rmd()
usethis::use_vignette(name = "priors",
                      title = "Specifying priors")
usethis::use_vignette(name = "blinded_bssd",
                      title = "BSSD on a blinded ongoing study")
usethis::use_vignette(name = "unblinded_bssd",
                      title = "BSSD on an unblinded ongoing study")
usethis::use_vignette(name = "bssd_and_adbayssd",
                      title = "BSSD and the adbayssd dataset")

# Run tests ----
devtools::test()
devtools::test(filter = 'stan_map_patient_data')
devtools::test(filter = 'stan_map_summary_data')

# usethis::use_pkgdown()
pkgdown::build_site()
pkgdown::build_articles()
devtools::build_manual()


# New attempt at installing gracefully ----

# Sys.getenv()
# Add modern C++ toolchain to path
Sys.setenv(
  PATH = paste(
    "/opt/software/texlive/2019/bin/x86_64-linux",
    "/usr/lib/jvm/java-1.8.0-openjdk/bin",
    "/opt/rh/devtoolset-11/root/usr/bin",
    "/usr/local/bin",
    "/usr/bin",
    "/usr/local/sbin",
    "/usr/sbin",
    "/usr/local/lib64",
    sep = ":"
  )
)

# Check dependencies are present:
library(methods)
library(Rcpp)
library(RcppParallel)
library(dplyr)
library(forcats)
library(ggplot2)
library(magrittr)
library(tidyr)
library(rstantools)
library(rstan)
# Test rstan
example(stan_model, package = "rstan", run.dontrun = TRUE)
library(RBesT)

# If needed
# install.packages("rstan")
# install.packages("RBesT")

# Install bssd

# Attempt 1
# remotes::install_git(url, git = "external")
# Fails on R v3.6.3

# Attempt 2
devtools::install(build = TRUE)
# Installation of devtools fails on R v3.6.3

# Attempt 3
# Then select Build -> Clean and Rebuild
# Also fails on R v3.6.3

# Check install
library(bssd)
