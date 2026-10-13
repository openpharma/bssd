#' The 'bssd' package.
#'
#' @description Fits models for safety signal detection using Stan
#'
#' @name bssd-package
#' @aliases bssd
#' @useDynLib bssd, .registration = TRUE
#' @importFrom methods new
#' @importFrom Rcpp loadModule
#' @importFrom RcppParallel CxxFlags RcppParallelLibs
#' @importFrom rstan sampling stanc_builder
#' @importFrom rstantools rstan_config
#'
#' @references
#' Stan Development Team (2020). RStan: the R interface to Stan. R package
#' version 2.21.2. https://mc-stan.org
#'
"_PACKAGE"
