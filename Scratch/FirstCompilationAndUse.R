
# Following https://cran.r-project.org/web/packages/rstantools/vignettes/minimal-rstan-package.html ----
example(source) # defines the sourceDir() function
try(roxygen2::roxygenize(load_code = sourceDir), silent = TRUE)
pkgbuild::compile_dll()
roxygen2::roxygenize()

# using ../rstanlm because already inside the package directory
getwd()
install.packages("../bssd", repos = NULL, type = "source")
# Failed...
# Weirdly, below lines worked anyway, so compile_dll followed by roxygenize
# appears to have installed the package correctly?
library("bssd")
fit <- lm_stan(y = rnorm(10), x = rnorm(10),
               # arguments passed to sampling
               iter = 2000, refresh = 500)
print(fit)
# Works fine.
