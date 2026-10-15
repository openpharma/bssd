# Derive a beta prior for an AE from an homogeneous set of historical studies.

The historical studies are assumed to be heterogeneous. That assumption
is tested using sequential chi-squared tests. If homogeneity is
detected, the largest outlier study is removed from the historical set
and the test rerun. This cycle continues until: i) heterogeneity is not
rejected; or ii) there is only one historical study remaining.

## Usage

``` r
chi_squ_prior(y, n, time, time_std = 1, alpha = 0.01, verbose = FALSE)
```

## Arguments

- y:

  numeric vector, number of events in the trials.

- n:

  numeric vector, number of patients in the trials (either the total
  sample size or effective sample size, see Details for more
  information).

- time:

  numeric vector, strictly positive, time at risk in the trials.

- time_std:

  numeric, the unit of time we standardize to.

- alpha:

  the one-sided percentile to use in the chi-squared test of
  heterogeneity.

- verbose:

  logical, TRUE to see log messages about heterogeneous studies being
  dropped.

## Value

a list with elements alpha and beta, the shape parameters of a beta
distribution that defines the prior.

## Details

It is recommended to use the total number of events in the trial (one
event per subject is counted) and effective sample size (the number of
subjects who completed the study plus fractional dropouts) as the number
of patients in the trial (`n`). The planned study duration of the
blinded ongoing study is used for standardization (`time_std`).

## Examples

``` r
y <- c(10, 300, 0)
n <- c(95, 1050, 5)
time <- c(1, 3, 0.1)
chi_squ_prior(y = y, n = n, time = time)
#> $alpha
#> [1] 121.4013
#> 
#> $beta
#> [1] 1028.599
#> 
```
