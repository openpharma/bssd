# Get standardized adverse event counts.

Get standardized adverse event counts.

## Usage

``` r
get_std_event_counts(y, n, time, time_std = 1, as_data_frame = FALSE)
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

- as_data_frame:

  logical, TRUE to get results in a data-frame.

## Value

a data-frame if `as_data_frame = TRUE`; else a numeric vector.

## Details

To maintain consistency with other functions that create priors, such as
[`stan_map_summary_data`](stan_map_summary_data.md), we use the variable
names `y`, `n` and `time`. It is recommended to use the total number of
events in the trial (one event per subject is counted) and effective
sample size (the number of subjects who completed the study plus
fractional dropouts) as the number of patients in the trial (`n`). The
planned study duration of the blinded ongoing study is used for
standardization (`time_std`).

The returned data-frame includes the standardized adverse event counts
using the following formula:

\\y^\* = n \* (1 - (1 - (y/n))^{time\_{std} / time})\\

Note also that when any is a vector, `y`, `n`, `time_std`, and `time`
are recycled using default R and tibble recycling behaviors.

## Examples

``` r
# In a trial of ten patients, one experienced the event, with follow-up twice
# the length of our reference period:
get_std_event_counts(y = 1, n = 10, time = 2, time_std = 1)
#> [1] 0.513167

# In a trial of ten patients, one experienced the event, with follow-up half
# the length of our reference period:
get_std_event_counts(y = 1, n = 10, time = 0.5, time_std = 1)
#> [1] 1.9

# Those two examples in one call, with data-frame output:
get_std_event_counts(y = 1, n = 10, time = c(2, 0.5), time_std = 1,
  as_data_frame = TRUE)
#> # A tibble: 2 × 5
#>       y     n  time time_std y_std
#>   <dbl> <dbl> <dbl>    <dbl> <dbl>
#> 1     1    10   2          1 0.513
#> 2     1    10   0.5        1 1.9  
```
