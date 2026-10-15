# Probability that vector x is element-wise greater than y by bootstrap.

To estimate the confidence intervals for the posterior probability that
the event rate in the active treatment group is higher than in the
placebo group (Prob(Event rate in arm 2 \> Event rate in arm 1)), the
function employs a bootstrap-like method. This involves generating
samples through circular shifts and comparing elements between the
shifted versions of x and y. Let I be the length of vectors x and y. For
i varying from 1 to I, the elements of the vector x are shifted to the
left by i elements to build the i-th sample. For each bootstrap sample,
the proportion of elements of x that are greater than the corresponding
elements of y is computed. These proportions are then used to estimate
the mean, median, and standard deviation for the calculated sample
statistics. Highest density interval (HDI) is also computed, which
provides the lower and upper bounds of the proportions for given
confidence level alpha.

## Usage

``` r
p_xgty_bootstrap(x, y, offset = 0, alpha = 0.05)
```

## Arguments

- x:

  numerical vector.

- y:

  numerical vector.

- offset:

  We calculate Pr(x \> y + offset). Default offset is 0.

- alpha:

  if not null, 1 - alpha confidence interval is calculated.

## Value

tibble with columns mean and sd, and also .lower and .upper, if alpha is
not null, from the highest density posterior interval.

## Examples

``` r
set.seed(2023)
x <- rbeta(n = 1000, shape1 = 0.2, shape2 = 0.8)
y <- rbeta(n = 1000, shape1 = 0.1, shape2 = 0.9)
p_xgty_bootstrap(x, y)
#> # A tibble: 1 × 5
#>    mean median      sd .lower .upper
#>   <dbl>  <dbl>   <dbl>  <dbl>  <dbl>
#> 1 0.684  0.684 0.00896  0.666    0.7
p_xgty_bootstrap(x, y, offset = 0.05)
#> # A tibble: 1 × 5
#>    mean median      sd .lower .upper
#>   <dbl>  <dbl>   <dbl>  <dbl>  <dbl>
#> 1 0.413  0.413 0.00635    0.4  0.424
p_xgty_bootstrap(x, y, offset = 0.1)
#> # A tibble: 1 × 5
#>    mean median      sd .lower .upper
#>   <dbl>  <dbl>   <dbl>  <dbl>  <dbl>
#> 1 0.348  0.348 0.00551  0.337  0.358
p_xgty_bootstrap(x, y, offset = 0.2)
#> # A tibble: 1 × 5
#>    mean median      sd .lower .upper
#>   <dbl>  <dbl>   <dbl>  <dbl>  <dbl>
#> 1 0.275  0.275 0.00519  0.264  0.284
p_xgty_bootstrap(x, y, offset = 0.3)
#> # A tibble: 1 × 5
#>    mean median      sd .lower .upper
#>   <dbl>  <dbl>   <dbl>  <dbl>  <dbl>
#> 1 0.226  0.226 0.00505  0.217  0.236
```
