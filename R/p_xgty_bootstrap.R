#' Probability that vector x is element-wise greater than y by bootstrap.
#'
#' @description To estimate the confidence intervals for the posterior
#' probability that the event rate in the active treatment group is higher than
#' in the placebo group (Prob(Event rate in arm 2 > Event rate in arm 1)), the
#' function employs a bootstrap-like method. This involves generating samples
#' through circular shifts and comparing elements between the shifted versions
#' of x and y. Let I be the length of vectors x and y. For i varying from 1 to
#' I, the elements of the vector x are shifted to the left by i elements to
#' build the i-th sample. For each bootstrap sample, the proportion of elements
#' of x that are greater than the corresponding elements of y is computed. These
#' proportions are then used to estimate the mean, median, and standard
#' deviation for the calculated sample statistics. Highest density interval
#' (HDI) is also computed, which provides the lower and upper bounds of the
#' proportions for given confidence level alpha.
#'
#' @param x numerical vector.
#' @param y numerical vector.
#' @param offset We calculate Pr(x > y + offset). Default offset is 0.
#' @param alpha if not null, 1 - alpha confidence interval is calculated.
#'
#' @return tibble with columns mean and sd, and also .lower and .upper, if alpha
#' is not null, from the highest density posterior interval.
#'
#' @importFrom stats median qnorm sd
#' @importFrom purrr map_dbl
#' @importFrom utils head tail
#' @importFrom tibble tibble is_tibble
#' @importFrom dplyr mutate
#' @importFrom HDInterval hdi
#' @export
#'
#' @examples
#' set.seed(2023)
#' x <- rbeta(n = 1000, shape1 = 0.2, shape2 = 0.8)
#' y <- rbeta(n = 1000, shape1 = 0.1, shape2 = 0.9)
#' p_xgty_bootstrap(x, y)
#' p_xgty_bootstrap(x, y, offset = 0.05)
#' p_xgty_bootstrap(x, y, offset = 0.1)
#' p_xgty_bootstrap(x, y, offset = 0.2)
#' p_xgty_bootstrap(x, y, offset = 0.3)
p_xgty_bootstrap <- function(x, y, offset = 0, alpha = 0.05) {
  if (length(x) != length(y)) {
    stop("x and y must be the same length")
  }
  if (any(is.na(x)) | any(is.na(y))) {
    stop("Each element of x and y should be non-missing")
  }
  if ((!(is.null(alpha))) && !(alpha >= 0 && alpha <= 1)) {
    stop("non-null alpha must be between 0 and 1")
  }

  n <- length(x)
  p_xgty <- seq_along(x) %>%
    map_dbl(.f = function(i) {
      this_x <- c(tail(x, n - i), head(x, i))
      mean(this_x > y + offset)
    })
  tb <- tibble(
    mean = mean(p_xgty),
    median = median(p_xgty),
    sd = sd(p_xgty)
  )
  if (!is.null(alpha)) {
    z <- qnorm(p = alpha / 2, lower.tail = FALSE)
    p_xgty_hdi <- hdi(p_xgty, credMass = 1 - alpha)
    tb <- tb %>%
      mutate(
        .lower = p_xgty_hdi[1],
        .upper = p_xgty_hdi[2]
      )
  }
  tb
}
