
#' Get follow-up times where all patients are followed up as expected.
#'
#' @param sample_size integer, number of follow-up times to
#' @param follow_up_time numeric, expected duration of follow-up.
#'
#' @return numerical
#' @export
#'
#' @examples
#' complete_follow_up(10, 1.2)
complete_follow_up <- function(sample_size, follow_up_time) {
  return(rep(follow_up_time, sample_size))
}
