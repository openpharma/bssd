
# n = c(1.5, 10, 5)
# y = c(1, 3, 2)
# time = c(1, 1, 1)
# time_std = 1
# # y_std <-
# n * (
#   1 - (
#     1 - (y / n)
#   )^(time_std / time)
# )
base_durpln_df <- adbayssd_sl_ongoing %>%
  distinct(DURPLN)
# There should be exactly one DURPLAN for the ongoing study:
expect_equal(nrow(base_durpln_df), 1)
base_durpln <- base_durpln_df %>% pull(DURPLN)

# SC ----
# hist_chi_squ_sc <- adbayssd_sl_hist %>%
#   group_by(STUDYID, DURPLN) %>%
#   summarise(
#     n = sum(MXPROTAR),
#     n_DURPLAN = n_distinct(DURPLN),
#     time = median(DURPLN), # There should be one per study. Checked below.
#     .groups = "drop"
#   ) %>%
#   left_join(
#     adbayssd_adae_hist %>%
#       filter(!!sym(event_analysis_flag) == "Y") %>%
#       group_by(STUDYID) %>%
#       summarise(y = n_distinct(USUBJID), .groups = "drop"),
#     by = "STUDYID"
#   ) %>%
#   replace_na(list(y = 0)) %>%
#   mutate(trial = as.integer(as.factor(STUDYID)))
hist_chi_squ_sc <- adbayssd_sl_hist %>%
  group_by(STUDYID, time = DURPLN) %>%
  summarise(n = sum(MXPROTAR), .groups = "drop") %>%
  left_join(
    adbayssd_adae_hist %>%
      filter(!!sym(event_analysis_flag) == "Y") %>%
      group_by(STUDYID) %>%
      summarise(y = n_distinct(USUBJID), .groups = "drop"),
    by = "STUDYID"
  ) %>%
  replace_na(list(y = 0)) %>%
  mutate(trial = as.integer(as.factor(STUDYID)))
hist_chi_squ_sc <- hist_chi_squ_sc %>%
  mutate(y_std = get_std_event_counts(y, n, time, time_std = base_durpln))
cs_prior1 <- chi_squ_prior(
  y = hist_chi_squ_sc$y,
  n  = hist_chi_squ_sc$n,
  time = hist_chi_squ_sc$time,
  time_std = base_durpln
)

# MT ----
hist_chi_squ_mt <- adbayssd_sl_hist %>%
  group_by(STUDYID) %>%
  summarise(
    n = n_distinct(USUBJID),
    time = mean(TARCAP)
  ) %>%
  left_join(
    adbayssd_adae_hist %>%
      filter(!!sym(event_analysis_flag) == "Y") %>%
      group_by(STUDYID) %>%
      summarise(y = n_distinct(USUBJID), .groups = "drop"),
    by = "STUDYID"
  ) %>%
  replace_na(list(y = 0)) %>%
  mutate(trial = as.integer(as.factor(STUDYID)))
cs_prior_mt <- chi_squ_prior(
  y = hist_chi_squ_mt$y,
  n  = hist_chi_squ_mt$n,
  time = hist_chi_squ_mt$time,
  time_std = base_durpln,
  alpha = chi_squ_alpha
)
