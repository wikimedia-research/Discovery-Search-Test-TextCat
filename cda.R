library(magrittr)
library(tidyr)
import::from(dplyr, keep_where = filter, group_by, ungroup, summarize, select, rename, mutate, arrange, top_n, distinct, left_join)

events <- dplyr::tbl_df(readr::read_rds("data/textcat-enwiki-abtest.rds")) %>% keep_where(session_id != "")

flip_rows <- function(x) x[rev(1:nrow(x)), ]
flip_cols <- function(x) x[, rev(1:ncol(x))]

valid_seshs <- events %>%
  keep_where((results_returned > 0 | new_results_returned > 0) & new_results_returned >= results_returned) %>%
  select(session_id, test_group) %>%
  distinct %T>%
  { print(with(., table(test_group))) } %>%
  { .$session_id }

ct_by_group <- events %>%
  keep_where(session_id %in% valid_seshs) %>%
  # We only want to compare searches where TextCat (or TextCat + A-L detection) could detect an alternative language:
  keep_where(!is.na(language_detected)) %>%
  group_by(session_id, action) %>%
  summarize(events = n(), test_group = head(test_group, 1)) %>%
  ungroup %>%
  tidyr::spread(action, events, fill = 0) %>%
  # ^ Gets us to session_id, click, searchResultPage counts
  # Some of the sessions had some clicks but no SERP events, so we want to filter those out:
  keep_where(searchResultPage > 0) %>%
  # Then we count sessions by group & clickthrough:
  group_by(test_group, clickthrough = click > 0) %>%
  summarize(sessions = n()) %>%
  ungroup %>%
  # Turn the data frame of counts into a contingency table:
  xtabs(sessions ~ test_group + clickthrough, data = .) %>%
  flip_cols

set.seed(0)

# Compare A vs B
ct_by_group %>% { .[1:2, ] } %>%
  flip_rows %>%
  BCDA::beta_binom() %T>%
  { print(BCDA:::summary.beta_binomial_fit(., interval_type = "HPD")) } %>%
  BCDA:::plot.beta_binomial_fit()
# 3.35% (1.3-6.5) greater success probability; 1.49 (0.96-2.02) times more likely

# Compare A vs C
ct_by_group %>% { .[c(1,3), ] } %>%
  flip_rows %>%
  BCDA::beta_binom() %T>%
  { print(BCDA:::summary.beta_binomial_fit(., interval_type = "HPD")) } %>%
  BCDA:::plot.beta_binomial_fit()
# Slightly greater success probability of 3.76% (0.9-6.45); 1.546 (1.06-2.06) times more likely

# Compare B vs C
ct_by_group %>% { .[2:3, ] } %>%
  flip_rows %>%
  BCDA::beta_binom() %T>%
  { print(BCDA:::summary.beta_binomial_fit(., interval_type = "HPD")) } %>%
  BCDA:::plot.beta_binomial_fit()
# Not a significant difference of 0.39% (-2.78, 3.45); 1.05 times more likely (0.751-1.36)
