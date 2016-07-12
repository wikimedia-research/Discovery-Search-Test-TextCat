library(magrittr)
library(tidyr)
import::from(dplyr, keep_where = filter, group_by, ungroup, summarize, select, rename, mutate, arrange, top_n, distinct, left_join, tally)
library(ggplot2)
library(cowplot)
library(BCDA)
import::from(BCDA, flip_cols)
import::from(BCDA, flip_rows)

options(digits = 3)

load("data/textcat-enwiki-abtest-refined.RData")

# Number of searches performed in sessions with detected language
sessions %>%
  ggplot(aes(y = SERPs, x = test_group)) +
  geom_violin(fill = "gray90") +
  geom_boxplot(width = 0.2) +
  scale_y_log10() +
  labs(y = "Number of searches performed per session", x = NULL)

# Comparing Enwiki clicks only -- across searches
searches %>%
  keep_where(session_id %in% valid_session_ids) %>%
  mutate(any_enwiki_clicks = ifelse(any_enwiki_clicks, "Clicked an enwiki result", "Did not")) %>%
  group_by(group = ifelse(test_group == "a (control)", "controls", "test"),
           any_enwiki_clicks) %>%
  tally() %>%
  xtabs(n ~ group + any_enwiki_clicks, data = .) %>%
  prop.table(margin = 1) %>%
  knitr::kable()

# Comparing enwiki+interwiki clicks across sessions with detected language -- across searches
searches %>%
  keep_where(session_id %in% valid_session_ids) %>%
  mutate(clickthrough = ifelse(clickthrough, "Clicked a result", "Did not")) %>%
  group_by(group = ifelse(test_group == "a (control)", "controls", "test"),
           clickthrough) %>%
  tally() %>%
  xtabs(n ~ group + clickthrough, data = .) %>%
  prop.table(margin = 1) %>%
  knitr::kable()

# Comparing enwiki CTR in controls vs interwiki CTR in sessions with detected language
sessions %>%
  mutate(new_clickthrough = (test_group == "a (control)" & any_enwiki_clicks) | (test_group %in% c("b (textcat)", "c (textcat + accept-lang)") & any_interwiki_clicks)) %>%
  group_by(group = ifelse(test_group == "a (control)", "controls", "test"),
           new_clickthrough) %>%
  tally() %>%
  xtabs(n ~ group + new_clickthrough, data = .) %>%
  # prop.table(margin = 1) %>%
  knitr::kable()

# Time to first click...appears to be the same
searches %>%
  keep_where(session_id %in% valid_session_ids) %>%
  group_by(group = test_group, session_id) %>%
  summarize(secs_to_first_click = min(time_to_first_click, na.rm = TRUE)) %>%
  ungroup %>%
  keep_where(!is.na(secs_to_first_click) & secs_to_first_click > 0) %>%
  ggplot(aes(y = secs_to_first_click, x = group)) +
  geom_violin(fill = "gray80") +
  geom_boxplot(width = 0.2) +
  scale_y_log10()
