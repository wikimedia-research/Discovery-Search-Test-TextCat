library(magrittr)
library(tidyr)
import::from(dplyr, keep_where = filter, group_by, ungroup, summarize, select, rename, mutate, arrange, top_n, distinct, left_join, tally)
library(ggplot2)
library(cowplot)
library(BCDA)
import::from(BCDA, flip_cols)
import::from(BCDA, flip_rows)

options(digits = 3)

load("data/textcat-enwiki-abtest-extended-refined.RData")

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

searches %>%
  keep_where(detected_language, test_group != "a (control)") %>%
  keep_where(!zero_interwiki_results & !zero_enwiki_results, clickthrough) %>%
  mutate(`same-wiki results` = results_returned - new_results_returned) %>%
  group_by(`same-wiki results`, first_click_position, `first clicked on` = ifelse(first_click_is_interwiki, "an interwiki result", "a same-wiki result")) %>%
  tally() %>%
  ungroup %>%
  mutate(first_click_position = ifelse(first_click_position <= 3, first_click_position, 4)) %>%
  group_by(`same-wiki results`, `first clicked on`, first_click_position) %>%
  summarize(n = sum(n)) %>%
  mutate(position = paste(to_ordinal(first_click_position) , 'result')) %>%
  keep_where((`first clicked on` == "a same-wiki result" & first_click_position <= `same-wiki results`) | (`first clicked on` == "an interwiki result")) %>%
  select(-first_click_position) %>%
  spread(position, n, fill = 0) %>%
  ungroup %>%
  {
    names(.) <- sub("4th result", "4th result or lower", names(.))
    .
  } %>%
  # results[, -(1:2)] <- add_props(results[, -(1:2)], 1:2)
  lapply(., . %>% gsub("^0$", "--", .)) %>%
  dplyr::as_data_frame() %>%
  knitr::kable(caption = "Counts of clickthroughs by number of same-wiki results shown, type of result clicked first, and the position of the result (1st, 2nd, 3rd, 4th or lower). The positions are independent, although the 1st interwiki result is the 2nd result shown on the page (in case of 1 same-language wiki result) or 3rd result (in case of 2 same-language wiki results). There are two technical anomalies that have a '4th result or lower' as a same-wiki result, but the maximum position that a same-wiki result can have is technically '2nd'.")

# An interesting thing to note is that of the 1647 searches made by the two test
# groups 'b' and 'c' that had (1) had a language detected, and (2) a clickthrough:
# only 37.64% (620) searches had both interwiki and same-wiki results. We were
# curious how those users engaged with their mixed results, so we looked into and
# found that users almost overwhelmingly initially clicked on the 1-2 same-wiki
# results that show up first, rather than the interwiki results shown underneath
# the same-wiki ones. This is not a particularly new finding, as we have known and
# seen for a while that a vast majority of users just click on the first couple
# results. This is interesting because it introduces two interesting possibilities:
# either the interwiki results are not as relevant as we hope them to be, or users
# just click on the first or second result even if the results underneath are
# actually, objectively more relevant because they were fetched from the Wikipedia
# of the query's language.
