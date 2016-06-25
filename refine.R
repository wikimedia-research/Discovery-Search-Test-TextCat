library(magrittr)
library(tidyr)
import::from(dplyr, keep_where = filter, group_by, ungroup, summarize, select, rename, mutate, arrange, top_n, distinct, left_join)

events <- dplyr::tbl_df(readr::read_rds("data/textcat-enwiki-abtest.rds")) %>% keep_where(session_id != "")

valid_page_ids <- events %>%
  group_by(session_id, page_id) %>%
  summarize(`has SERP` = any(action == "searchResultPage")) %>%
  ungroup %>%
  keep_where(`has SERP`) %>%
  { .$page_id }

searches <- events %>%
  select(-query) %>%
  keep_where(page_id %in% valid_page_ids) %>%
  group_by(session_id, page_id) %>%
  arrange(desc(action), ts) %>%
  summarize(
    test_group = test_group[1],
    n_clicks = sum(action == "click"),
    any_clicks = "click" %in% action,
    any_enwiki_clicks = any(!interwiki_click[-1], na.rm = TRUE),
    any_interwiki_clicks = any(interwiki_click[-1], na.rm = TRUE),
    first_click_position = position_clicked[2],
    first_click_is_interwiki = interwiki_click[2],
    # interwiki_position = head(position_clicked[action == "click" & interwiki_click], 1),
    detected_language = detected_language[1],
    results_returned = results_returned[1],
    new_results_returned = new_results_returned[1],
    zero_enwiki_results = results_returned[1] - new_results_returned[1] <= 0,
    zero_interwiki_results = new_results_returned[1] == 0,
    zero_results = results_returned[1] == 0)
# ebernhardson: bearloga: so the third value is always the number of results returned by the alternate language query
## Note on Gerrit: The third value of extraParams should always be <= hitsReturned. In our logging of results this is
#                  not always the case. This turns out to be because users in the control group run the query, report
#                  the number of results, but are not shown the results. As such no fix was done here it was only a
#                  problem with how to interpret the data. (https://gerrit.wikimedia.org/r/#/c/294773/)

valid_session_ids <- searches %>%
  keep_where(
    (
      (!zero_results & test_group %in% c("b (textcat)", "c (textcat + accept-lang)"))
      # For the test groups, we'll want to limit ourselves to searches that yielded some enwiki+interwiki results.
      | (!zero_enwiki_results & test_group == "a (control)")
      # For controls, only matters if they got any enwiki results.
    ) & detected_language # It doesn't make sense comparing sessions for which we could not detect a language
  ) %>%
  select(session_id, test_group) %>%
  distinct %T>%
  { print(with(., table(test_group))) } %>%
  { .$session_id }

sessions <- searches %>%
  group_by(session_id) %>%
  summarize(test_group = head(test_group, 1),
            SERPs = length(unique(page_id)),
            any_clicks = any(any_clicks),
            any_enwiki_clicks = any(any_enwiki_clicks),
            any_interwiki_clicks = any(any_interwiki_clicks))
