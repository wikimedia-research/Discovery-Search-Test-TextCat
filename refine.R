library(magrittr)
library(tidyr)
import::from(dplyr, keep_where = filter, group_by, ungroup, summarize, select, rename, mutate, arrange, top_n, distinct, left_join)

events <- dplyr::tbl_df(readr::read_rds("data/textcat-enwiki-abtest-expanded.rds")) %>% keep_where(session_id != "")

valid_page_ids <- events %>%
  group_by(session_id, page_id) %>%
  summarize(`has SERP` = any(action == "searchResultPage")) %>%
  ungroup %>%
  keep_where(`has SERP`) %>%
  { .$page_id }

first_click_position <- function(actions, positions) {
  if (all(actions != "click")) {
    return(as.numeric(NA))
  } else {
    return(head(positions[actions == "click"], 1))
  }
}

first_click_is_interwiki <- function(actions, is_interwiki) {
  if (all(actions != "click")) {
    return(as.logical(NA))
  } else {
    return(head(is_interwiki[actions == "click"], 1))
  }
}

time_to_second_event <- function(x) {
  if (length(x) < 2) {
    return(as.numeric(NA))
  } else {
    return(as.numeric(difftime(x[2], x[1], units = "secs")))
  }
}

interwiki_click_position <- function(actions, positions, is_interwiki) {
  if (length(actions) == 1 && length(positions) == 1 && length(is_interwiki) == 1) {
    return(as.numeric(NA))
  } else if (all(actions != "click", na.rm = TRUE) || all(is.na(positions)) || all(!is_interwiki, na.rm = TRUE)) {
    return(as.numeric(NA))
  } else {
    output <- positions[actions == "click" & is_interwiki]
    return(output[1])
  }
}

any_clicks <- function(actions, is_interwiki, enwiki_only) {
  if (all(actions != "click")) {
    return(FALSE)
  } else {
    if (enwiki_only) {
      output <- any(actions == "click" & !is_interwiki, na.rm = TRUE)
    } else {
      output <- any(actions == "click" & is_interwiki, na.rm = TRUE)
    }
  }
  return(output)
}

searches <- events %>%
  select(-query) %>%
  keep_where(page_id %in% valid_page_ids) %>%
  group_by(session_id, page_id) %>%
  arrange(desc(action), ts) %>%
  summarize(
    test_group = test_group[1],
    wiki = wiki[1],
    n_clicks = sum(action == "click", na.rm = TRUE),
    clickthrough = n_clicks > 0,
    any_enwiki_clicks = any_clicks(action, interwiki_click, enwiki_only = TRUE),
    any_interwiki_clicks = any_clicks(action, interwiki_click, enwiki_only = FALSE),
    first_click_position = first_click_position(action, position_clicked),
    first_click_is_interwiki = first_click_is_interwiki(action, interwiki_click),
    time_to_first_click = time_to_second_event(ts),
    interwiki_position = interwiki_click_position(action, position_clicked, interwiki_click),
    detected_language = detected_language[1],
    results_returned = results_returned[1],
    new_results_returned = new_results_returned[1],
    zero_enwiki_results = results_returned - new_results_returned <= 0,
    zero_interwiki_results = new_results_returned == 0,
    zero_results = results_returned == 0) %>%
  ungroup
# ebernhardson: bearloga: so the third value is always the number of results returned by the alternate language query
## Note on Gerrit: The third value of extraParams should always be <= hitsReturned. In our logging of results this is
#                  not always the case. This turns out to be because users in the control group run the query, report
#                  the number of results, but are not shown the results. As such no fix was done here it was only a
#                  problem with how to interpret the data. (https://gerrit.wikimedia.org/r/#/c/294773/)

valid_session_ids <- searches %>%
  keep_where(
    (
      ((!zero_results) & (test_group %in% c("b (textcat)", "c (textcat + accept-lang)")))
      # For the test groups, we'll want to limit ourselves to searches that yielded some enwiki+interwiki results.
      | ((!zero_enwiki_results) & (test_group == "a (control)") & (results_returned < 3))
      # For controls, only matters if they got any enwiki results.
    ) & detected_language # It doesn't make sense comparing sessions for which we could not detect a language
  ) %>%
  select(session_id, test_group) %>%
  distinct %>%
  { .$session_id }

sessions <- searches %>%
  keep_where(session_id %in% valid_session_ids) %>%
  group_by(session_id) %>%
  summarize(test_group = head(test_group, 1),
            wiki = head(wiki, 1),
            SERPs = length(unique(page_id)),
            clickthrough = any(clickthrough),
            any_enwiki_clicks = any(any_enwiki_clicks),
            any_interwiki_clicks = any(any_interwiki_clicks))

save(list = c("events", "searches", "valid_session_ids", "sessions"),
     file = "data/textcat-enwiki-abtest-extended-refined.RData",
     compress = "gzip")

queries <- events %>%
  keep_where(page_id %in% valid_page_ids) %>%
  group_by(session_id, page_id) %>%
  arrange(desc(action), ts) %>%
  summarize(wiki_searched_original = wiki[1],
            wiki_searched_additional = wiki_searched[1],
            group = test_group[1], query = query[1],
            detected_language = detected_language[1],
            language_detected = language_detected[1],
            n_clicks = sum(action == "click", na.rm = TRUE),
            clickthrough = n_clicks > 0,
            interwiki_clickthrough = any_clicks(action, interwiki_click, enwiki_only = FALSE),
            combined_results_returned = results_returned[1],
            interwiki_results_returned = new_results_returned[1])
readr::write_tsv(queries, "data/textcat_queries_for_tjones.tsv")
system("gzip -f data/textcat_queries_for_tjones.tsv")
system("scp data/textcat_queries_for_tjones.tsv.gz stat2:/home/bearloga/")
