start_date <- as.Date("2016-05-16")
end_date <- as.Date("2016-05-23") # or when we turn the test off...
cat("Fetching EL data from", as.character(start_date), "to", as.character(end_date), "...\n")
events <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching EL data from", as.character(date), "\n")
  data <- wmf::build_query("SELECT LEFT(timestamp, 8) AS date,
                              timestamp AS ts,
                              userAgent AS user_agent,
                              event_mwSessionId AS session_id,
                              event_subTest AS test_group,
                              event_pageViewId AS page_id,
                              event_action AS action,
                              event_hitsReturned AS results_returned,
                              event_position AS position_clicked,
                              event_extraParams AS additional_data",
                           date = date,
                           table = "TestSearchSatisfaction2_15357244",
                           conditionals = "(LEFT(event_subTest, 7) = 'textcat')
                             AND ((event_action = 'click' AND event_position IS NOT NULL) OR (event_action = 'searchResultPage' AND event_hitsReturned IS NOT NULL))
                             AND event_source = 'fulltext'")
  return(data)
}))
library(magrittr)
events$date %<>% lubridate::ymd()
events$ts %<>% lubridate::ymd_hms()

# Address the bug with extra parameters field wherein the number of results was not included if it was 0:
count_extra_params <- function(x) {
  return(nchar(x) - nchar(gsub(",", "", x, fixed = TRUE)) + 1)
}
events$n_extra_params <- count_extra_params(events$additional_data)
events$additional_data[events$n_extra_params == 2 & events$additional_data != ""] <- paste0(events$additional_data[events$n_extra_params == 2 & events$additional_data != ""], ",0")
events$n_extra_params <- NULL
rm(count_extra_params)

# Rename the test groups:
events$test_group <- sub("textcat1:a", "a (control)", events$test_group, fixed = TRUE)
events$test_group <- sub("textcat1:b", "b (textcat)", events$test_group, fixed = TRUE)
events$test_group <- sub("textcat1:c", "c (textcat + accept-lang)", events$test_group, fixed = TRUE)

# Process the extra parameters field:
events$additional_data[events$additional_data == ""] <- "NA,NA,NA"
additional_data <- events$additional_data %>%
  strsplit(., ",", fixed = TRUE) %>%
  do.call(rbind, .) %>%
  apply(2, function(column) {
    return(replace(column, column == "NA", as.character(NA)))
  }) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  set_colnames(c("language_detected", "wiki_searched", "new_results_returned")) %>%
  { .$new_results_returned <- as.numeric(.$new_results_returned); . }
events <- cbind(events, additional_data)
events$additional_data <- NULL
events$detected_language <- !is.na(events$language_detected)

# Remove known spiders from the dataset (usually <10, but still):
devices <- uaparser::parse_agents(events$user_agent, "device")
spider_session_ids <- unique(events$session_id[devices == "Spider"])
events <- events[!(events$session_id %in% spider_session_ids), ]
rm(devices, spider_session_ids)
events$user_agent <- NULL

readr::write_rds(events, "textcat-enwiki-abtest.rds", "gz")
