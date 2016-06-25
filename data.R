start_date <- as.Date("2016-06-16")
end_date <- Sys.Date() # Sys.Date()-1
cat("Fetching EL data from", as.character(start_date), "to", as.character(end_date), "...\n")
events <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching EL data from", as.character(date), "\n")
  data <- wmf::build_query("SELECT LEFT(timestamp, 8) AS date,
                              event_uniqueId AS event_id,
                              timestamp AS ts,
                              userAgent AS user_agent,
                              event_searchSessionId AS session_id,
                              event_mwSessionId AS mediawiki_id,
                              event_subTest AS test_group,
                              event_pageViewId AS page_id,
                              event_action AS action,
                              event_hitsReturned AS results_returned,
                              event_msToDisplayResults AS time_to_display,
                              event_position AS position_clicked,
                              event_extraParams AS additional_data,
                              event_searchToken AS CirrusSearchRequestSets_id,
                              CAST(event_query AS CHAR CHARACTER SET utf8) AS query",
                           date = date,
                           table = "TestSearchSatisfaction2_15700292",
                           conditionals = "(LEFT(event_subTest, 7) = 'textcat')
                             AND ((event_action = 'click' AND event_position IS NOT NULL) OR (event_action = 'searchResultPage' AND event_hitsReturned IS NOT NULL))
                             AND event_source = 'fulltext' AND wiki = 'enwiki'")
  return(data)
}))
events <- events[!duplicated(events$event_id, fromLast = FALSE), ]; events$event_id <- NULL
library(magrittr)
events$date %<>% lubridate::ymd()
events$ts %<>% lubridate::ymd_hms()

Encoding(events$query) <- "UTF-8"
events$interwiki_click <- events$action == "click" & events$additional_data == "1"
events$interwiki_click[events$action != "click"] <- NA
events$additional_data[events$additional_data == ",,"] <- NA

# Remove known spiders from the dataset (usually <10, but still):
devices <- uaparser::parse_agents(events$user_agent, "device")
spider_session_ids <- unique(events$session_id[devices == "Spider"])
events <- events[!(events$session_id %in% spider_session_ids), ]
rm(devices, spider_session_ids)
events$user_agent <- NULL

# Rename the test groups:
events$test_group <- sub("textcat2:a", "a (control)", events$test_group, fixed = TRUE)
events$test_group <- sub("textcat2:b", "b (textcat)", events$test_group, fixed = TRUE)
events$test_group <- sub("textcat2:c", "c (textcat + accept-lang)", events$test_group, fixed = TRUE)

# Process the extra parameters field:
additional_data <- events$additional_data %>%
  strsplit(., ",", fixed = TRUE) %>%
  do.call(rbind, .) %>%
  apply(2, function(column) {
    return(replace(column, column == "NA", as.character(NA)))
  }) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  set_colnames(c("language_detected", "wiki_searched", "new_results_returned")) %>%
  { .$new_results_returned <- as.numeric(.$new_results_returned); . }
additional_data$language_detected[events$action == "click"] <- NA
additional_data$wiki_searched[events$action == "click"] <- NA
events <- cbind(events, additional_data)
events$additional_data <- NULL
events$detected_language <- !is.na(events$language_detected)

events <- dplyr::arrange(events, date, ts, session_id, mediawiki_id, test_group,
                         page_id, desc(action), ts)

readr::write_rds(events, "~/textcat-enwiki-abtest.rds", "gz")

dir.create("data")
system("scp stat2:/home/bearloga/textcat-enwiki-abtest.rds data/")
