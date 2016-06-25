library(magrittr)
library(tidyr)
import::from(dplyr, keep_where = filter, group_by, ungroup, summarize, select, rename, mutate, arrange, top_n, distinct, left_join)
library(ggplot2)
library(cowplot)
library(binom) # install.packages("binom")

flip_rows <- function(x) x[rev(1:nrow(x)), ]
flip_cols <- function(x) x[, rev(1:ncol(x))]

events %>%
  distinct(test_group, session_id) %>%
  group_by(test_group) %>%
  summarize(sessions = n()) %>%
  knitr::kable()

events %>%
  keep_where(interwiki_click) %>%
  group_by(session_id) %>%
  summarize(date = head(date, 1)) %>%
  group_by(date) %>%
  summarize(sessions = n())

# Comparing Enwiki clicks only
sessions %>%
  group_by(group = ifelse(test_group == "a (control)", "controls", "test"),
           any_enwiki_clicks) %>%
  summarize(sessions = n()) %>%
  xtabs(sessions ~ group + any_enwiki_clicks, data = .) %T>%
  { print(.) } %>%
  flip_cols %>%
  prop.table(margin = 1)
# Comparing enwiki+interwiki clicks across sessions with detected language
sessions %>%
  keep_where(session_id %in% valid_session_ids) %>%
  group_by(group = ifelse(test_group == "a (control)", "controls", "test"),
           any_clicks) %>%
  summarize(sessions = n()) %>%
  xtabs(sessions ~ group + any_clicks, data = .) %T>%
  { print(.) } %>%
  flip_cols %>%
  prop.table(margin = 1)
# Comparing enwiki clicks across sessions with detected language
sessions %>%
  keep_where(session_id %in% valid_session_ids) %>%
  group_by(group = ifelse(test_group == "a (control)", "controls", "test"),
           any_enwiki_clicks) %>%
  summarize(sessions = n()) %>%
  xtabs(sessions ~ group + any_enwiki_clicks, data = .) %T>%
  { print(.) } %>%
  flip_cols %>%
  prop.table(margin = 1)
  

sessions %>%
  mutate(ctr = clicked_SERPs/SERPs) %>%
  ggplot(aes(x = ctr, fill = test_group)) +
  geom_density(aes(y = ..count..), position = "fill") +
  # geom_density(aes(y = ..scaled..), alpha = 0.3) +
  scale_color_brewer(name = "Group", type = "qual", palette = "Set1") +
  scale_fill_brewer(name = "Group", type = "qual", palette = "Set1") +
  theme(legend.position = "bottom")

sessions %>%
  mutate(ctr = clicked_SERPs/SERPs) %>%
  ggplot(aes(x = ctr, color = test_group)) +
  geom_density() +
  scale_color_brewer(name = "Group", type = "qual", palette = "Set1") +
  theme(legend.position = "bottom")

sessions %>%
  group_by(test_group, clickthrough = clicked_SERPs > 0) %>%
  summarize(sessions = n()) %>%
  ungroup %>%
  spread(clickthrough, sessions) %>%
  mutate(CTR = `TRUE`/(`TRUE`+`FALSE`)) %>%
  {
    jeffreys_intervals <- binom.confint(x = .$`TRUE`, n = .$`TRUE` + .$`FALSE`, methods = "bayes")
    return(cbind(., jeffreys_intervals[, c("mean", "lower", "upper")]))
  } %>%
  ggplot(aes(y = CTR, x = test_group)) +
  geom_bar(stat = "identity", fill = "grey70") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_point(aes(y = mean), size = 2) +
  scale_y_continuous("Clickthrough Rate", labels = scales::percent_format()) +
  labs(title = "TextCat A/B Test: Clickthrough Rate by Group",
       subtitle = paste(sum(sessions$`TRUE` + sessions$`FALSE`),
                        "sessions for which we could detect an alternative language"),
       x = "Group") +
  ggthemes::theme_tufte(base_family = "Gill Sans")
