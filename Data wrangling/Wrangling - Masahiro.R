library(rtweet)
library(tidyverse)
library(tidytext)
library(textdata)

stop_words <- tidytext::stop_words

rt <- search_tweets(
  q = '"water access" OR "Water access" OR "Water Access" OR "access to clean water"',
  n = 5000,
  include_rts = FALSE
)

tweets_final <- rt %>%
  select(text) %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  anti_join(stop_words, by = "word")