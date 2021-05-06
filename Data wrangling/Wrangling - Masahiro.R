library(rtweet)
library(tidyverse)
library(tidytext)
library(textdata)

stop_words <- tidytext::stop_words

rt <- search_tweets(
  q = '"water access" OR "Water access" OR "Water Access" OR "access to clean water"
  OR "access to drinking water"',
  n = 5000,
  include_rts = FALSE
) %>%
  select(text)

##write_csv(rt, 
##          "/Users/masahiro/STAT231/git/Blog-JAMS/Data wrangling/tweetsMay4.csv")
### The above part is run on May 4th, and do not run the chunk again!

tweets_final <- rt %>%
  select(text) %>%
  unnest_tokens(output = word, input = text, token = "words") %>%
  anti_join(stop_words, by = "word")

