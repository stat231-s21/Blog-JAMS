library(rtweet)
library(tidyverse)
library(tidytext)
library(textdata)

stop_words <- tidytext::stop_words


## scraping conducted on May 4th first
rt <- search_tweets(
  q = '"water access" OR "Water access" OR "Water Access" OR "access to clean water"
  OR "access to drinking water"',
  n = 5000,
  include_rts = FALSE
) %>%
  select(text)
# the above select command was run once when the csv file was created
# and the scraping was done again excludinng the select function
# run the below code to record the maximum value of created_at for the sake of reproducibility
time_data <- rt %>% select(created_at)
write_csv(time_data, 
          "/Users/masahiro/STAT231/git/Blog-JAMS/Data wrangling/tweets_reproducibility.csv")

##write_csv(rt, 
##          "/Users/masahiro/STAT231/git/Blog-JAMS/Data wrangling/tweetsMay4.csv")
### The above part is run on May 4th, and do not run the chunk again!


## from here, a scraping conducted on May 7th is enumerated
rt2 <- search_tweets(
  q = '"water access" OR "Water access" OR "Water Access" OR "access to clean water"
  OR "access to drinking water"',
  n = 5000,
  include_rts = FALSE
) %>%
  filter(created_at>max(rt$created_at))
# this filtering ensures that no duplicate tweets are included simultaneously
# the maximum value used for filtering was "2021-05-04 18:37:00 UTC"

## NOTE ##
# The search_tweets function does not allow ordinary users to access tweets
# more than 6-9 days ago. Thus, running the code above would not help you
# reproduce the same dataset

rt3 <- rt2 %>% select(text)

write_csv(rt3, 
          "/Users/masahiro/STAT231/git/Blog-JAMS/Data wrangling/tweetsMay7.csv")

# merge the dataset together
may4 <- read_csv("tweetsMay4.csv")
may7 <- read_csv("tweetsMay7.csv")

# combine them together and write out
tweets_for_blog_JAMS <- may4 %>%
  bind_rows(may7)
# write the dataset out
write_csv(tweets_for_blog_JAMS, 
          "/Users/masahiro/STAT231/git/Blog-JAMS/Data wrangling/tweetsfinal.csv")

