# -------------------------------------------------------------------------
# File: twitter_examples.R
#
# Description:
# Collect examples from Twitter of *BE sat/stood* for illustration
# -------------------------------------------------------------------------


library(tidyverse)
library(rtweet) # for interacting with Twitter's API

# define the search queries
sat_q <- '"am sat" OR "is sat" OR "are sat" OR "was sat" OR "were sat"'
stood_q <- '"am stood" OR "is stood" OR "are stood" OR "was stood" OR "were stood"'

sat_tweets <- search_tweets(
  q = sat_q,
  n = 100, # 100 most recent tweets
  geocode = "52.33,-1.9025,15mi", # search within 15 mile radius of Birmingham
  include_rts = FALSE
)


sat_tweets %>%
  pull(text) %>%
  head(10)

stood_tweets <- search_tweets(
  q = stood_q,
  n = 100,
  geocode = "52.33,-1.9025,50mi", # within 50 mile radius of Birmingham
  include_rts = FALSE
)

stood_tweets %>%
  pull(text) %>%
  head(10)

# DO NOT RUN!
# stream tweets for a week (60 secs * 60 mins * 24 hours *  7 days)
stream_tweets(
  q = sat_q,
  timeout = 60 * 60 * 24 * 7,
  file_name = here::here("data_raw", "live_be_sat_tweets.json"),
  parse = FALSE
)


