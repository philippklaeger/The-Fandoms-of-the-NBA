#' download_data.R
#'
#'
#'
#' Contributors: Philipp Klaeger
#' SNR: 2062105
#'
#' Purpose:
#' Collecting adequate Twitter data to examine the research question


# Step 1: Loading Libraries
library(data.table)
library(dplyr)
library(here)
library(rtweet)
library(readr)


# Step 2: Loading Seeds
seeds <- fread("seeds.csv")
seeds <- as.data.frame(seeds)


# Step 3: Collecting Tweets
# Tweets: 30,000 most recent tweets per collection day for all 30 NBA teams
# Collection Day I: May 7th 2021 - before the playoffs
tweets <- search_tweets2(q = seeds$Teams,
                         n = 100,
                         include_rts = FALSE,
                         retryonratelimit = TRUE,
                         `-filter` = "replies",
                         lang = "en")

tweets <- search_tweets2(q = seeds$Teams,
                        n = 1000,
                        include_rts = FALSE,
                        retryonratelimit = TRUE,
                        `-filter` = "replies",
                        lang = "en")


# Collection Day II: June 7th 2021 - within the playoffs
# tweets <- search_tweets2(q = seeds$Teams,
#                         n = 1000,
#                         include_rts = FALSE,
#                         retryonratelimit = TRUE,
#                         `-filter` = "replies",
#                         lang = "en")


# Step 4: Write Data to File
write.csv(subset(tweets, select = c(screen_name:reply_count, lang:country_code)),"tweets2.csv", row.names = FALSE)
tweets2 <- fread("tweets2.csv")
