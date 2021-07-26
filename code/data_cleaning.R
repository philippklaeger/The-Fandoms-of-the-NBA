#' data_cleaning.R
#'
#' What this file does:
#' Do any data cleaning in this script
#'

# CLEANING & PREPARATION



# 1. Loading Packages:
library(broom)
library(data.table)
library(dplyr)
library(fastDummies)
library(fixest)
library(ggplot2)
library(haven)
library(here)
library(kableExtra)
library(modelsummary)
library(purrr)
library(RColorBrewer) 
library(readr)
library(recipes)
library(rlist)
library(rtweet)
library(SnowballC)
library(syuzhet)
library(stringr)
library(tidyr)
library(tm)
library(wordcloud)


# 2. Loading Data:
tweets <- fread("tweets2.csv")
tweets <- as.data.frame(tweets)


# 3. Dropping Unnecessary Columns
df <- subset(tweets, select = c(screen_name, text, display_text_width, favorite_count, retweet_count))


# 4. Creating Team ID for the tweets
## Phase I: Detection - Hashtag, City, Franchise Name & Official Abbreviation
df <- mutate(df, team_id = case_when(str_detect(df$text, "atlantahawks|Atlanta|Hawks|ATL") ~ '1',
                                     str_detect(df$text, "bostonceltics|Boston|Celtics|BOS") ~ '2',
                                     str_detect(df$text, "brooklynnets|Brooklyn|Nets|BKN") ~ '3',
                                     str_detect(df$text, "charlottehornets|Charlotte|Hornets|CHA") ~ '4',
                                     str_detect(df$text, "chicagobulls|Chicago|Bulls|CHI") ~ '5',
                                     str_detect(df$text, "clevelandcavaliers|Cleveland|Cavaliers|CLE") ~ '6',
                                     str_detect(df$text, "dallasmavericks|Dallas|Mavericks|DAL") ~ '7',
                                     str_detect(df$text, "denvernuggets|Denver|Nuggets|DEN") ~ '8',
                                     str_detect(df$text, "detroitpistons|Detroit|Pistons|DET") ~ '9',
                                     str_detect(df$text, "goldenstatewarriors|GoldenState|SanFrancisco|Warriors|GSW") ~ '10',
                                     str_detect(df$text, "houstonrockets|Houston|Rockets|HOU") ~ '11',
                                     str_detect(df$text, "indianapacers|Indiana|Pacers|IND") ~ '12',
                                     str_detect(df$text, "losangelesclippers|laclippers|Clippers|LAC") ~ '13',
                                     str_detect(df$text, "losangeleslakers|lalakers|Lakers|LAL") ~ '14',
                                     str_detect(df$text, "memphisgrizzlies|Memphis|Grizzlies|MEM") ~ '15',
                                     str_detect(df$text, "miamiheat|Miami|Heat|MIA") ~ '16',
                                     str_detect(df$text, "milwaukeebucks|Milwaukee|Bucks|MIL") ~ '17',
                                     str_detect(df$text, "minnesotatimberwolves|Minessota|Timberwolves|MIN") ~ '18',
                                     str_detect(df$text, "neworleanspelicans|NewOrleans|Pelicans|NOP") ~ '19',
                                     str_detect(df$text, "newyorkknicks|NewYork|Knicks|NYK") ~ '20',
                                     str_detect(df$text, "oklahomacitythunder|okctunder|OklahomaCity|Thunder|OKC") ~ '21',
                                     str_detect(df$text, "orlandomagic|Orlando|Magic|ORL") ~ '22',
                                     str_detect(df$text, "philidelphia76ers|philadelphiasixers|
                                          Philadelphia|Sixers|76ers|PHI") ~ '23',
                                     str_detect(df$text, "phoenixsuns|Phoenix|Suns|PHO") ~ '24',
                                     str_detect(df$text, "portlandtrailblazers|Portland|TrailBlazers|POR") ~ '25',
                                     str_detect(df$text, "sacramentokings|Sacramento|Kings|SAC") ~ '26',
                                     str_detect(df$text, "sanantoniospurs|SanAntonio|Spurs|SAS") ~ '27',
                                     str_detect(df$text, "torontoraptors|Toronto|Raptors|TOR") ~ '28',
                                     str_detect(df$text, "utahjazz|Utah|Jazz|UTA") ~ '29',
                                     str_detect(df$text, "washingtonwizards|Washington|Wizards|WAS") ~ '30'))


## Phase II: Drop unidentifiable tweets
sum(is.na(df$team_id))
df <- df %>% 
    drop_na(team_id)


## Phase III: Dummy variables for each team
df$Hawks <- ifelse(df$team_id == '1', 1, 0)
df$Celtics <- ifelse(df$team_id == '2', 1, 0)
df$Nets <- ifelse(df$team_id == '3', 1, 0)
df$Hornets <- ifelse(df$team_id == '4', 1, 0)
df$Bulls <- ifelse(df$team_id == '5', 1, 0)
df$Cavaliers <- ifelse(df$team_id == '6', 1, 0)
df$Mavericks <- ifelse(df$team_id == '7', 1, 0)
df$Nuggets <- ifelse(df$team_id == '8', 1, 0)
df$Pistons <- ifelse(df$team_id == '9', 1, 0)
df$Warriors <- ifelse(df$team_id == '10', 1, 0)
df$Rockets <- ifelse(df$team_id == '11', 1, 0)
df$Pacers <- ifelse(df$team_id == '12', 1, 0)
df$Clippers <- ifelse(df$team_id == '13', 1, 0)
df$Lakers <- ifelse(df$team_id == '14', 1, 0)
df$Grizzlies <- ifelse(df$team_id == '15', 1, 0)
df$Heat <- ifelse(df$team_id == '16', 1, 0)
df$Bucks <- ifelse(df$team_id == '17', 1, 0)
df$Timberwolves <- ifelse(df$team_id == '18', 1, 0)
df$Pelicans <- ifelse(df$team_id == '19', 1, 0)
df$Knicks <- ifelse(df$team_id == '20', 1, 0)
df$Thunder <- ifelse(df$team_id == '21', 1, 0)
df$Magic <- ifelse(df$team_id == '22', 1, 0)
df$Sixers <- ifelse(df$team_id == '23', 1, 0)
df$Suns <- ifelse(df$team_id == '24', 1, 0)
df$TrailBlazers <- ifelse(df$team_id == '25', 1, 0)
df$Kings <- ifelse(df$team_id == '26', 1, 0)
df$Spurs <- ifelse(df$team_id == '27', 1, 0)
df$Raptors <- ifelse(df$team_id == '28', 1, 0)
df$Jazz <- ifelse(df$team_id == '29', 1, 0)
df$Wizards <- ifelse(df$team_id == '30', 1, 0)


# 5. Text Cleaning I - Data Frame
tweets_cleaned <-
    df %>%
    mutate(
        # remove links
        text = str_remove_all(text, "https\\S*"),
        text = str_remove_all(text, "http\\S*"),
        text = str_remove_all(text, "t.co*"),
        # remove mentions
        text = str_remove_all(text, "@\\S*"),
        # remove htmls
        text = str_remove_all(text, "amp"),
        text = str_remove_all(text, "&S*"),
        text = str_replace_all(text, "&#x27;|&quot;|&#x2F;", "'"),
        text = str_replace_all(text, "<a(.*?)>", " "),
        text = str_replace_all(text, "&gt;|&lt;|&amp;", " "),
        text = str_replace_all(text, "&#[:digit:]+;", " "),
        text = str_remove_all(text, "<[^>]*>"),
        # remove numbers
        text = str_remove_all(text, "[:digit:]"),
        # remove excess whitespace
        text = str_squish(text),
        text = str_trim(text)
    )

df <- as.data.frame(tweets_cleaned)


df$text <- as.character(df$text)                                        # remove unneeded special characters
df$text <- gsub("\\$", "", df$text) 
df$text <- gsub("@\\w+", "", df$text)
df$text <- gsub("[[:punct:]]","", df$text)
df$text <- gsub("http\\w+", "", df$text)
df$text <- gsub("[ |\t]{2,}", "", df$text)
df$text <- gsub("^ ", "", df$text)
df$text <- gsub(" $", "", df$text)
df$text <- gsub("RT","",df$text)
df$text <- gsub("href", "", df$text)
df$text <- gsub("([0-9])","", df$text)


# 6. Text Cleaning II - Corpus (package: tm)
NBA_corpus <- Corpus(VectorSource(df$text))
NBA_corpus <- tm_map(NBA_corpus, tolower)                               # all to lower cases
NBA_corpus <- tm_map(NBA_corpus, removePunctuation)                     # remove punctuation
NBA_corpus <- tm_map(NBA_corpus, removeNumbers)                         # remove numbers
NBA_corpus <- tm_map(NBA_corpus, removeWords, stopwords("english"))     # remove common english stopwords
NBA_corpus <- tm_map(NBA_corpus, stripWhitespace)                       # eliminate extra white spaces
NBA_corpus <- tm_map(NBA_corpus, stemDocument)                          # text stemming
tdm <- TermDocumentMatrix(NBA_corpus)                                   # create TermDocumentMatrix (tdm)


# 7. Saving Data
write.csv(df,"tweets_clean.csv", row.names = FALSE)
# save the *clean* data to the 'output' directory