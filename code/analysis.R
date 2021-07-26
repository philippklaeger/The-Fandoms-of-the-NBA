#' analysis.R
#'
#' What this file does:
#' Do your main analysis in this script
#'

# 1. Load Libraries
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
tweets <- fread("tweets_clean.csv")
tweets <- as.data.frame(tweets)


# EXPLORATORY DATA ANAYLSIS



# NBA Word Cloud
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud <- wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                       max.words=200, random.order=FALSE, rot.per=0.35, 
                       colors=brewer.pal(8, "Dark2"))

# Descriptives
cm1 <- c('NUnique' = 'Unique (#)',
         'screen_name' = 'Profile Name',
         'text' = 'Tweet Content',
         'display_text_width' = 'Character Length',
         'favorite_count' = 'Number of Favorites',
         'retweet_count' = 'Number of Retweets')

df2 <- subset(df, select = c(screen_name, text, display_text_width, favorite_count, retweet_count))

summary1 <- datasummary_skim(df2, title = "Sample Overview (N=10765)", coef_map = cm1)



#summary1 <- datasummary_df(
#  display_text_width, favorite_count, retweet_count
#  ~ Ncol + NUnique + NPercent + Min + Max + Mean + Var + SD,
#  data = df,
#  title = 'Sentiment Analysis NBA - Western Conference',
#  coef_map = cm1)
#summary1


# ANALYSIS



# 1. Sentiment Analysis I
sentiment_nrc <- get_nrc_sentiment(df$text, language = 'english')
df$anger <- sentiment_nrc$anger
df$anticipation <- sentiment_nrc$anticipation
df$disgust <- sentiment_nrc$disgust
df$fear <- sentiment_nrc$fear
df$joy <- sentiment_nrc$joy
df$sadness <- sentiment_nrc$sadness
df$surprise <- sentiment_nrc$surprise
df$trust <- sentiment_nrc$trust
df$negative <- sentiment_nrc$negative
df$positive <- sentiment_nrc$positive

sentiment_nrc <- as.data.frame(sentiment_nrc)


## 1.1  Regression Models - Lists
### 1.1.1   List 1 - Entire League
models1 <- list(
    "Atlanta Hawks" = lm(Hawks ~ anger + anticipation + disgust + fear + joy + 
                             sadness + surprise + trust + negative + positive, data = df),
    "Boston Celtics" = lm(Celtics ~ anger + anticipation + disgust + fear + joy + 
                              sadness + surprise + trust + negative + positive, data = df),
    "Brooklyn Nets" = lm(Nets ~ anger + anticipation + disgust + fear + joy + 
                             sadness + surprise + trust + negative + positive, data = df),
    "Charlotte Hornets" = lm(Hornets ~ anger + anticipation + disgust + fear + joy + 
                                 sadness + surprise + trust + negative + positive, data = df),
    "Chicago Bulls" = lm(Bulls ~ anger + anticipation + disgust + fear + joy + 
                             sadness + surprise + trust + negative + positive, data = df),
    "Cleveland Cavaliers" = lm(Cavaliers ~ anger + anticipation + disgust + fear + joy + 
                                   sadness + surprise + trust + negative + positive, data = df),
    "Dallas Mavericks" = lm(Mavericks ~ anger + anticipation + disgust + fear + joy + 
                                sadness + surprise + trust + negative + positive, data = df),
    "Denver Nuggets" = lm(Nuggets ~ anger + anticipation + disgust + fear + joy + 
                              sadness + surprise + trust + negative + positive, data = df),
    "Detroit Pistons" = lm(Pistons ~ anger + anticipation + disgust + fear + joy + 
                               sadness + surprise + trust + negative + positive, data = df),
    "Golden State Warriors" = lm(Warriors ~ anger + anticipation + disgust + fear + joy + 
                                     sadness + surprise + trust + negative + positive, data = df),
    "Houston Rockets" = lm(Rockets ~ anger + anticipation + disgust + fear + joy + 
                               sadness + surprise + trust + negative + positive, data = df),
    "Indiana Pacers" = lm(Pacers ~ anger + anticipation + disgust + fear + joy + 
                              sadness + surprise + trust + negative + positive, data = df),
    "Los Angeles Clippers" = lm(Clippers ~ anger + anticipation + disgust + fear + joy + 
                                    sadness + surprise + trust + negative + positive, data = df),
    "Los Angeles Lakers" = lm(Lakers ~ anger + anticipation + disgust + fear + joy + 
                                  sadness + surprise + trust + negative + positive, data = df),
    "Memphis Grizzlies" = lm(Grizzlies ~ anger + anticipation + disgust + fear + joy + 
                                 sadness + surprise + trust + negative + positive, data = df),
    "Miami Heat" = lm(Heat ~ anger + anticipation + disgust + fear + joy + 
                          sadness + surprise + trust + negative + positive, data = df),
    "Milwaukee Bucks" = lm(Bucks ~ anger + anticipation + disgust + fear + joy + 
                               sadness + surprise + trust + negative + positive, data = df),
    "Minnesota Timberwolves" = lm(Timberwolves ~ anger + anticipation + disgust + fear + joy + 
                                      sadness + surprise + trust + negative + positive, data = df),
    "New Orleans Pelicans" = lm(Pelicans ~ anger + anticipation + disgust + fear + joy + 
                                    sadness + surprise + trust + negative + positive, data = df),
    "New York Knicks" = lm(Knicks ~ anger + anticipation + disgust + fear + joy + 
                               sadness + surprise + trust + negative + positive, data = df),
    "Oklahoma City Thunder" = lm(Thunder ~ anger + anticipation + disgust + fear + joy + 
                                     sadness + surprise + trust + negative + positive, data = df),
    "Orlando Magic" = lm(Magic ~ anger + anticipation + disgust + fear + joy + 
                             sadness + surprise + trust + negative + positive, data = df),
    "Philadelphia 76ers" = lm(Sixers ~ anger + anticipation + disgust + fear + joy + 
                                  sadness + surprise + trust + negative + positive, data = df),
    "Phoenix Suns" = lm(Suns ~ anger + anticipation + disgust + fear + joy + 
                            sadness + surprise + trust + negative + positive, data = df),
    "Portland Trail Blazers" = lm(TrailBlazers ~ anger + anticipation + disgust + fear + joy + 
                                      sadness + surprise + trust + negative + positive, data = df),
    "Sacramento Kings" = lm(Kings ~ anger + anticipation + disgust + fear + joy + 
                                sadness + surprise + trust + negative + positive, data = df),
    "San Antonio Spurs" = lm(Spurs ~ anger + anticipation + disgust + fear + joy + 
                                 sadness + surprise + trust + negative + positive, data = df),
    "Toronto Raptors" = lm(Raptors ~ anger + anticipation + disgust + fear + joy + 
                               sadness + surprise + trust + negative + positive, data = df),
    "Utah Jazz" = lm(Jazz ~ anger + anticipation + disgust + fear + joy + 
                         sadness + surprise + trust + negative + positive, data = df),
    "Washington Wizards" = lm(Wizards ~ anger + anticipation + disgust + fear + joy + 
                                  sadness + surprise + trust + negative + positive, data = df)
)


### 1.1.2 List 2 - Eastern Conference
models2 <- list(
    "Atlanta Hawks" = lm(Hawks ~ anger + anticipation + disgust + fear + joy + 
                             sadness + surprise + trust + negative + positive, data = df),
    "Boston Celtics" = lm(Celtics ~ anger + anticipation + disgust + fear + joy + 
                              sadness + surprise + trust + negative + positive, data = df),
    "Brooklyn Nets" = lm(Nets ~ anger + anticipation + disgust + fear + joy + 
                             sadness + surprise + trust + negative + positive, data = df),
    "Charlotte Hornets" = lm(Hornets ~ anger + anticipation + disgust + fear + joy + 
                                 sadness + surprise + trust + negative + positive, data = df),
    "Chicago Bulls" = lm(Bulls ~ anger + anticipation + disgust + fear + joy + 
                             sadness + surprise + trust + negative + positive, data = df),
    "Cleveland Cavaliers" = lm(Cavaliers ~ anger + anticipation + disgust + fear + joy + 
                                   sadness + surprise + trust + negative + positive, data = df),
    "Detroit Pistons" = lm(Pistons ~ anger + anticipation + disgust + fear + joy + 
                               sadness + surprise + trust + negative + positive, data = df),
    "Indiana Pacers" = lm(Pacers ~ anger + anticipation + disgust + fear + joy + 
                              sadness + surprise + trust + negative + positive, data = df),
    "Miami Heat" = lm(Heat ~ anger + anticipation + disgust + fear + joy + 
                          sadness + surprise + trust + negative + positive, data = df),
    "Milwaukee Bucks" = lm(Bucks ~ anger + anticipation + disgust + fear + joy + 
                               sadness + surprise + trust + negative + positive, data = df),
    "New York Knicks" = lm(Knicks ~ anger + anticipation + disgust + fear + joy + 
                               sadness + surprise + trust + negative + positive, data = df),
    "Orlando Magic" = lm(Magic ~ anger + anticipation + disgust + fear + joy + 
                             sadness + surprise + trust + negative + positive, data = df),
    "Philadelphia 76ers" = lm(Sixers ~ anger + anticipation + disgust + fear + joy + 
                                  sadness + surprise + trust + negative + positive, data = df),
    "Toronto Raptors" = lm(Raptors ~ anger + anticipation + disgust + fear + joy + 
                               sadness + surprise + trust + negative + positive, data = df),
    "Washington Wizards" = lm(Wizards ~ anger + anticipation + disgust + fear + joy + 
                                  sadness + surprise + trust + negative + positive, data = df)
)


### 1.1.3 List 3 - Western Conference
models3 <- list(
    "Dallas Mavericks" = lm(Mavericks ~ anger + anticipation + disgust + fear + joy + 
                                sadness + surprise + trust + negative + positive, data = df),
    "Denver Nuggets" = lm(Nuggets ~ anger + anticipation + disgust + fear + joy + 
                              sadness + surprise + trust + negative + positive, data = df),
    "Golden State Warriors" = lm(Warriors ~ anger + anticipation + disgust + fear + joy + 
                                     sadness + surprise + trust + negative + positive, data = df),
    "Houston Rockets" = lm(Rockets ~ anger + anticipation + disgust + fear + joy + 
                               sadness + surprise + trust + negative + positive, data = df),
    "Los Angeles Clippers" = lm(Clippers ~ anger + anticipation + disgust + fear + joy + 
                                    sadness + surprise + trust + negative + positive, data = df),
    "Los Angeles Lakers" = lm(Lakers ~ anger + anticipation + disgust + fear + joy + 
                                  sadness + surprise + trust + negative + positive, data = df),
    "Memphis Grizzlies" = lm(Grizzlies ~ anger + anticipation + disgust + fear + joy + 
                                 sadness + surprise + trust + negative + positive, data = df),
    "Minnesota Timberwolves" = lm(Timberwolves ~ anger + anticipation + disgust + fear + joy + 
                                      sadness + surprise + trust + negative + positive, data = df),
    "New Orleans Pelicans" = lm(Pelicans ~ anger + anticipation + disgust + fear + joy + 
                                    sadness + surprise + trust + negative + positive, data = df),
    "Oklahoma City Thunder" = lm(Thunder ~ anger + anticipation + disgust + fear + joy + 
                                     sadness + surprise + trust + negative + positive, data = df),
    "Phoenix Suns" = lm(Suns ~ anger + anticipation + disgust + fear + joy + 
                            sadness + surprise + trust + negative + positive, data = df),
    "Portland Trail Blazers" = lm(TrailBlazers ~ anger + anticipation + disgust + fear + joy + 
                                      sadness + surprise + trust + negative + positive, data = df),
    "Sacramento Kings" = lm(Kings ~ anger + anticipation + disgust + fear + joy + 
                                sadness + surprise + trust + negative + positive, data = df),
    "San Antonio Spurs" = lm(Spurs ~ anger + anticipation + disgust + fear + joy + 
                                 sadness + surprise + trust + negative + positive, data = df),
    "Utah Jazz" = lm(Jazz ~ anger + anticipation + disgust + fear + joy + 
                         sadness + surprise + trust + negative + positive, data = df)
)


## 1.2  Regression Models - Separate:
model1 <- lm(Hawks ~ anger + anticipation + disgust + fear + joy + 
                 sadness + surprise + trust + negative + positive, data = df)
model2 <- lm(Celtics ~ anger + anticipation + disgust + fear + joy + 
                 sadness + surprise + trust + negative + positive, data = df)
model3 <- lm(Nets ~ anger + anticipation + disgust + fear + joy + 
                 sadness + surprise + trust + negative + positive, data = df)
model4 <- lm(Hornets ~ anger + anticipation + disgust + fear + joy + 
                 sadness + surprise + trust + negative + positive, data = df)
model5 <- lm(Bulls ~ anger + anticipation + disgust + fear + joy + 
                 sadness + surprise + trust + negative + positive, data = df)
model6 <- lm(Cavaliers ~ anger + anticipation + disgust + fear + joy + 
                 sadness + surprise + trust + negative + positive, data = df)
model7 <- lm(Mavericks ~ anger + anticipation + disgust + fear + joy + 
                 sadness + surprise + trust + negative + positive, data = df)
model8 <- lm(Nuggets ~ anger + anticipation + disgust + fear + joy + 
                 sadness + surprise + trust + negative + positive, data = df)
model9 <- lm(Pistons ~ anger + anticipation + disgust + fear + joy + 
                 sadness + surprise + trust + negative + positive, data = df)
model10 <- lm(Warriors ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model11 <- lm(Rockets ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model12 <- lm(Pacers ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model13 <- lm(Clippers ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model14 <- lm(Lakers ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model15 <- lm(Grizzlies ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model16 <- lm(Heat ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model17 <- lm(Bucks ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model18 <- lm(Timberwolves ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model19 <- lm(Pelicans ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model20 <- lm(Knicks ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model21 <- lm(Thunder ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model22 <- lm(Magic ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model23 <- lm(Sixers ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model24 <- lm(Suns ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model25 <- lm(TrailBlazers ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model26 <- lm(Kings ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model27 <- lm(Spurs ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model28 <- lm(Raptors ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model29 <- lm(Jazz ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)
model30 <- lm(Wizards ~ anger + anticipation + disgust + fear + joy + 
                  sadness + surprise + trust + negative + positive, data = df)


## 1.3 Inspecting Individual Models
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)
summary(model7)
summary(model8)
summary(model9)
summary(model10)
summary(model11)
summary(model12)
summary(model13)
summary(model14)
summary(model15)
summary(model16)
summary(model17)
summary(model18)
summary(model19)
summary(model20)
summary(model21)
summary(model22)
summary(model23)
summary(model24)
summary(model25)
summary(model26)
summary(model27)
summary(model28)
summary(model29)
summary(model30)


## 1.4  Saving Models
cm2 <- c('anger' = 'Anger',
         'anticipation' = 'Anticipation',
         'disgust' = 'Disgust',
         'fear' = 'Fear',
         'joy' = 'Joy',
         'sadness' = 'Sadness',
         'surprise' = 'Surprise',
         'trust' = 'Trust',
         'negative' = 'Negative (-)',
         'positive' = 'Positive (+)')

summary2 <- modelplot(models1,
                      data = df,
                      title = "Sentiment Analysis Overview (N=10765)")

summary3 <- modelsummary(models2,
                         title = 'Sentiment Analysis NBA - Eastern Conference',
                         notes = "Sig.: *** = 0.001, ** 0.01, * = 0.5; Std. Err. in ()",
                         coef_map = cm2,
                         estimate = "{estimate}{stars}")

summary4 <- modelsummary(models3,
                         title = 'Sentiment Analysis NBA - Western Conference',
                         notes = "Sig.: *** = 0.001, ** 0.01, * = 0.5; Std. Err. in ()",
                         coef_map = cm2,
                         estimate = "{estimate}{stars}")


# 2. Saving Data
write.csv(sentiment_nrc,"sentiment_nrc.csv", row.names = FALSE)
save(models1, file='models1.rda')
save(models2, file='models2.rda')
save(models3, file='models3.rda')


# 3. Results
# Sentiment Analysis per Team - Eastern Conference
summary3

#Sentiment Analysis per Team - Western Conference
summary4
