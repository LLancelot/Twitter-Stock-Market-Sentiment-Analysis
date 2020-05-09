#Twitter Stock Market Sentimental Analysis
rm(list=ls());
cat("\014")

library("twitteR")
library(tm)
library(wordcloud)
library(SnowballC)
library(stringr)

t.api.key <-"PbPWLhdVb9OyJGPxdAkgtKG8l"
t.api.secret <- "xD8zKxIIsjQsmqxxafkm3LeyuAbcyHWo9wKinecazx69iYTvp9"
t.access.token <- "873822439662170112-4I8XGrAwt1Dk22tonQpPxZprL37yrhx"
t.access.secret <- "3hYtnLfnwQmtZfN6dkWMxrpaa2wU9YhdeStdJhpTGSefJ"

# -------save credentials------
library("twitteR")
setup_twitter_oauth(consumer_key = t.api.key, consumer_secret = t.api.secret, 
                    access_token = t.access.token, access_secret = t.access.secret)
save(list = (c("t.api.key", "t.api.secret", "t.access.token", "t.access.secret")), file = "twitter_credentials.RData")
# -----------------------------

# Search for 100 tweets
# Top 3 largest gainers: AVTR, SDC, NGVT

# Gain 1) For AVTR
tweets_AVTR <- searchTwitter('$AVTR', n = 100)
display_tweet <- function(tweet){
  cat("Screen name:", tweet$getScreenName(), "\nText:", tweet$getText(), "\n\n")
}

# Display tweets for AVTR  
for (t in tweets_AVTR) {
  display_tweet(t)
}

# Gain 2) For SDC
tweets_SDC <- searchTwitter('$SDC', n = 100)
display_tweet <- function(tweet){
  cat("Screen name:", tweet$getScreenName(), "\nText:", tweet$getText(), "\n\n")
}

# Display tweets for SDC
for (t in tweets_SDC){
  display_tweet(t)
}

# Gain 3) For NGVT
tweets_NGVT <- searchTwitter('$NGVT', n = 100)
display_tweet <- function(tweet){
  cat("Screen name:", tweet$getScreenName(), "\nText:", tweet$getText(), "\n\n")
}

# Display tweets for NGVT 
for (t in tweets_NGVT) {
  display_tweet(t)
}

# --------------------------------------------------------------------------------

# Top 3 largest losers: OLN, MANT, GIL
# Lose 1) OLN
tweets_OLN <- searchTwitter('$OLN', n = 100)
display_tweet <- function(tweet){
  cat("Screen name:", tweet$getScreenName(), "\nText:", tweet$getText(), "\n\n")
}

# Display tweets for SDC
for (t in tweets_OLN){
  display_tweet(t)
}

# Lose 2) MANT
tweets_MANT <- searchTwitter('$MANT', n = 100)
display_tweet <- function(tweet){
  cat("Screen name:", tweet$getScreenName(), "\nText:", tweet$getText(), "\n\n")
}

# Display tweets for SDC
for (t in tweets_MANT){
  display_tweet(t)
}

# Lose 3) GIL
tweets_GIL <- searchTwitter('$GIL', n = 100)
display_tweet <- function(tweet){
  cat("Screen name:", tweet$getScreenName(), "\nText:", tweet$getText(), "\n\n")
}

# Display tweets for SDC
for (t in tweets_GIL){
  display_tweet(t)
}

# ---------------------------------------------------------------------------------
# Combine 3 top gainers into a whole tweet
t.gainers <- c(tweets_AVTR, tweets_SDC, tweets_NGVT)
t.gainers
# Combine 3 top gainers
t.losers <- c(tweets_OLN, tweets_MANT, tweets_GIL)
t.losers

# ---------------------------------------------------------------------------------
# Create Corpus for Gainer Tweets
t.gainers.text <- lapply(t.gainers, function(t) {t$getText()})
t.gainers.source <- VectorSource(t.gainers.text)

data.corpus1 <- Corpus(t.gainers.source)
data.corpus1 <- tm_map(data.corpus1, function(x) iconv(x, to='UTF-8', sub = 'byte'))
data.corpus1

wd = getwd()
# writing data.corpus1 to "stocks" folder
writeCorpus(data.corpus1, paste(wd, '/stocks', sep = ""))

# Create Corpus for Loser Tweets
t.loser.text <- lapply(t.losers, function(t) {t$getText()})
t.loser.source <- VectorSource(t.loser.text)

data.corpus2 <- Corpus(t.loser.source)
data.corpus2 <- tm_map(data.corpus2, function(x) iconv(x, to='UTF-8', sub = 'byte'))
data.corpus2

# writing data.corpus2 to "stocks" folder
writeCorpus(data.corpus2, paste(wd, '/stocks', sep = ""))

# --------------Preprocessing-------------------
# 1) Gainer : data.corpus1
getTransformations()
content_transformer()

data.corpus1 <- tm_map(data.corpus1, content_transformer(tolower))
removeURL <- function(x){
  gsub("(http[^ ]*)","",x)
  }

data.corpus1 <- tm_map(data.corpus1, content_transformer(removeURL))
data.corpus1 <- tm_map(data.corpus1, content_transformer(removePunctuation))
english.stopwords <- stopwords("en")

data.corpus1 <- tm_map(data.corpus1, content_transformer(removeWords), english.stopwords)
removeNumberWords <- function(x){
  gsub("([[:digit:]]+)([[:alnum:]])*","", x)
  }

data.corpus1 <- tm_map(data.corpus1, content_transformer(removeNumberWords))
data.corpus1 <- tm_map(data.corpus1, content_transformer(stemDocument))
data.corpus1 <- tm_map(data.corpus1, content_transformer(stripWhitespace))
# inspect(data.corpus1[1:2])

# 2) Loser: data.corpus2
getTransformations()
content_transformer()

data.corpus2 <- tm_map(data.corpus2, content_transformer(tolower))
removeURL <- function(x){gsub("(http[^ ]*)","",x)}

data.corpus2 <- tm_map(data.corpus2, content_transformer(removeURL))
data.corpus2 <- tm_map(data.corpus2, content_transformer(removePunctuation))
english.stopwords <- stopwords("en")

data.corpus2 <- tm_map(data.corpus2, content_transformer(removeWords), english.stopwords)
removeNumberWords <- function(x){gsub("([[:digit:]]+)([[:alnum:]])*","",x)}
data.corpus2 <- tm_map(data.corpus2, content_transformer(removeNumberWords))
data.corpus2 <- tm_map(data.corpus2, content_transformer(stemDocument))
data.corpus2 <- tm_map(data.corpus2, content_transformer(stripWhitespace))
# inspect(data.corpus2[1:2])

#-------------------Term Document Matrix----------------------
# 1) Gainers
tdm.Gainer <- TermDocumentMatrix(data.corpus1)
tdm.Gainer

# Save tdm.Gainers
mat_tdmGainer <- as.matrix(tdm.Gainer)
write.csv(mat_tdmGainer, file = "tdm_Gainer.csv")

# 2) Losers
tdm.Loser <- TermDocumentMatrix(data.corpus2)
tdm.Loser

# Save
mat_tdmLoser <- as.matrix(tdm.Loser)
write.csv(mat_tdmLoser, file = "tdm_Loser.csv")

#-------------------Frequent Terms----------------------------
# For Gainers
findFreqTerms(tdm.Gainer, lowfreq = 20)
matrix_gainers <- as.matrix(tdm.Gainer)
wordFreq_gainers <- rowSums(matrix_gainers)
wordFreq_gainers <- sort(wordFreq_gainers, decreasing = TRUE)
cbind(wordFreq_gainers[1:10])

# Make WordCloud for Gainers
palette_gainers <- brewer.pal(12, "Paired")
set.seed(0)
wordcloud(words = names(wordFreq_gainers), freq = wordFreq_gainers, min.freq = 20, random.order = F, colors = palette_gainers)


# For Losers
findFreqTerms(tdm.Loser, lowfreq = 20)
matrix_losers <- as.matrix(tdm.Loser)
wordFreq_losers <- rowSums(matrix_losers)
wordFreq_losers <- sort(wordFreq_losers, decreasing = TRUE)
cbind(wordFreq_losers[1:10])

# Make WordCloud for Gainers
palette_losers <- brewer.pal(8, "Dark2")
set.seed(0)
wordcloud(words = names(wordFreq_losers), freq = wordFreq_losers, min.freq = 75, random.order = F, colors = palette_gainers)

# -----------Compute the Sentiment Score--------------
wd = getwd()
setwd(paste(wd,"/stocks",sep = ""))

pos.words = scan('positive-words.txt', what = 'character', comment.char = ';')
neg.words = scan('negative-words.txt', what = 'character', comment.char = ';')

sentiment <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '',text)
  text <- gsub('[[:cntrl:]]', '',text)
  text <- gsub('\\d+', '',text)
  text <- tolower(text)
  
  #Split the text into a vector of words  
  words <- strsplit(text,'\\s+')
  words <- unlist(words)
  
  #Find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  
  #Find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  
  #Calculate the sentiment score
  score_return <- sum(pos.matches) - sum(neg.matches)
  cat("Positive:", words[pos.matches], "\n")
  cat("Negative:", words[neg.matches], "\n")
  cat("Positive Num:", sum(pos.matches),", Negative Num:", sum(neg.matches), "\n")
  return(score_return)
}

getNumsPosMatches <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '',text)
  text <- gsub('[[:cntrl:]]', '',text)
  text <- gsub('\\d+', '',text)
  text <- tolower(text)
  
  #Split the text into a vector of words  
  words <- strsplit(text,'\\s+')
  words <- unlist(words)
  
  #Find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  
  #Find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  
  #Calculate the sentiment score
  score_return <- sum(pos.matches) - sum(neg.matches)
  # cat("Positive:", words[pos.matches], "\n")
  # cat("Negative:", words[neg.matches], "\n")
  # cat("Positive Num:", sum(pos.matches),", Negative Num:", sum(neg.matches), "\n")
  return(sum(pos.matches))
}

getNumsNegMatches <- function(text, pos.words, neg.words) {
  text <- gsub('[[:punct:]]', '',text)
  text <- gsub('[[:cntrl:]]', '',text)
  text <- gsub('\\d+', '',text)
  text <- tolower(text)
  
  #Split the text into a vector of words  
  words <- strsplit(text,'\\s+')
  words <- unlist(words)
  
  #Find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  
  #Find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  
  #Calculate the sentiment score
  score_return <- sum(pos.matches) - sum(neg.matches)
  # cat("Positive:", words[pos.matches], "\n")
  # cat("Negative:", words[neg.matches], "\n")
  # cat("Positive Num:", sum(pos.matches),", Negative Num:", sum(neg.matches), "\n")
  return(sum(neg.matches))
}

gainers.score = sentiment(names(wordFreq_gainers), pos.words = pos.words, neg.words = neg.words)
losers.score = sentiment(names(wordFreq_losers), pos.words = pos.words, neg.words = neg.words)

gainers.pos.num = getNumsPosMatches(names(wordFreq_gainers), pos.words = pos.words, neg.words = neg.words)
gainers.neg.num = getNumsNegMatches(names(wordFreq_gainers), pos.words = pos.words, neg.words = neg.words)

losers.pos.num = getNumsPosMatches(names(wordFreq_losers), pos.words = pos.words, neg.words = neg.words)
losers.neg.num = getNumsNegMatches(names(wordFreq_losers), pos.words = pos.words, neg.words = neg.words)
# ----------------Plot the chart------------------------------
# install.packages("googleVis")
library("googleVis")
# For 3 gainers, sum(pos.matches) = 28, sum(neg.matches) = 20
# For 3 losers, sum(pos.matches) = 15, sum(neg.matches) = 21
gainers.percent.pos = round(100*gainers.pos.num/(gainers.pos.num+gainers.neg.num),2)
gainers.percent.neg = round(100 - gainers.percent.pos, 2)
losers.percent.pos = round(100*losers.pos.num/(losers.pos.num+losers.neg.num), 2)
losers.percent.neg = round(100 - losers.percent.pos, 2)
gainers.sentiment.data = data.frame(matches = c("pos.matches(%)", "neg.matches(%)"),
                                    Percentage_matches = c(gainers.percent.pos, gainers.percent.neg))
chart1 <- gvisBarChart(gainers.sentiment.data)

plot(chart1)

losers.sentiment.data = data.frame(matches = c("pos.matches(%)", "neg.matches(%)"),
                                   Percentage_matches = c(losers.percent.pos, losers.percent.neg))
chart2 <- gvisBarChart(losers.sentiment.data)
plot(chart2)

score.data = data.frame(score_name = c("Gainers Score", "Losers Score"),
                        score_value = c(gainers.score, losers.score))
chart3 <- gvisColumnChart(score.data)
plot(chart3)


#-----------------Plot Candlestick Graph-----------------------

library(readr)

# Here I choose stock $AVTR to plot candlestick graph
data_stock <- read.csv("AVTR.csv")
data_stock

# use data frame
data_stock <- data.frame(data_stock)

chart_AVTR <- gvisCandlestickChart(data_stock, xvar = "Date", low = "Low", high = "High", open = "Open", close = "Close",
                                   options = list(height = 825, vAxis = '{minValue:6, maxValue:20}', legend = 'none'))
plot(chart_AVTR)
# max(data_stock$High)
# min(data_stock$Low)






