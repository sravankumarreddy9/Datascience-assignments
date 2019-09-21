library("twitteR")
library("ROAuth")
library(base64enc)
library(httpuv)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
#setting the access to read the tweets from the twitter
cred <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")
setup_twitter_oauth("BagGgBbanzbdpPNNp8Uy6TQBP", # Consumer Key (API Key)
                    "pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa", #Consumer Secret (API Secret)
                    "1076425245521731584-Ev31ZLB7Cf0idVMqDI8BxiVG2SgRnu",  # Access Token
                    "ZVUw0Z0mFrX7d6sjQxuB08l48JHhmnjmlAm86G2OPG7BS")
#reading the tweets from donald trump
Tweets <- userTimeline('POTUS', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
#storing in local directory
setwd("C:\\Users\\Sravan\\Desktop\\EXCEL R DATA SCIENCE\\assignments\\Text mining")
write.csv(TweetsDF, "Tweets.csv",row.names = F)

getwd()
donald <- read.csv(file.choose())
str(donald)
#cleaning the text
corpus <- donald$text
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])
corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])
cleanset<-tm_map(corpus,removeWords, stopwords('english'))                 
inspect(cleanset[1:5])
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])
cleanset<-tm_map(cleanset,removeWords, c('donald','can'))
cleanset <- tm_map(cleanset, gsub,pattern = 'pages', replacement = 'page')
inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])
#creating documentation matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
inspect(tdm)
#inverse term documentation matrix 
tdm1 <-TermDocumentMatrix(cleanset, control = list(weighting = function(p) weightTfIdf(p, normalize = T)))
inspect(tdm1)
#postive and negative sentiment analysis 
#reading the positive, negative and stop words
pos <- scan(file.choose(), what = "character",comment.char=";")
neg <- scan(file.choose(),what="character",comment.char=";")
stop <- scan(file.choose(), what="character", comment.char=";")
#custom function to create wordcloud
makewordc = function(a){
  
  a.colsum = apply(a, 2, sum);
  
  words = colnames(a)
  
  freq = a.colsum*100
  
  wordcloud(words, freq, scale=c(8, 0.3), colors=1:10)  } 

a0 <- NULL
a1 <- NULL 
for (i1 in 1:ncol(tdm))
{ if (sum(tdm[, i1]) == 0) {a0 = c(a0, i1)}}
for (i1 in 1:ncol(tdm1))
{ if (sum(tdm1[, i1]) == 0) {a1 = c(a1, i1)}}
a0
a1
tdm <- tdm[, -a0]
tdm1 <- tdm1[, -a1]
dtm <- t(tdm)
dtm1 <- t(tdm1)
#wordcloud
makewordc(tdm0)
title(sub = "unigram worcloud using TF")
words_bar_plot(tdm1)
#custom function for positive words 
makeposwordc = function(a){ 
  
  pos.matches = match(colnames(a), pos.words)       
  
  pos.matches = !is.na(pos.matches)
  
  b1 = apply(a, 2, sum)[pos.matches];    b1 = as.data.frame(b1);
  
  colnames(b1) = c("freq");
  
  wordcloud(rownames(b1), b1[,1], scale=c(5, 1), colors=1:10)  
  
}   

# custom function for negative words

makenegwordc = function(a){ 
  
  neg.matches = match(colnames(a), neg.words)       
  
  neg.matches = !is.na(neg.matches)
  
  b1 = apply(a, 2, sum)[neg.matches];    b1 = as.data.frame(b1);
  
  colnames(b1) = c("freq");
  
  wordcloud(rownames(b1), b1[,1], scale=c(5, 1), colors=1:10) 
  
}   
#positive word cloud
makeposwordc(tdm)
title(sub ="positive wordcloud")
makeposwordc(tdm1)
title(sub = "postive wordcloud inverse")
#negative wordcloud
makenegwordc(tdm)
title(sub = "positive wordcloud")
makenegwordc(tdm1)
title(sub = "negative wordcloud inverse")
tdm <- as.matrix(tdm)
tdm[1:10,1:20]
w <- rowSums(tdm)  # provides the no of times a particular word has been used.
w <- subset(w, w>= 25) # Pull words that were used more than 25 times.
barplot(w, las = 2, col = rainbow(50))
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, minSize = 1)
letterCloud(w,word = "F",frequency(5), size=1)
library(reshape2)

#sentiment analysis
donald_trump <- read.csv(file.choose(), header = TRUE)
tweets <- as.character(donald_trump$text)
class(tweets)
s <- get_nrc_sentiment(tweets)
head(s)
tweets[4]
get_nrc_sentiment('vision')
get_nrc_sentiment('pro-jobs')
tweets[10]
get_nrc_sentiment('Intelligence')
get_nrc_sentiment('troops')
get_nrc_sentiment('Chinese')
get_nrc_sentiment('informed')
barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for Facebook Tweets')
