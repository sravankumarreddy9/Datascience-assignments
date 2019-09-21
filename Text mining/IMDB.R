library(rvest)
library(XML)
library(magrittr)
library(tm)
library(wordcloud)
library(wordcloud2)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
#reading the reviews
aurl <- "https://www.imdb.com/title/tt6320628/reviews?ref_=tt_urv"
IMDB_reviews <- NULL
#function to read the reviews 10 per page 
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".show-more__control") %>%
    html_text()
  IMDB_reviews <- c(IMDB_reviews,rev)
}
#checking number of reviews
length(IMDB_reviews)
#storing the reviews
setwd("C:\\Users\\Sravan\\Desktop\\EXCEL R DATA SCIENCE\\assignments\\Text mining")
write.table(IMDB_reviews,"spiderman.txt",row.names = F)
spiderman <- read.delim('spiderman.txt')
str(spiderman)
View(spiderman)
#Build corpus and DTM/TM
library(tm)
#clean the text
corpus <- spiderman[-1,]
head(corpus)
class(corpus)
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
cleanset<-tm_map(cleanset,removeWords, c('can','film'))
cleanset<-tm_map(cleanset,removeWords, c('movie','movies'))
cleanset <- tm_map(cleanset, gsub,pattern = 'character', replacement = 'characters')
inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])
#Term document matrix
#converting unstructured data to structured data
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
#97% sparsity indicates there are lots of zero values
tdm <- as.matrix(tdm)
tdm[1:10,1:20]
#barplot
w <- rowSums(tdm)
w <- subset(w, w>= 50)
barplot(w, las = 2, col = rainbow(50))
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE) # Sort words in decreasing order.
set.seed(123)
wordcloud(words = names(w), freq = w, 
          max.words = 250,random.order = F,
          min.freq =  3, 
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5,0.3),
          rot.per = 0.6)
library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, 
           minSize = 1)
#sentiment analysis
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
IMDB_reviews <- read.delim('spiderman.txt')
reviews <- as.character(IMDB_reviews[-1,])
class(reviews)
#obtaining sentiment scores
s <- get_nrc_sentiment(reviews)
head(s)
reviews[4]
#from the review what the word indicates 
get_nrc_sentiment('incredible')
get_nrc_sentiment('relationships')
barplot(colSums(s), las = 2.5, col = rainbow(10),
        ylab = 'Count',main= 'Sentiment scores for IMDB Reviews
        for spiderman')