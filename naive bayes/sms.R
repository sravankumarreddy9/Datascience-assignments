
library(tm)
library(wordcloud)
library(NLP)
library(RColorBrewer)
library(e1071)
library(gmodels)
library(klaR)
install.packages("klaR")
sms_data <- read.csv("C:\\Users\\Sravan\\Desktop\\EXCEL R DATA SCIENCE\\assignments\\naive bayes\\sms_raw_NB.csv", stringsAsFactors = FALSE)
attach(sms_data)
head(sms_data)
str(sms_data)
summary(sms_data)
#checking for how many ham and spam in the dataset
table(sms_data$type)
sms_data$type <- factor(sms_data$type)
sms_data_fac <- as.factor(sms_data$type)
plot(sms_data_fac)
#splitting the data into train and test
sms_train_labels <- sms_data[1:4169, ]$type
sms_test_labels <- sms_data[4170:5559, ]$type
#checking for percentage split of both ham and spam for train and test
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))
sms_corpus <- VCorpus(VectorSource(sms_data$text))
#creating document term matrix
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(tolower = TRUE,
                                                            removeNumber = TRUE,
                                                            stopwords = TRUE,
                                                            removePunctuation = TRUE,
                                                            stemming = TRUE))

sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(tolower = TRUE,
                                                            removeNumber = TRUE,
                                                            stopwords = function(x) {removeWords(x, stopwords() ) },
                                                            removePunctuation = TRUE,
                                                            stemming = TRUE))
print(sms_dtm2)
#clean the dataset 
sms_corpus <- VCorpus(VectorSource(sms_data$text))
sms_corpus_clean <-tm_map(sms_corpus, content_transformer(tolower))
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean <-tm_map(sms_corpus_clean, removeWords, stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
#creating word clouds for both spam and ham 
wordcloud(sms_corpus_clean, min.freq = 10, random.order = FALSE)
spam <- subset(sms_data, type == "spam")
wordcloud(spam$text, max.words = 40, scale = c(3,0.5))
ham <- subset(sms_data, type == "ham")
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))
findFreqTerms(sms_dtm2, 5)
sms_train <- apply(sms_train_labels, MARGIN = 2, convert_counts)
CrossTable(sms_test_pred, sms_test_labels, rop.chisq = FALSE, prop.t = FALSE,dnn = c('predicted', 'actual')))
