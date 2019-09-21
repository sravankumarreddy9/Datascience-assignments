library(klaR)
library(MASS)
library(caret)
library(tm)
library(pander)
library(dplyr)
#for percentage frequent tables
frqtab <- function(x, caption) {
  round(100*prop.table(table(x)), 1)
}

sms <- read.csv("C:\\Users\\Sravan\\Desktop\\EXCEL R DATA SCIENCE\\assignments\\naive bayes\\sms_raw_NB.csv", header = TRUE, sep = ",", quote='\"\"', stringsAsFactors = FALSE)
attach(sms)
type <- factor(type)
set.seed(12358)
sms <- sms[sample(nrow(sms)),]
str(sms)
#preprocessing the data
sms_corpus <- Corpus(VectorSource(text))
sms_corpus_clean <- sms_corpus %>%
  tm_map(content_transformer(tolower)) %>%  
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
#creating classification model with naive bayes
#creating training and testing datasets
train_index <- createDataPartition(sms$type, p=0.75, list=FALSE)
sms_raw_train <- sms[train_index,]
sms_raw_test <- sms[-train_index,]
sms_corpus_clean_train <- sms_corpus_clean[train_index]
sms_corpus_clean_test <- sms_corpus_clean[-train_index]
sms_dtm_train <- sms_dtm[train_index,]
sms_dtm_test <- sms_dtm[-train_index,]
#Applying custom function 
ft_orig <- frqtab(sms$type)
ft_train <- frqtab(sms_raw_train$type)
ft_test <- frqtab(sms_raw_test$type)
ft_df <- as.data.frame(cbind(ft_orig, ft_train, ft_test))
colnames(ft_df) <- c("Original", "Training set", "Test set")
pander(ft_df, style="rmarkdown",caption=paste0("Comparison of SMS type frequencies among datasets"))

sms_dict <- findFreqTerms(sms_dtm_train, lowfreq=5)
sms_train <- DocumentTermMatrix(sms_corpus_clean_train, list(dictionary=sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_clean_test, list(dictionary=sms_dict))

convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("Absent", "Present"))
}
sms_train <- sms_train %>% apply(MARGIN=2, FUN=convert_counts)
sms_test <- sms_test %>% apply(MARGIN=2, FUN=convert_counts)
#training prediction models
ctrl <- trainControl(method="cv", 10)
set.seed(12358)
sms_model1 <- train(sms_train, sms_raw_train$type, method="nb",trControl=ctrl)
sms_model1
#Accuracy 98%

set.seed(12358)
sms_model2 <- train(sms_train, sms_raw_train$type, method="nb", 
                    tuneGrid=data.frame(.fL=1, .usekernel=FALSE),
                    trControl=ctrl)
sms_model2
#Accuracy 98%
#testing the predictions
sms_predict1 <- predict(sms_model1, sms_test)
cm1 <- confusionMatrix(sms_predict1, sms_raw_test$type, positive="spam")
cm1
#Accuracy 97%

sms_predict2 <- predict(sms_model2, sms_test)
cm2 <- confusionMatrix(sms_predict2, sms_raw_test$type, positive="spam")
cm2
#Accuracy 97%
