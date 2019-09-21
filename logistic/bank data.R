library(plyr)
library(readr)
bank_data <- read_delim("C:\\Users\\Sravan\\Desktop\\EXCEL R DATA SCIENCE\\assignments\\Logistic Regression\\bank-full.csv", ";")
attach(bank_data)
# To check null values
sum(is.na(bank_full))
#Partition data into two dataset
library(caret)
bank_data$y=factor(bank_data$y)
trainindex<-createDataPartition(bank_data$y,p=0.7,list=FALSE)
# Get training data-frame
train_data<-bank_data[trainindex,]
# Get data-frame for testing
test_data<-bank_data[-trainindex,]
# Check number of rows in split-data set
nrow(train_data)
nrow(test_data)
# Indicate now which variables are categorical variables
train_data$y=factor(train_data$y)
train_data$job=factor(train_data$job)
train_data$marital=factor(train_data$marital)
train_data$education=factor(train_data$education)
train_data$housing=factor(train_data$housing)
train_data$loan=factor(train_data$loan)
train_data$contact=factor(train_data$contact)
train_data$month=factor(train_data$month)
train_data$poutcome=factor(train_data$poutcome)

# Get descriptive stats for data
summary(train_data)
#creating a model using glm function 
model=glm(y~., data=train_data,family=binomial(logit))
summary(model)
model1=glm(y~poutcome+previous+pdays+campaign+duration+month+day+contact+loan+housing+balance+default+education+marital+job+age, data=train_data,family=binomial(logit))
summary(model1)
# Find odds ratio as:
exp(coef(model))
#confusion matrix table
probabilities <- predict.glm(model, train_data, type="response")
confusion <- table(probabilities>0.5,train_data$y)
#model accuracy
accuracy <- sum(diag(confusion)/sum(confusion))
accuracy #0.90
#creating empty vectors to store predicted values
pred_values <- NULL
yes_no <- NULL
pred_values <- ifelse(probabilities>=0.5,1,0)
yes_no <- ifelse(probabilities>=0.5, "yes", "no")
train_data[,"probabilities"] <- probabilities
train_data[,"pred_values"] <- pred_values
train_data[,"yes_no"] <- yes_no
table(train_data$y, train_data$pred_values)
library(ROCR)
rocrpred<-prediction(probabilities,train_data$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)
library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
#testing in test data
probabilities_test <- predict.glm(model, test_data, type="response")
confusion1 <- table(probabilities_test>0.5,test_data$y)
accuracy <- sum(diag(confusion1)/sum(confusion1))
accuracy
