install.packages('caTools')  
install.packages('dplyr')    
install.packages('ggplot2')  
install.packages('class')    
install.packages('caret')    
install.packages('corrplot')

library(caTools)
library(dplyr)
library(ggplot2)
library(caret)
library(class)
library(corrplot)
glass_data <- read.csv(file.choose())
#standardize the data for scale free.
standardize <- scale(glass_data[,1:9])
#join the standardize data with target column.
glass <- cbind(standardize, glass_data[10])
#check for na values
anyNA(glass)
head(glass)
corrplot(cor(glass))
set.seed(101)

sample <- sample.split(glass$Type,SplitRatio = 0.75)

train <- subset(glass,sample==TRUE)

test <- subset(glass,sample==FALSE)
#using k value 1 for test data
predicted_Values <- knn(train[1:9],test[1:9],train$Type,k=1)
#Error in prediction
error <- mean(predicted_Values!=test$Type)
error
#Confusion Matrix
confusionMatrix(predicted_Values,as.factor(test$Type))
#Accuracy = 73%

predicted_Values <- NULL
error.rate <- NULL

for (i in 1:10) {
  predicted_Values <- knn(train[1:9],test[1:9],train$Type,k=i)
  error.rate[i] <- mean(predicted_Values!=test$Type)
  }

knn.error <- as.data.frame(cbind(k=1:10,error.type =error.rate))
knn.error
#using graph to select k value.
ggplot(knn.error,aes(k,error.type))+ geom_point()+  geom_line() + scale_x_continuous(breaks=1:10)+ theme_bw() +xlab("Value of K") +ylab('Error')
#k value is 5.
#predicting with k = 5
predicted_value <- knn(train[1:9],test[1:9],train$Type,k=5)
#Error in prediction
error <- mean(predicted_value!=test$Type)
error
#Confusion Matrix
confusionMatrix(predicted_value,as.factor(test$Type))
#Accuracy is 79%
