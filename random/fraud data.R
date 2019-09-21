library(randomForest)
fraud_data <- read.csv(file.choose())
TIRisky <- NULL
#converting if the income is less than or equal to 1 and above to 0
TIRisky <- ifelse(fraud_data$Taxable.Income <=30000,1,0)
fraud_data[,"TIRisky"]<- TIRisky
#converting using as.factor function for undergrad, martialstatus, urban, TIrisky variable
fraud_data$Undergrad <- as.factor(fraud_data$Undergrad)
fraud_data$Marital.Status <- as.factor(fraud_data$Marital.Status)
fraud_data$Urban <- as.factor(fraud_data$Urban)
fraud_data$TIRisky <- as.factor(fraud_data$TIRisky)
#stoing less than or equal to 30000 and greater than 30000 stored in risky and non risky 
fraud_risky <- fraud_data[fraud_data$TIRisky == "1",] 
fraud_not_risky <- fraud_data[fraud_data$TIRisky == "0",]
#data is divided into train and test
data_train <- rbind(fraud_risky[1:93,], fraud_not_risky[1:357,])
data_test <- rbind(fraud_risky[94:124,], fraud_not_risky[357:476,])
#creating model
fit.forest <- randomForest(TIRisky~.,data=data_train, na.action=na.roughfix,importance=TRUE)
mean(data_train$TIRisky == predict(fit.forest,data_train))
pred_train <- predict(fit.forest,data_train)
library(caret)
confusionMatrix(data_train$TIRisky, pred_train)
#prediction for test dataset
pred_test <- predict(fit.forest,newdata=data_test)
mean(pred_test == data_test$TIRisky)
confusionMatrix(data_test$TIRisky, pred_test)
plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)
