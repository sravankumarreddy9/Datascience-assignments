library(readr)
library(party)
library(caret)
library(C50)
library(tree)
library(gmodels)
library(knitr)
library(png)
fraud_data <- read.csv(file.choose())
#checking taxable income variable data distribution
hist(fraud_data$Taxable.Income)
#converting the values if the income is <=30000 risky otherwise good
risky_good = ifelse(fraud_data$Taxable.Income <=30000, "Risky", "Good")
#storing the variable to the dataframe
fraud = data.frame(fraud_data, risky_good)
#splitting the data into test and train
fraud_train <- fraud[1:300,]
fraud_test <- fraud[301:600,]
#applying party function
png(file = "decision_tree.png")
#taking risky_good as output variable 
opall_tree = ctree(risky_good ~ Undergrad + Marital.Status + City.Population + Work.Experience + Urban, data = fraud)
summary(opall_tree)
plot(opall_tree)
#applying party function for train data
png(file = "decision_tree.png")
op_tree = ctree(risky_good ~ Undergrad + Marital.Status + City.Population + Work.Experience + Urban, data = fraud_train)
summary(op_tree)
plot(op_tree)
#predicting for test data
pred_tree <- as.data.frame(predict(op_tree,newdata=fraud_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=fraud_test)
mean(pred_test_df==fraud_test$risky_good)
#accuracy is 82%
CrossTable(fraud_test$risky_good,pred_test_df)
confusionMatrix(fraud_test$risky_good,pred_test_df)
