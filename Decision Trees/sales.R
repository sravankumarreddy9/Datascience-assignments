library(readr)
library(rmarkdown)
library(C50)
library(tree)
library(caret)
library(gmodels)
library(party)
company_data <- read.csv(file.choose())
attach(company_data)
#checking for the distribution of sales variable
hist(Sales)
#assigning if the sales is less than 10 yes and no
high = ifelse(Sales<10, "No", "Yes")
#storing the conversion variable to the dataframe
c_data = data.frame(company_data, high)
#splitting the data into train and test
company_train <- c_data[1:200,]
company_test <- c_data[201:400,]
#applying conditional interference trees/party function
ctreeop_tree = ctree(high ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc+ Age + Education + Urban + US, data = company_train)
summary(op_tree)
plot(op_tree)
#from the above tree shelv is good 60% chance customer will buy
#shelve loc having a bad or medium and price less than or equal to 87 60% chance of high sales
#if it is greater than or equal to 87 advertising is less than or equal to 7 zero percent chance of high sales
#the same condition if adverstiment is greater than eight twenty percent chance of high sales
pred_tree <- as.data.frame(predict(op_tree,newdata=company_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(op_tree,newdata=company_test)
mean(pred_test_df==c_data$high)
#Accuracy = 68%
CrossTable(company_test$high, pred_test_df)
#checking confusion matrix
confusionMatrix(company_test$high, pred_test_df)
cd_tree_org <- tree(high~.-Sales,data=c_data)
summary(cd_tree_org)
plot(cd_tree_org)
text(cd_tree_org,pretty = 0)
#Applying tree function for training dataset
cd_tree <- tree(high~.-Sales,data=company_train)
summary(cd_tree)
plot(cd_tree)
text(cd_tree,pretty = 0)
#predicting the test data using model
pred_tree <- as.data.frame(predict(cd_tree,newdata=CD_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(cd_tree,newdata=company_test)
pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]
pred_tree$final <- as.factor(pred_tree$final)
summary(pred_tree$final)
summary(company_test$high)
mean(pred_tree$final==c_data$high)
CrossTable(company_test$high,pred_tree$final)
confusionMatrix(company_test$high,pred_tree$final)
