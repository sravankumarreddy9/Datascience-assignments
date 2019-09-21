library(randomForest)
library(MASS)
library(caret)
set.seed(123)
computer_sales <- read.csv(file.choose())
hist(computer_sales$Sales, main = "Sales of Companydata",xlim = c(0,20),
   breaks=c(seq(10,20,30)), col = c("blue","red", "green","violet"))
#if the sales are greater than 8 then sales is high
highsales = ifelse(computer_sales$Sales<9, "No", "Yes")
C_sales = data.frame(computer_sales[2:11], highsales)
str(C_sales)
table(C_sales$highsales)
#data partition for train and test 
set.seed(123)
ind <- sample(2, nrow(C_sales), replace = TRUE, prob = c(0.7,0.3))
train <- C_sales[ind==1,]
test  <- C_sales[ind==2,]
set.seed(213)
rf <- randomForest(highsales~., data=train)
rf
#error percentage is 16%
attributes(rf)
#prediction for training dataset
pred1 <- predict(rf, train)
head(pred1)
head(train$highsales)
confusionMatrix(pred1, train$highsales)
#prediction for test dataset
pred2 <- predict(rf, test)
confusionMatrix(pred2, test$highsales)
plot(rf)
tune <- tuneRF(train[,-11], train[,11], stepFactor = 0.5, plot = TRUE, ntreeTry = 300,
               trace = TRUE, improve = 0.05)
rf1 <- randomForest(highsales~., data=train, ntree = 300, mtry = 3, importance = TRUE,
                    proximity = TRUE)
rf1
pred1 <- predict(rf1, train)
confusionMatrix(pred1, train$highsales)  
pred2 <- predict(rf1, test)
confusionMatrix(pred2, test$highsales)
hist(treesize(rf1), main = "No of Nodes for the trees", col = "green")
varImpPlot(rf1)
varImpPlot(rf1 ,Sort = T, n.var = 5, main = "Top 5 -Variable Importance")
importance(rf1)
varUsed(rf)
#partial plot
partialPlot(rf1, train, Price, "Yes")
#single tree from forest
getTree(rf, 1, labelVar = TRUE)
#multi dimension scaling plot of matrix
MDSplot(rf1, C_sales$highsales)
