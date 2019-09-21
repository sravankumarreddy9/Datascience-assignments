library('AER')
library(plyr)
affairs_data <- read.csv(file.choose())
#removing the first column as it need to analyze the data
affairs_data <- affairs_data[-1]
affairs1 <- affairs_data
summary(affairs1)
table(affairs1$affairs)
#changing values for the affairs variable for greather than 0 is 1 and if it is equal to 0 is 0
affairs1$ynaffairs[affairs1$affairs > 0] <- 1
affairs1$ynaffairs[affairs1$affairs == 0] <- 0
#converting gender variable to numeric variable
affairs1$gender <- as.factor(revalue(Affairs$gender,c("male"=1, "female"=0)))
affairs1$children <- as.factor(revalue(Affairs$children,c("yes"=1, "no"=0)))
View(affairs1)
class(affairs1)
attach(affairs1)
#creating linear regression model affairs as output variable remaining input variables
model <- lm(ynaffairs ~ factor(gender) + age+ yearsmarried+ factor(children) + religiousness+education+occupation+rating, data = affairs1)
summary(model)
#predicting the values
pred1 <- predict(model,affairs1)
pred1
plot(pred1)
#creating model using glm function
model2 <- glm(ynaffairs ~ factor(gender) + age+ yearsmarried+ factor(children) + religiousness+education+occupation+rating, data = affairs1,family = "binomial")
exp(coef(model2))
#confusion matrix
prob <- predict(model2,affairs1,type="response")
summary(model2)
#compare between different models
#confusion matrix with threshold value is 0.5
confusion<-table(prob>0.5,affairs1$ynaffairs)
confusion
#model accuracy
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
#store predicted values
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
affairs1[,"prob"] <- prob
affairs1[,"pred_values"] <- pred_values
affairs1[,"yes_no"] <- yes_no

View(affairs1[,c(1,9:11)])

table(affairs1$ynaffairs,affairs1$pred_values)
#ROC curve is used to evaluate accuracy of logistic model
library(ROCR)
rocrpred<-prediction(prob,affairs1$ynaffairs)
rocrperf<-performance(rocrpred,'tpr','fpr')
#more area under the ROC curve better the model
str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)
library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
#sorting dataframe tpr in decreasing order
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
