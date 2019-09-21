library(neuralnet)
library(nnet) 
library(NeuralNetTools)
library(plyr)
library(NeuralNetTools)
startup <- read.csv(file.choose())
class(startup)
#converting the categorical data to numeric
startup$State <- as.numeric(revalue(startup$State,c("New York"="0", "California"="1","Florida"="2")))
str(startup)
startup <- as.data.frame(startup)
attach(startup)
#checking the scatterplot for linearity of variables with resect to profit
plot(R.D.Spend, Profit)
plot(Administration, Profit)
plot(Marketing.Spend, Profit)
plot(State, Profit)
#correaltion for how strong the data is
cor(startup)
#matrix plot for the variables 
pairs(startup)
summary(startup)
#normalize function 
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
normalized_data<-as.data.frame(lapply(startup,FUN=normalize))
summary(normalized_data$Profit)
set.seed(123)
#splitting the data into train and test
ind <- sample(2, nrow(normalized_data), replace = TRUE, prob = c(0.7,0.3))
Startups_train <- normalized_data[ind==1,]
startups_test  <- normalized_data[ind==2,]
#creating the model taking profit as output variable on training dataset
model <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+State,data = Startups_train)
model
plot(model, rep = "best")
summary(model)
par(mar = numeric(4), family = 'serif')
plotnet(model, alpha = 0.6)
set.seed(12323)
model_results <- compute(model,startups_test[1:4])
predicted_profit <- model_results$net.result
#predicted profit and actual profit of the test data
cor(predicted_profit,startups_test$Profit)
str_max <- max(startup$Profit)
str_min <- min(startup$Profit)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualProfit_pred <- unnormalize(predicted_profit,str_min,str_max)
head(ActualProfit_pred)
set.seed(12345)
model2 <- neuralnet(Profit~R.D.Spend+Administration
                             +Marketing.Spend+State,data = Startups_train,
                             hidden = 2)
plot(model2 ,rep = "best")
summary(model2)
model_results2<-compute(model2,startups_test[1:4])
predicted_Profit2<-model_results2$net.result
cor(predicted_Profit2,startups_test$Profit)
plot(predicted_Profit2,startups_test$Profit)
par(mar = numeric(4), family = 'serif')
plotnet(Startups_model2, alpha = 0.6)
#observations
#if the number of neurons in hidden layers is increased error has been reduced.
