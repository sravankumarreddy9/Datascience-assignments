library(neuralnet)
library(nnet)
library(NeuralNetTools)
concrete_data <- read.csv(file.choose())
str(concrete_data)
attach(concrete_data)
#EDA
hist(cement, prob = T, breaks = 30)
lines(density(cement))
summary(cement)
hist(slag, prob = T, breaks = 30)
lines(density(slag))
summary(slag)
summary(concrete_data)
#normalizing the data 
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
normalized_data <-as.data.frame(lapply(concrete_data,FUN=normalize))
summary(normalized_data$strength)
summary(strength)
#splitting the data into train and test
set.seed(123)
ind <- sample(2, nrow(normalized_data), replace = TRUE, prob = c(0.7,0.3))
concrete_train <- normalized_data[ind==1,]
concrete_test  <- normalized_data[ind==2,]
#creating a model
model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train)
str(model)
plot(model, rep="best")
summary(model)
par(mar = numeric(4), family = 'serif')
plotnet(model, alpha = 0.6)
#evaluating model performance
set.seed(12323)
model_results <- compute(model,concrete_test[1:8])
predicted_strength <- model_results$net.result
#predicted strength and actual strength of test data
cor(predicted_strength,concrete_test$strength)
str_max <- max(concrete_data$strength)
str_min <- min(concrete_data$strength)
#unnormalize the prediction to get the actual prediction of strength
unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualStrength_pred <- unnormalize(predicted_strength,str_min,str_max)
head(ActualStrength_pred)
set.seed(12345)
model2 <- neuralnet(strength~cement+slag+ash+water+superplastic+
                               coarseagg+fineagg+age,data= concrete_train,
                             hidden = 5)
plot(model2, rep = "best")
summary(model2)
model_results2<-compute(model2,concrete_test[1:8])
predicted_strength2<-model_results2$net.result
cor(predicted_strength2,concrete_test$strength)
plot(predicted_strength,concrete_test$strength)
par(mar = numeric(4), family = 'serif')
plotnet(model2, alpha = 0.6)
#SSS Error has reduced and training steps had been increased as the number of neurons under hidden layer are increased
