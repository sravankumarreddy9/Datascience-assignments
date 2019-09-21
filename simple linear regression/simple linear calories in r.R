library(readr)
calories<- read.csv(file.choose())
#checking for variables dispersion individually using qqnorm function
attach(calories)
qqnorm(calories$caloriesconsumed)
qqline(calories$caloriesconsumed) # there is no problem with values
qqnorm(calories$weightgained)
qqline(calories$weightgained) # these values are dispersed slightly away from the qqline 

plot(density(calories$caloriesconsumed)) #for the bell shaped curve
plot(density(calories$weightgained))

#for calories consumed it is good for weight gained it is slightly skewed to right. so we transofrm the weight gained values
# Checking for outliers
boxplot(calories$weightgained, calories$caloriesconsumed)

#There are no outliers present in both variables.

#EDA
summary(calories)

#scatter plot
plot(calories$caloriesconsumed, calories$weightgained) #x= calories consumed y= weight gained

#correlation coefficient

cor(caloriesconsumed, weightgained) #cor(x,y) = 0.946991

#simple linear regression model (Y~X)
reg_model1<- lm(weightgained~caloriesconsumed)
summary(reg_model1)       #R2 = 0.89

pred1 <- predict(reg_model1)
reg_model1$residuals
sum(reg_model1$residuals) #sum of errors is zero
sqrt(sum(reg_model1$residuals^2)/nrow(calories)) #RMSE = 103
sqrt(mean(reg_model1$residuals^2))
confint(reg_model1,level=0.95)
predict(reg_model1,interval="predict")
library(ggplot2)

ggplot(data = calories, aes(x = caloriesconsumed, y = weightgained)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories, aes(x=caloriesconsumed, y=weightgained))


ggplot(calories,aes(caloriesconsumed,weightgained))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

#checking for Y~log(X)
cor(log(caloriesconsumed), weightgained) #cor =0.89
plot(log(caloriesconsumed), weightgained)
model2 <- lm(weightgained ~ log(caloriesconsumed))
summary(model2)
#R2 = 0.80
pred2 <- predict(model2)
model2$residuals
sum(model2$residuals)
sqrt(sum(model2$residuals^2)/nrow(calories)) #RMSE = 141

#checking for log(Y)~X
plot(caloriesconsumed, log(weightgained))

  #Logrthmic Model
#x = caloriesconsumed, y = log(Weightgained)

cor(caloriesconsumed, log(weightgained))#cor = 0.93
model3<-lm(log(weightgained)~caloriesconsumed)
summary(model3) #R2 = 0.87
predict(model3)
sum(model3$residuals) #sum of errors = 0
logwg <- predict(model3)
wg <- exp(logwg)
error = calories$weightgained - wg
sqrt(sum(error^2)/nrow(calories)) #RMSE = 118
model3$residuals
confint(model3,level=0.95)
predict(model3,interval="confidence")

#check for log(Y)~log(X)
cor(log(caloriesconsumed), log(weightgained)) #cor = 0.92
model4 <- lm(log(weightgained)~log(caloriesconsumed))
summary(model4) #R2 = 0.84
#R squared value is less we stop it here.

#check for polynomial degree with two curve (Y~X+(X2))
cor(caloriesconsumed*caloriesconsumed, weightgained) #cor =0.97
plot(caloriesconsumed*caloriesconsumed, weightgained)
cor(caloriesconsumed*caloriesconsumed, log(weightgained)) #cor = 0.92
model5 <- lm(weightgained ~ caloriesconsumed + I(caloriesconsumed*caloriesconsumed))
summary(model5) #R2 = 0.95
predict(model5)
model5$residuals
sum(model5$residuals)
sqrt(sum(model5$residuals^2)/nrow(calories)) #RMSE = 70

#best fit model is simple linear regression 

