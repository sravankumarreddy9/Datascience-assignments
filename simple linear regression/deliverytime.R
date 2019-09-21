library(readr)
library(ggplot2)
delivery<-read.csv(file.choose())
attach(delivery)

qqnorm(delivery$sorting_time) #distributed around the line
qqline(delivery$sorting_time)

qqnorm(delivery$delivery_time) #distributed around the line
qqline(delivery$delivery_time)
plot(sorting_time, delivery_time)

#Simple linear regression model
#x = sorting time , y = delivery time
cor(sorting_time, delivery_time) #cor = 0.82
plot(sorting_time, delivery_time)
reg_model1 <- lm(delivery_time ~ sorting_time)
summary(reg_model1) #R2 = 0.68 
pred1 <- predict(reg_model1)
reg_model1$residuals
sum(reg_model1$residuals) #sum of errors = 0
sqrt(sum(reg_model1$residuals^2)/nrow(delivery)) #RMSE = 2.79

# Logrthemic model
#x = log(sorting time), y = delivery time
cor(log(sorting_time), delivery_time) #cor = 0.83
plot(log(sorting_time), delivery_time)
reg_logmodel <- lm(delivery_time ~ log(sorting_time))
summary(reg_logmodel) #R2 = 0.69
pred2 <- predict(reg_logmodel)
reg_logmodel$residuals
sum(reg_logmodel$residuals) #sum of errors = 0
sqrt(sum(reg_logmodel$residuals^2)/nrow(delivery)) #RMSE = 2.73

ggplot(data = delivery, aes(x = log(sorting_time), y = delivery_time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delivery, aes(x=log(sorting_time), y=pred2))
#exponential model
# x= sorting time, y= log(delivery time)
cor(sorting_time, log(delivery_time)) #cor = 0.84
plot(sorting_time, log(delivery_time))
reg_expmodel <- lm(log(delivery_time)~sorting_time)
summary(reg_expmodel) #R2 = 0.71
pred3 <- predict(reg_expmodel)
reg_expmodel$residuals
sum(reg_expmodel$residuals) #sum of errors = 0
sqrt(mean(reg_expmodel$residuals^2))
logdt <- predict(reg_expmodel)
dt <- exp(logdt)     
error = delivery$delivery_time - dt
error
sqrt(sum(error^2)/nrow(delivery)) #RMSE = 2.94
ggplot(data = delivery, aes(x =sorting_time, y = log(delivery_time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delivery, aes(x=sorting_time, y=logdt)))

#After using polynomial R2 values are decreasing hence we will stop with three transformations as the graph is not curve.
#Exponential model is best fit model
#Final model will be log(deliverytime)= 2.12 + 0.10(sorting time)
