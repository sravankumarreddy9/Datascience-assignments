library(readr)
library(ggplot2)
emp <- read.csv(file.choose())
summary(emp)
attach(emp)
qqnorm(emp$Churn_out_rate)
qqline(emp$Churn_out_rate)

qqnorm(emp$Salary_hike)
qqline(emp$Salary_hike)
plot(Salary_hike, Churn_out_rate)
#as per problem objective find out churnout rate so churnout rate will be the output variable
#simple linear regression model
cor(Salary_hike, Churn_out_rate) #cor = -0.91
reg_model1 <- lm(Churn_out_rate~Salary_hike)
summary(reg_model1) #R2 = 0.83
pred1 <- predict(reg_model1)
reg_model1$residuals
sum(reg_model1$residuals) #sum of errors = 0
sqrt(sum(reg_model1$residuals^2)/nrow(emp)) #RMSE = 3.99

#Logrthemic model
plot(log(Salary_hike), Churn_out_rate) #still it is curve
cor(log(Salary_hike), Churn_out_rate) #cor = -0.92
reg_logmodel <- lm(Churn_out_rate ~ log(Salary_hike))
summary(reg_logmodel) #R2 = 0.84
pred2 <- predict(reg_logmodel)
reg_logmodel$residuals
sum(reg_logmodel$residuals) #sum of errors = 0 
sqrt(sum(reg_logmodel$residuals^2)/nrow(emp)) #RMSE = 3.78

#polynomial model with two degree
plot(Salary_hike*Salary_hike, log(Churn_out_rate))
cor(Salary_hike*Salary_hike, log(Churn_out_rate)) #cor = -0.92
reg_poly<- lm(log(Churn_out_rate)~ Salary_hike*Salary_hike)
summary(reg_poly) #R2 = 0.87
pred3 <- predict(reg_poly)
reg_poly$residuals
sum(reg_poly$residuals) #sum of errors = 0 
logch <- predict(reg_poly)
ch <- exp(logch)
error = emp$Churn_out_rate - ch
error
sqrt(sum(error^2)/nrow(emp)) #RMSE = 3.54

ggplot(data = emp, aes(x = Churn_out_rate + I(Salary_hike^2), y = log(Churn_out_rate))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=Salary_hike+I(Salary_hike^2), y=logch))

#final model log(churnout rate) = 6.638 -(0.00139*salaryhike^2)
