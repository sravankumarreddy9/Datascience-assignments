library(readr)
sal <- read.csv(file.choose())
summary(sal)
attach(sal)
boxplot(sal)
#scatter plot
plot(sal)
#from the plot it is showing constant linear between variables
cor(sal$YearsExperience, sal$Salary)
#cor = 0.97 It means it has strong linear relationship

#Simple linear regression model
reg_model1 <- lm(sal$Salary ~ sal$YearsExperience)
summary(reg_model1)   #R2 = 0.95
pred <- predict(reg_model1)
reg_model1$residuals
sum(reg_model1$residuals) #sum of errors is zero
mean(reg_model1$residuals)

sqrt(sum(reg_model1$residuals^2)/nrow(sal)) #RMSE = 5592.044
sqrt(mean(reg_model1$residuals^2))

confint(reg_model1,level=0.95)
predict(reg_model1,interval="predict")


library(ggplot2)


ggplot(data = sal, aes(x = sal$YearsExperience, y = sal$Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal, aes(x=sal$YearsExperience, y=sal$Salary))

# Logrithamic Model
#x = Years Experience, y = salary
plot(log(YearsExperience), Salary)
plot(YearsExperience, log(Salary))
cor(log(YearsExperience), Salary) #cor = 0.92
reg_logmodel <- lm(Salary ~ log(YearsExperience))
summary(reg_logmodel) #R2 = 0.85
pred1 <- predict(reg_logmodel)
reg_logmodel$residuals
sum(reg_logmodel$residuals) #sum of errors is zero
sqrt(sum(reg_logmodel$residuals^2)/nrow(sal)) #RMSE = 10302.89

#Exponential Model
#x = Yearsexperience y= log(salary)
plot(YearsExperience, log(Salary))
cor(YearsExperience, log(Salary)) #cor = 0.96
reg_expmodel <- lm(log(Salary) ~ YearsExperience )
summary(reg_expmodel) #R2 = 0.93

reg_expmodel$residuals


logat <- predict(reg_expmodel)
at <- exp(logat)

error = sal$Salary - at
error

sqrt(sum(error^2)/nrow(sal))  #RMSE = 7213.235

confint(reg_expmodel,level=0.95)
predict(reg_expmodel,interval="confidence")
#After seeing with three transformations with observing values we will go with simple regression model
#model will be salary = 9450 (yearsexperience) + 25792.2
