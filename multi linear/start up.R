library(plyr)
start_up <- read.csv(file.choose())
class(start_up)
attach(start_up)
#converting the categorical to numerical value
start_up$State <- revalue(start_up$State, c("New York"="0", "California"="1", "Florida"="2"))
attach(start_up)
start_up <- cbind(RD_Spend=R.D.Spend,Administration,Marketing_Spend=Marketing.Spend,State,Profit)
start_up <- as.data.frame(start_up)
attach(start_up)
#EDA
summary(start_up)
plot(R.D.Spend, Profit)
plot(Administration, Profit)
plot(Marketing.Spend, Profit)
plot(State, Profit)
pairs(start_up)
#correaltion coefficient
cor(start_up)
#creating linear regression model
model <- lm(Profit~RD_Spend+Administration+Marketing_Spend+State)
summary(model)
#applying log on administration
model2 <- lm(Profit~RD_Spend+log(Administration))
summary(model2)
library(corpcor)
cor2pcor(cor(start_up))
library(mvinfluence)
library(car)
influence.measures(model)
influenceIndexPlot(model, id.n=3)
influencePlot(model, id.n=3)
#logarthimic model
model_log<-lm(Profit~RD_Spend+log(Administration)+Marketing_Spend+log(State),data=start_up[-c(49,50),])
summary(model_log)
confint(model_log,level=0.95)
predict(model_log,interval="predict")
model_final<-lm(Profit~RD_Spend+Administration+Marketing_Spend+State,data=start_up[-c(49,50),])
summary(model_final)
#exponential model
model_exp<-lm(log(Profit)~RD_Spend+Administration+Marketing_Spend+State,data=start_up[-c(49,50),])
summary(model_exp)
model_exp1<-lm(log(Profit)~RD_Spend+Marketing_Spend,data=start_up[-c(49,50),])
summary(model_exp1)
#quadratic model
model_quad <- lm(Profit~RD_Spend+I(RD_Spend^2)+Administration+I(Administration^2)
                          +Marketing_Spend+I(Marketing_Spend^2)+State+I(State^2),data=start_up[-c(49,50),])
summary(model_quad)
confint(model_quad,level=0.95)
predict(model_quad,interval="predict")
model_quad1 <- lm(Profit~RD_Spend+I(RD_Spend^2)+Marketing_Spend+I(Marketing_Spend^2)
                           ,data=start_up[-c(49,50),])
summary(model_quad1)
#poly model
model_poly <- lm(Profit~RD_Spend+I(RD_Spend^2)+I(RD_Spend^3)+
                            Administration+I(Administration^2)+I(Administration^3)+
                            Marketing_Spend+I(Marketing_Spend^2)+I(Marketing_Spend^3)+
                            State+I(State^2)+I(State^3),data=start_up[-c(49,50),])
summary(model_poly)
model_poly1 <- lm(Profit~RD_Spend+I(RD_Spend^2)+I(RD_Spend^3)+
                             Marketing_Spend+I(Marketing_Spend^2)+I(Marketing_Spend^3)
                           ,data=start_up[-c(49,50),])
summary(model_poly1) 
vif(model_log)
#final model
finalmodel<-lm(Profit~RD_Spend+log(Administration)+Marketing_Spend+log(State),data=start_up[-c(49,50),])
summary(finalmodel)
Profit_Predict <- predict(finalmodel,interval="predict")
Final <- cbind(start_up$RD_Spend,start_up$Administration,start_up$Marketing_Spend,
               start_up$State,start_up$Profit,Profit_Predict)
View(Final)
plot(finalmodel)
library("MASS")
#checking for least AIC 
stepAIC(finalmodel)
#final model is profit = 8.233e-01(RD_spend) -8.680e+03(Administration) + 1.539e+05