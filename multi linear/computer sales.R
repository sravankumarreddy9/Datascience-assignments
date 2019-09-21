library(plyr)
sales_data <- read.csv(file.choose())
sales_data1 <- sales_data
#converting to categorical values to numerical value
sales_data1$cd <- as.numeric(revalue(sales_data1$cd, c("yes"=1, "no"=0)))
sales_data1$multi <- as.numeric(revalue(sales_data1$multi, c("yes"=1, "no"=0)))
sales_data1$premium <- as.numeric(revalue(sales_data1$premium, c("yes"=1, "no"=0)))
View(sales_data1)
class(sales_data1)
attach(sales_data1)
#EDA
summary(sales_data1)
plot(speed, price)
plot(hd, price)
plot(ram, price)
plot(screen, price)
plot(cd, price)
plot(multi, price)
plot(premium, price)
plot(ads, price)
plot(trend, price)
pairs(sales_data1)
cor(sales_data1)
#creating model simple linear regression
model <- lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend)
summary(model)
#R2 = 0.77
library(corpcor)
cor2pcor(cor(sales_data1))
library(mvinfluence)
library(car)
influence.measures(model)
influenceIndexPlot(model, id.n=3)
influencePlot(model, id.n=3)
#logirthmeic model
model_log <- lm(price~log(speed)+log(hd)+log(ram)+log(screen)+
                               log(cd)+log(multi)+log(premium)+log(ads)+log(trend)
                             ,data=sales_data1[-c(1441,1701),])
summary(model_log) 
#r2 = 0.74
confint(model_log,level=0.95)
predict(model_log,interval="predict")
model_log2<-lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend,
                         data=sales_data1[-c(1441,1701),])
summary(model_log2)
#r2 = 0.77
#exponential model
model_exp <- lm(log(price)~speed+hd+ram+screen+cd+multi+premium+ads+trend,data=sales_data1[-c(1441,1701),])
summary(model_exp)
#r2 = 0.78
#quadratic model
model_quad <- lm(price~speed+I(speed^2)+hd+I(hd^2)+ram+I(ram^2)+screen+I(screen^2)+cd+I(cd^2)+multi+I(multi^2)+
                   premium+I(premium^2)+ads+I(ads^2)+trend+I(trend^2),data=sales_data1[-c(1441,1701),])
summary(model_quad)
#r2 = 0.80
confint(model_quad,level=0.95)
predict(model_quad,interval="predict")
model_poly <- lm(price~speed+I(speed^2)+I(speed^3)+
                            hd+I(hd^2)+I(hd^3)+
                            ram+I(ram^2)+I(ram^3)+
                            screen+I(screen^2)+I(screen^3)+
                            cd+I(cd^2)+I(cd^3)+
                            multi+I(multi^2)+I(multi^3)+
                            premium+I(premium^2)+I(premium^3)+
                            ads+I(ads^2)+I(ads^3)+
                            trend+I(trend^2)+I(trend^3),data=sales_data1[-c(1441,1701),])
summary(model_poly)
#r2 = 0.81
FinalModel<-lm(price~speed+I(speed^2)+I(speed^3)+
                 hd+I(hd^2)+I(hd^3)+
                 ram+I(ram^2)+I(ram^3)+
                 screen+I(screen^2)+I(screen^3)+
                 cd+I(cd^2)+I(cd^3)+
                 multi+I(multi^2)+I(multi^3)+
                 premium+I(premium^2)+I(premium^3)+
                 ads+I(ads^2)+I(ads^3)+
                 trend+I(trend^2)+I(trend^3),data=sales_data1[-c(1441,1701),])

summary(FinalModel)
#r2 =0.81 it is polynomial model
Profit_Predict <- predict(FinalModel)
View(Profit_Predict)

finplot <- sales_data1[-c(1441,1701),]
View(finplot)

plot1 <- cbind(finplot$price, Profit_Predict)
pairs(plot1)
Final <- cbind(speed,hd,ram,screen,cd,multi,premium,ads,trend,price,Profit_Predict)
pairs(Final)
View(Final)
plot(FinalModel)
#polynomial model is the best fit model