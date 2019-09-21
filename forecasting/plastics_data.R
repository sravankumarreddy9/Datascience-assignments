library(readr)
library(forecast)
library(fpp)
library(smooth)
Plastics <- read.csv(file.choose())
View(Plastics)
#seasonality is 12 months
#plotting the sales variable
plot(Plastics$Sales, type = "o")
#creating 12 dummy variables
x <- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )
View(x)
#Assigning month names
colnames(x) <- month.abb
View(x)
#storing to the data frame 
Plasticsdata <- cbind(Plastics, x)
View(Plastics)
colnames(Plastics)
Plasticsdata["t"] <- 1:60
View(Plasticsdata)
Plasticsdata['log_sales'] <- log(Plasticsdata["Sales"])
Plasticsdata["t_square"]<-Plasticsdata["t"]*Plasticsdata["t"]
attach(Plasticsdata)
#dividing the data into train and test  
train<-Plasticsdata[1:48,]
test<-Plasticsdata[49:60,]
#simple linear regression 
linear_model<-lm(Sales~t,data=train)
summary(linear_model)
#predicting values
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear
#exponential model
expo_model<-lm(log(Sales)~t,data=train)
summary(expo_model)
#predicting values
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo
#quadratic model
Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
#predicting values 
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad 
#additive seasonality model
sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(sea_add_model)
#predicting values
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add
#Additive seasonality with linearity
Add_sea_Linear_model<-lm(Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Linear_model)
#predicting values 
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear
#Additive seasonality with Quadratic
Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Quad_model)
#predicting values
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad
#multiplicative seasonality 
multi_sea_model<-lm(log(Sales)~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_sea_model)
#predicting values 
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea
#multiplicative seasonality with linear trend
multi_add_sea_model<-lm(log(Sales)~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_add_sea_model) 
#predicting values 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea
#creating table for rmse values
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
#final model 
final_model <-lm(log(Sales)~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = Plasticsdata)
final_model_pred<-data.frame(predict(final_model,newdata=Plasticsdata,interval='predict'))
new_model_fin <- exp(final_model$fitted.values)
View(new_model_fin)

Month <- as.data.frame(Plasticsdata$Month)
Final <- as.data.frame(cbind(Month,Plasticsdata$Sales, new_model_fin))
colnames(Final) <-c("Month","Sales","New_Pred_Value")
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Months",
     col.axis="blue",type="o") 
plot(Final$New_Pred_Value, main = "PredictedGraph", xlab="Sales(Predicted)", ylab="Months",
     col.axis="Green",type="s")
View(Final)
