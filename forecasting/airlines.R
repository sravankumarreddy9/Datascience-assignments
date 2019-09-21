library(forecast)
library(fpp)
library(smooth)
library(readxl)
library(tseries)
library(readr)
airlines_data <- read_excel(file.choose())
attach(airlines_data)
airlines_data <- airlines_data$Passengers
#to create time series object
airlines_data <- as.ts(airlines_data)
View(airlines_data)
class(airlines_data)
#applying time series function with start and end of frequency
airlines_data1 <- ts(airlines_data, start = c(1995,1), end = c(2002,12),frequency = 12)
start(airlines_data1)
end(airlines_data1)
class(airlines_data1)
#checking for na values
sum(is.na(airlines_data1))
summary(airlines_data1)
View(airlines_data1)
#decomposing the data to check seasonal, trend seperately to check changes over time
data_decomposing <- decompose(airlines_data1, "multiplicative")
#checking in graph
plot(data_decomposing)
plot(data_decomposing$seasonal)
plot(data_decomposing$trend)
plot(data_decomposing$random)
#EDA
plot(airlines_data1)
cycle(airlines_data1)
boxplot(airlines_data1~cycle(airlines_data1,xlab = "Date", ylab = "Passenger Number(100's)",main = "Monthly Boxplot of passengers from 1995 to 2002"))
#creating model using auto arima
model <- auto.arima(airlines_data1)
model
auto.arima(airlines_data1, ic = "aic", trace = TRUE)
#tseries evaluation
plot.ts(model$residuals)
acf(ts(model$residuals),main = 'ACF Residual')
pacf(ts(model$residuals),main = 'PACF Residual')
#forecast for next 2 years
Pass_Forecast <- forecast(model,Level=c(95),h=10*12)
plot(Pass_Forecast)
#testing the final model
Box.test(model$resid, lag = 5, type = "Ljung-Box")
Box.test(model$resid, lag = 15, type = "Ljung-Box")
Box.test(model$resid, lag = 10, type = "Ljung-Box")
