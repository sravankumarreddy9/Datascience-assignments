library(forecast)
library(fpp)
library(smooth)
library(readxl)
library(tseries)
cocacola_data <- read_excel(file.choose())
cocacola_data <- cocacola_data$Sales
#to create time series object
cocacola_data <- as.ts(cocacola_data)
View(cocacola_data)
class(cocacola_data)
#applying time series function with start and end of frequency
cocacola_data1 <- ts(cocacola_data, start = c(1986, 1), end = c(1995, 6), frequency = 4)
start(cocacola_data1)
end(cocacola_data1)
class(cocacola_data1)
#checking for na values
sum(is.na(cocacola_data1))
summary(cocacola_data1)
View(cocacola_data1)
#decomposing the data to check seasonal, trend seperately to check changes over time
data_decompose <- decompose(cocacola_data1, "multiplicative")
#checking in graph
plot(data_decompose)
plot(data_decompose$seasonal)
plot(data_decompose$trend)
plot(data_decompose$random)
#EDA
plot(cocacola_data1)
abline(reg=lm(cocacola_data1~time(cocacola_data1)))
cycle(cocacola_data1)
boxplot(cocacola_data1~cycle(cocacola_data1,xlab = "Date", ylab = "Passenger Number(100's)",main = "Monthly Boxplot of passengers from 1995 to 2002"))
#creating model using auto arima
model <- auto.arima(cocacola_data1)
model
auto.arima(cocacola_data1, ic = "aic", trace = TRUE)
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
