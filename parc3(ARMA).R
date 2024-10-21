library(tseries)

value = c(100,110,120,130,140,150,160,170,180,190,200,210,220,230,240)

ts_data = ts(value,start = 1 , frequency = 1)
ts_data

adf = adf.test(ts_data)
adf
acf(ts_data , main = "ACF of Original Time Series")
pacf(ts_data , main = "PACF of Original Time Series")

arma_model = arima(ts_data , order = c(1,0,1))
summary(arma_model)

#chackresidual 
checkresiduals(arma_model)

#Forecast the next 10 values using the fitted model
forecast_value = forecast(arma_model,h=10)
forecast_value

#Plot the Original Time Series, Fitted Values, and Forecasted Values
plot(forecast_value, main = "Original and Forecasted Values", 
     xlab = "Time", ylab = "Values", ylim = c(100, 300))
lines(ts_data, col = "blue", lwd = 2)  # Original values
lines(fitted(arma_model), col = "red", lwd = 2)  # Fitted values
legend("topleft", legend = c("Original", "Fitted", "Forecast"),
       col = c("blue", "red", "black"), lty = 1, bty = "n")



#question2
sales = c(500,520,540,560,580,600,620,640,660,680,700,720)

ts_data3 = ts(sales,start=1,frequency = 4)
ts_data3

adf = adf.test(ts_data3)
adf

acf(ts_data3 , main="ACF Graph")
pacf(ts_data3, main="PACF")

arma_model = arima(ts_data3 ,order = c(1,0,1))
arma_model

checkresiduals(arma_model)


# Step 5: Forecast the Next 12 Values
forecasted_values <- forecast(arma_model, h = 12)
forecast_value
# Step 6: Plot the Original Time Series, Fitted Values, and Forecasted Values
plot(forecasted_values, main = "Original and Forecasted Sales Values", 
     xlab = "Quarter", ylab = "Sales")
lines(ts_data3, col = "blue", lwd = 2)  # Original values
lines(fitted(arma_model), col = "red", lwd = 2)  # Fitted values
legend("topleft", legend = c("Original", "Fitted", "Forecast"),
       col = c("blue", "red", "black"), lty = 1, bty = "n")
