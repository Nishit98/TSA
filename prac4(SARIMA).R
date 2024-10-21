library(tseries)
library(ggplot2)

# Create the sales data
sales_data <- c(200, 220, 240, 210, 230, 250, 300, 270, 260, 280, 310, 330, 
                220, 230, 250, 240, 260, 280, 320, 300, 290, 310, 330, 350, 
                240, 250, 270, 260, 280, 300, 340, 320, 310, 330, 350, 370, 
                260, 270, 290, 280, 300, 320, 360, 340, 330, 350, 370, 390, 
                280, 290, 310, 300, 320, 340, 380, 360, 350, 370, 390, 410)

ts_data = ts(sales_data , start = c(2019,1),frequency = 12)
ts_data

#plot the sales_data
plot(ts_data,main="sales_data")
# Observations: Check for seasonality and trend by inspecting the plot

# Differencing to remove trend
diff_sales_ts = diff(ts_data , differences =1)
plot(diff_sales_ts)

#2) MODEL IDETIFICATION
#A)
# Perform Augmented Dickey-Fuller test for stationarity
adf = adf.test(ts_data)
adf
# If p-value > 0.05, differencing is needed (non-stationary).


# Seasonal differencing can be checked by examining the ACF plot
Acf(ts_data)
# Look for seasonal lags in the ACF plot (e.g., significant peaks at 12 months).

#acf and pacf
acf(diff_sales_ts , main = "ACF plot")
pacf(diff_sales_ts , main ="PACF plot")

#3) SARIMA MODEL FITTING
sarima_model = auto.arima(ts_data , seasonal = TRUE)
summary(sarima_model)

# Plot residuals to check for white noise
checkresiduals(ts_data)

#4)forcast for the 12-month
forecast_value = forecast(ts_data,h=12)
plot(forecast_value, main = "Sales Forecast for Next 12 Months", xlab = "Time", ylab = "Sales")
lines(ts_data,col='red')

#5)model comparison
arima_model = auto.arima(ts_data , seasonal = FALSE)
arima_model

#compare with AIC and BIC AND RMSE
sarima_aic = AIC(sarima_model)
sarima_bic = BIC(sarima_model)
sarima_rmse = sqrt(mean(residuals(sarima_model)^2))

arima_aic = AIC(arima_model)
arima_bic = BIC(arima_model)
arima_rmse = sqrt(mean(residuals(arima_model)^2))

print(paste("SARIMA AIC:", sarima_aic, "BIC:", sarima_bic, "RMSE:", sarima_rmse))
print(paste("ARIMA AIC:", arima_aic, "BIC:", arima_bic, "RMSE:", arima_rmse))


#6) Holiday adjustment in sarima

# Suppose you have a holiday variable (1 for holiday, 0 for non-holiday)
# Add holiday variable as external regressor
holiday_effect = c(rep(0,11),1,rep(0,11),1,rep(0,11),1,rep(0,11),1,rep(0,11),1)

# Fit SARIMA with external regressors
sarima_holiday = auto.arima(ts_data,xreg = holiday_effect,seasonal = TRUE)
summary(sarima_holiday)

# Compare the SARIMA with and without holiday adjustments using AIC, BIC, and RMSE
sarima_holiday_aic <- AIC(sarima_holiday)
sarima_holiday_bic <- BIC(sarima_holiday)
sarima_holiday_rmse = sqrt(mean(residuals(sarima_holiday)^2))

print(paste("SARIMA with holiday AIC:", sarima_holiday_aic, "BIC:", sarima_holiday_bic, "RMSE:", sarima_holiday_rmse))

