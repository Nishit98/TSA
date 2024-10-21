#Consider an AR(1) process defined by the equation: Xt=0.5Xt−1+ϵt 
library(forecast)
library(tseries)

#simulate 100 obs from AR(1) process
set.seed(42)
n = 100
phi = 0.5
ar1_process = arima.sim(model = list(ar = phi),n=n)
ar1_process

#plot ts data
plot.ts(ar1_process,col='blue')

#estimation of AR(1) process parameter using ARIMA function
ar1_fit = arima(ar1_process,order = c(1,0,0))
ar1_fit$coef[1]

#plot ACF and PACF
acf(ar1_process,main="ACF of AR(1)")
pacf(ar1_process,main="PACF of AR(1)")

#Fit AR(1) and AR(2) model to data 
ar1_model = arima(ar1_process,order = c(1,0,0))
ar1_model
ar2_model = arima(ar1_process,order = c(2,0,0))
ar2_model

#forcast for next 10obs
forecast_value = forecast(ar1_model,h=10)
forecast_value
plot(forecast_value)


#for MA(q) Consider an MA(1) Process: Xt = εt + 0.5εt-1 

set.seed(42)
n=100
phi=0.5
ma1_process = arima.sim(model = list(ma = phi),n = n)
ma1_process

plot(ma1_process)

#estimation of ma(1) parameter
ma1_fit = arima(ma1_process , order = c(0,0,1))
ma1_fit$coef[1]

par(mfrow = c(1, 2))
acf(ma1_process,main = "ACF of MA(1)")
pacf(ma1_process , main="PACF of MA(1)")
par(mfrow = c(1, 1)) 

#fit MA(1) and MA(2) model
ma1_model = arima(ma1_process,order = c(0,0,1))
ma1_model
ma2_model = arima(ma1_process,order = c(0,0,2))
ma2_model


forecast_value = forecast(ma1_model,h=10)
plot(forecast_value, main="ma(1)")

