library(forecast)

year <- 1996:2019
obs <- c(150.3,150.9,151.4,151.9,152.5,152.9,153.2,153.7,153.6,153.5,154.4,154.9,155.7,156.3,156.6,156.7,157,
         157.3,157.8,158.3,158.6,158.6,159.1,159.3)

ts_data = ts(obs , start = min(year) ,frequency = 1)

#ex. smoothing
alpha = 0.3
ses_model = ses(ts_data,alpha=alpha)
ses_model

#holt
beta = 0.2
holt_model = holt(ts_data,alpha = alpha , beta = beta)
holt_model

#plot
plot(ts_data,type='o',col='blue',xlab = "year",ylab="Observation")
lines(ses_model$fitted , col = 'red')
lines(holt_model$fitted , col = "green")

legend("topleft",legend = c("Original Data", "SES Smoothed", "Holt's Smoothed"),
       col = c('blue','red','green'),lty=1:2)


#question 2
library(ggplot2)

year = 1991:2003
obs =  c(355.62, 356.36, 357.1, 358.86, 360.9, 362.58, 363.84, 366.58, 368.3, 369.47, 371.03, 373.61, 357.61)

ts_data2 = ts(obs,start=min(year),frequency=1)

plot(ts_data2 , type='o',col='blue')

#moving avg
moving_avg_manual = function(data,window_size){
  n = length(data)
  ma = rep(NA,n)
  for (i in seq(window_size,n)){
    ma[i] = mean(data[(i-window_size+1):i])
  }
  return(ma)
}
moving_avg <- filter(ts_data2, filter=rep(1/3, 3), sides=1)

window_size = 3
ma_model = moving_avg_manual(ts_data2,window_size)
ma_model

forcast_2004 = tail(ma_model,1)
cat("Forecasted CO2 Concentration for 2004 using 3-Year Moving Average: ", forcast_2004, "\n")


#question3
data = data('AirPassengers')

plot(AirPassengers )

#applying Holt-Winter Multipicative model
HoltWinters_model = HoltWinters(AirPassengers,gamma = TRUE , seasonal = 'multiplicative')
HoltWinters_model

#forcast for 12 month
forecast_value = forecast(HoltWinters_model, h = 12)
forecast_value

plot(forecast_value)
