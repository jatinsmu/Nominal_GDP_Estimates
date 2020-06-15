library(readr)
library(caret)
library('forecast')
set.seed(5580)

#Time Series Analysis
data <- read_csv("gdp_by_year_data.csv", col_names = FALSE)

tSeries=ts(data$X2,start=c(1965), end=c(2015), frequency = 1)
plot(tSeries) 

# ==============Simple Moving average==============
sm <- ma(tSeries, order=1) 
sm_forecast = forecast(sm, h=10)
plot(sm_forecast)
accuracy(sm_forecast)

# ==============Exponential Smoothing==============

# Holt Winters: Level, Trend and Seasonality
model <- hw(tSeries, initial = 'optimal', h=(10))
plot(model)
accuracy(model) # calculate accuracy measures

# ==============Arima==============
# training using auto arima which automatically uses best arima model after iterations
ar_min = auto.arima(tSeries, trace=TRUE)
summary(ar_min)
# predicting 
futurVal <- forecast(ar_min,h=10, level=c(95))
plot(futurVal)
# calculating mape value for arima
#mape_ar=lapply(100*abs(fitted(ar_min) - tSeries)/tSeries, mean, na.rm = TRUE)
#mape_ar # 3.32
accuracy(futurVal)