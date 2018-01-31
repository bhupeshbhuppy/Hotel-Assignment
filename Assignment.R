rm(list = ls())
if(!require("tseries")) install.packages("tseries")
if(!require("highcharter")) install.packages("highcharter")
if(!require("reshape2")) install.packages("reshape2")
if(!require("lubridate")) install.packages("lubridate")
if(!require("plotly")) install.packages("plotly")
if(!require("urca")) install.packages("urca")
if(!require("forecast")) install.packages("forecast")

time<- read.csv("Hotel_ts.csv")
### Data pre-prossing: Converting data to appropriate time series format
start <- as.POSIXct("1992-01-01")
end <- as.POSIXct("2015-12-01")
time$date<-seq(from=start, by="month", to=end)
time_ts<- ts(time$Number, start = c(1992,1), end =c(2015,12) , frequency = 12 )

### Finding Outlier:

### Plotting the timeseries of data
hchart(time_ts)

#### Box Plot for seasonality and outlier
plot_ly(time, y =~Number, type = "box")
temp<-data.frame("Period" = time$date, "Month"= substr(time$date,6,7),
                 "Year" = substr(time$date,1,4),"data" = time$Number)
temp<-melt(temp[,-1])
plot_ly(temp, y =~value, color = ~Month, type = "box")

#### There is seasonality in data as the mean of every month is different
decomposed_data <- stl(time_ts, "per")
hchart(decomposed_data)
decomposed_data<-as.data.frame(decomposed_data$time.series)

#### Removing seasonality in data
time$deseasonalized<-time$Number-decomposed_data[,1]
hchart(ts(time$deseasonalized))

#### Removing trend but lets first check the stationarity
###There  is trend as well but a small trend

modelling_data<-ts(time$deseasonalized,start = c(1992,1), end =c(2015,12) , frequency = 12 )
summary(ur.df(modelling_data,lags = trunc((length(modelling_data) - 1)^(1/3)),selectlags = "AIC"))
adf.test(modelling_data)
#### Non-Stationary Data; Lets remove trend
time$detrended<-time$deseasonalized- decomposed_data$trend
hchart(ts(time$detrended,start = c(1992,1), end =c(2015,12) , frequency = 12 ))

#### Checking Seasonality again
temp<-data.frame("Period" = time$date, "Month"= substr(time$date,6,7),
                 "Year" = substr(time$date,1,4),"data" = time$deseasonalized)
temp<-melt(temp[,-1])
plot_ly(temp, y =~value, color = ~Month, type = "box")

### Now only the variance varies but the mean is more or less same but there seems 
### to be a pattern in deviation in variance of data therefore checking for seasonality again

decomposed_data1 <- stl(ts(time$deseasonalized,start = c(1992,1), end =c(2015,12) , frequency = 12 ),
                        "per")
hchart(decomposed_data1)
decomposed_data1<-as.data.frame(decomposed_data1$time.series)
time$deseasonalized1<-time$detrended-decomposed_data1[,1]
hchart(ts(time$deseasonalized1,start = c(1992,1), end =c(2015,12) , frequency = 12 ))

#### Checking Seasonality again
temp<-data.frame("Period" = time$date, "Month"= substr(time$date,6,7),
                 "Year" = substr(time$date,1,4),"data" = time$deseasonalized1)
temp<-melt(temp[,-1])
plot_ly(temp, y =~value, color = ~Month, type = "box")

##### now the seasonal component is largely removed. However we can proceed with deseasonal data as well
#### instead of deseasonalizing it twice.

#### Removing trend but lets first check the stationarity
###There  is trend as well but a small trend

### Checking stationarity
modelling_data<-ts(time$deseasonalized1,start = c(1992,1), end =c(2015,12) , frequency = 12 )
summary(ur.df(modelling_data,lags = trunc((length(modelling_data) - 1)^(1/3)),selectlags = "AIC"))
adf.test(modelling_data)
### The Data is stationary

#### Forecast
####ACF PAcf
acf(modelling_data , type = c("correlation","covariance","partial"), plot = TRUE)
pacf(modelling_data)

### The data is atleast 2 or 3 AR component along with 1 or two MA component with zero mean
### We can confirm this using auto.arima function in forecast package
model_arima<-auto.arima(modelling_data)
summary(model_arima)
signifance_level<-(1-pnorm(abs(model_arima$coef)/sqrt(diag(model_arima$var.coef))))*2
signifance_level
#### Therefore we go with ARIMA(2,0,2) process

model_arima<-arima(modelling_data,order = c(2,0,2))
summary(model_arima)

###### Forecasting for next 2 years
forecasting_period<-24
forecasted_data<-forecast(model_arima, h=forecasting_period)

#### Adding Seasonality of data--1
forecasted_data$x<-forecasted_data$x+decomposed_data$seasonal
forecasted_data$mean<-forecasted_data$mean+decomposed_data$seasonal[1:forecasting_period]
forecasted_data$lower<-forecasted_data$lower+decomposed_data$seasonal[1:forecasting_period]
forecasted_data$upper<-forecasted_data$upper+decomposed_data$seasonal[1:forecasting_period]

#### Adding trend to data--- Adding last data of trend to all the forecasted data (can be changed)
forecasted_data$x<-forecasted_data$x+decomposed_data$trend
forecasted_data$mean<-forecasted_data$mean+decomposed_data$trend[nrow(decomposed_data)]
forecasted_data$lower<-forecasted_data$lower+decomposed_data$trend[nrow(decomposed_data)]
forecasted_data$upper<-forecasted_data$upper+decomposed_data$trend[nrow(decomposed_data)]

#### Adding Seasonality of data--2
forecasted_data$x<-forecasted_data$x+decomposed_data1$seasonal
forecasted_data$mean<-forecasted_data$mean+decomposed_data1$seasonal[1:forecasting_period]
forecasted_data$lower<-forecasted_data$lower+decomposed_data1$seasonal[1:forecasting_period]
forecasted_data$upper<-forecasted_data$upper+decomposed_data1$seasonal[1:forecasting_period]
hchart(forecasted_data)
