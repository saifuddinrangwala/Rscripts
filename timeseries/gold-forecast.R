# Set the working directory
setwd("~/Rscripts/timeseries")

library(haven)
require(labelled)
library(forecast)
library(ggplot2)

goldprice <- read_sav("data-files/GoldPrice.sav")
View(goldprice)

goldts = ts(goldprice$Indianrupee)
View(goldts)

plot.ts(goldts)

goldtsdiff1 = diff(goldts, differences=1)
plot.ts(goldtsdiff1)

acf(goldtsdiff1, lag.max = 20, na.action = na.omit)
pacf(goldtsdiff1, lag.max = 20, na.action = na.omit)

goldarima = arima(goldts, order=c(13,1,1))
goldarima

goldforecasts = forecast.Arima(goldarima, h=10)

acf(goldforecasts$residuals, lag.max=20, na.action = na.omit)
pacf(goldforecasts$residuals, lag.max=20, na.action = na.omit)

View(goldforecasts)
plot.forecast(goldforecasts)

Box.test(goldforecasts$residuals, lag=20, type="Ljung-Box")

plot.ts(goldforecasts$residuals)

# Automatic

fit <- auto.arima(goldts)
fit
acf(fit$residuals, lag.max=20)
pacf(fit$residuals, lag.max=20)
Box.test(fit$residuals)
plot.ts(fit$residuals)
goldforecasts = forecast.Arima(fit, h=10)
plot.forecast(goldforecasts)
View(goldforecasts)