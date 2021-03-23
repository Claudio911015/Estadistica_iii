library(openxlsx)
library(zoom)
library(forecast)

da <- read.csv("~/BTC-USD.csv")

dac <- da # for BU

dac.ts <- na.omit(as.numeric(ts(na.omit(dac[,5]),start=c(2016,3,22), freq=360)))

plot(dac.ts)

acf(dac.ts)
pacf(dac.ts)

##Vamos a transformar la serie de tiempo: Y_t = (1-B) X_t = X_t - X_{t-1}

pacf(diff(diff(dac.ts)))

data <- data.frame(closing=dac.ts, lclosing=log(dac.ts))
data.stl = stl(data$closing, s.window = "periodic")
plot(data.stl)

fit<-auto.arima(data$closing)
plot(forecast(fit, h=50, level=c(99.5)))
summary(fit)

pricetimeseriesarima<-arima(data$closing, order=c(2,2,1))
plot(forecast(pricetimeseriesarima, h=50, level=c(99.5)))
summary(pricetimeseriesarima)
checkresiduals(fit)

data.forecast = forecast(data.stl, method="arima", h=24, level=95)

plot(data.forecast, ylab="Bitcoin Price", xlab="Years")

head(data.forecast)















