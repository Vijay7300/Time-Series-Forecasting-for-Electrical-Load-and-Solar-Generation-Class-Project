install.packages("Metrics")
install.packages("readr")
install.packages("dplyr")


library(dplyr)
library(lubridate)
library(forecast)
library(fpp2)
library(ggplot2)
library(Metrics)
library(readr)
library(tseries)
library(zoo)
library(tsibble)


#Read dataset
data <- read.csv("C:/Users/asus/AppData/Local/Packages/5319275A.WhatsAppDesktop_cv1g1gvanyjgm/LocalState/sessions/502D450DBE773A981A3F02269C25B47A2F2DE1CA/transfers/2025-47/TimeSeries_TotalSolarGen_and_Load_IT_2016 - TimeSeries_TotalSolarGen_and_Load_IT_2016.csv")

data


# Convert timestamp

data$utc_timestamp <- ymd_hms(data$utc_timestamp)


# Aggregate hourly → daily totals

daily_data <- data %>%
  mutate(date = as.Date(utc_timestamp)) %>%
  group_by(date) %>%
  summarise(
    Daily_Load  = sum(IT_load_new, na.rm = TRUE),
    Daily_Solar = sum(IT_solar_generation, na.rm = TRUE)
  )
daily_data
plot.ts(daily_data)

print(daily_data,n=356)


# Create TS objects (weekly seasonality)

load_ts  <- ts(daily_data$Daily_Load,  frequency = 7)
solar_ts <- ts(daily_data$Daily_Solar, frequency = 7)



# Check for seasonality 
plot(load_ts)
plot(solar_ts)
ggseasonplot(load_ts)
ggseasonplot(diff(load_ts,lag=7))
ggseasonplot(solar_ts)

# check for stationarity

kpss.test(solar_ts)
kpss.test(diff(load_ts,lag=7))


acf(load_ts)
pacf(load_ts)
acf(solar_ts)
pacf(solar_ts)


##STL Decomposition


load_stl <- stl(load_ts, s.window = "periodic")
autoplot(load_stl) + ggtitle("STL Decomposition — Load")

solar_stl <- stl(solar_ts, s.window = "periodic")
autoplot(solar_stl) + ggtitle("STL Decomposition — Solar")


# Correct Train–Test Split

n <- length(load_ts)
train_size <- round(0.8 * n)

# CORRECTED Train–Test Split


### LOAD
load_train <- window(load_ts, end = time(load_ts)[train_size])

load_test <- window(load_ts, start = time(load_ts)[train_size + 1])

### SOLAR
solar_train <- window(solar_ts, end = time(solar_ts)[train_size])

solar_test <- window(solar_ts, start = time(solar_ts)[train_size + 1])

# Benchmark Models (Mean, Naive, etc.)


# Load
load_mean   <- meanf(load_train,  h = length(load_test))
load_naive  <- rwf(load_train,    h = length(load_test))
load_snaive <- snaive(load_train, h = length(load_test))
load_drift  <- rwf(load_train,    h = length(load_test), drift = TRUE) 

# Solar
solar_mean   <- meanf(solar_train,  h = length(solar_test))
solar_naive  <- rwf(solar_train,    h = length(solar_test))
solar_snaive <- snaive(solar_train, h = length(solar_test))
solar_drift  <- rwf(solar_train,    h = length(solar_test), drift = TRUE)


# ARIMA Models

load_arima  <- auto.arima(load_train,  seasonal = TRUE)
load_arima

fit1 <-Arima(load_train,order = c(1,1,3),seasonal = c(1,1,3))
fit2<-Arima(load_train,order = c(1,1,1),seasonal = c(1,1,0))
fit3<- Arima(load_train,order = c(2,1,1),seasonal = c(0,1,1))
c(fit1$aicc,fit2$aicc,fit3$aicc)
load_arima_fc  <- forecast(load_arima,  h = length(load_test))

##  RESIDUAL DIAGNOSTICS — LOAD (ARIMA)########

cat("\n================ LOAD ARIMA Residuals ================\n")

# Extract residuals
res_load_arima <- residuals(load_arima)

# Time plot
ts.plot(res_load_arima, main="Load ARIMA Residuals", ylab="Residuals")
abline(h=0, col="red")

# ACF
Acf(res_load_arima, main="ACF — Load ARIMA Residuals")

# Histogram
hist(res_load_arima, freq=FALSE, main="Histogram — Load ARIMA Residuals")
lines(density(res_load_arima), col="blue", lwd=2)

# Q-Q Plot
qqnorm(res_load_arima, main="Q-Q Plot — Load ARIMA Residuals")
qqline(res_load_arima, col="red")

# Ljung–Box
Box.test(res_load_arima, lag=20, type="Ljung")

# FPP2 automatic residual check
checkresiduals(load_arima)


solar_arima <- auto.arima(solar_train, seasonal = TRUE)
solar_arima_fc <- forecast(solar_arima, h = length(solar_test))


##  RESIDUAL DIAGNOSTICS — SOLAR (ARIMA)#######
cat("\n================ SOLAR ARIMA Residuals ================\n")

res_solar_arima <- residuals(solar_arima)

ts.plot(res_solar_arima, main="Solar ARIMA Residuals", ylab="Residuals")
abline(h=0, col="red")

Acf(res_solar_arima, main="ACF — Solar ARIMA Residuals")

hist(res_solar_arima, freq=FALSE, main="Histogram — Solar ARIMA Residuals")
lines(density(res_solar_arima), col="blue", lwd=2)

qqnorm(res_solar_arima, main="Q-Q Plot — Solar ARIMA Residuals")
qqline(res_solar_arima, col="red")

Box.test(res_solar_arima, lag=20, type="Ljung")

checkresiduals(solar_arima)





##  bis. ETS MODELS


# Load ETS
load_ets <- ets(load_train)
load_ets_fc <- forecast(load_ets, h = length(load_test))
#### RESIDUAL DIAGNOSTICS — LOAD (ETS)
cat("\n================ LOAD ETS Residuals ================\n")

res_load_ets <- residuals(load_ets)

ts.plot(res_load_ets, main="Load ETS Residuals", ylab="Residuals")
abline(h=0, col="red")

Acf(res_load_ets, main="ACF — Load ETS Residuals")

hist(res_load_ets, freq=FALSE, main="Histogram — Load ETS Residuals")
lines(density(res_load_ets), col="blue", lwd=2)

qqnorm(res_load_ets, main="Q-Q Plot — Load ETS Residuals")
qqline(res_load_ets, col="red")

Box.test(res_load_ets, lag=20, type="Ljung")

checkresiduals(load_ets)

load_ets


# Solar ETS
solar_ets <- ets(solar_train)
summary(solar_ets)
solar_ets1<-ets(solar_test,model = solar_ets)
summary(solar_ets1)

solar_ets_fc <- forecast(solar_ets, h = length(solar_test))
solar_ets_fc1<-forecast(solar_ets1,h=length(solar_test))


##### RESIDUAL DIAGNOSTICS — SOLAR (ETS)
cat("\n================ SOLAR ETS Residuals ================\n")

res_solar_ets <- residuals(solar_ets)

ts.plot(res_solar_ets, main="Solar ETS Residuals", ylab="Residuals")
abline(h=0, col="red")

Acf(res_solar_ets, main="ACF — Solar ETS Residuals")

hist(res_solar_ets, freq=FALSE, main="Histogram — Solar ETS Residuals")
lines(density(res_solar_ets), col="blue", lwd=2)

qqnorm(res_solar_ets, main="Q-Q Plot — Solar ETS Residuals")
qqline(res_solar_ets, col="red")

Box.test(res_solar_ets, lag=20, type="Ljung")
Box.test(solar_ets1$residuals,lag = 20,type = "Ljung")

checkresiduals(solar_ets)
checkresiduals(solar_ets1)
solar_ets


# ter. SARIMA MODELS


# LOAD – SARIMA
load_sarima <- auto.arima(load_train, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
load_sarima_fc <- forecast(load_sarima, h = length(load_test))
load_sarima

### Residual Diagnostics for SARIMA####
res_load_sarima <- residuals(load_sarima)

ts.plot(res_load_sarima, main="Load SARIMA Residuals", ylab="Residuals")
abline(h=0, col="red")

Acf(res_load_sarima, main="ACF — Load SARIMA Residuals")

hist(res_load_sarima, freq=FALSE, main="Histogram — Load SARIMA Residuals")
lines(density(res_load_sarima), col="blue", lwd=2)

qqnorm(res_load_sarima, main="Q-Q Plot — Load SARIMA Residuals")
qqline(res_load_sarima, col="red")

Box.test(res_load_sarima, lag=20, type="Ljung")

checkresiduals(load_sarima)



# SOLAR – SARIMA
solar_sarima <- auto.arima(solar_train, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
solar_sarima_fc <- forecast(solar_sarima, h = length(solar_test))
solar_sarima

###   Solar SARIMA Residuals#######
res_solar_sarima <- residuals(solar_sarima)

ts.plot(res_solar_sarima, main="Solar SARIMA Residuals", ylab="Residuals")
abline(h=0, col="red")

Acf(res_solar_sarima, main="ACF — Solar SARIMA Residuals")

hist(res_solar_sarima, freq=FALSE, main="Histogram — Solar SARIMA Residuals")
lines(density(res_solar_sarima), col="blue", lwd=2)

qqnorm(res_solar_sarima, main="Q-Q Plot — Solar SARIMA Residuals")
qqline(res_solar_sarima, col="red")

Box.test(res_solar_sarima, lag=20, type="Ljung")

checkresiduals(solar_sarima)




#  ACCURACY (NO ERRORS NOW)

#  ACCURACY (CORRECTED)

#  ACCURACY (USING FORECAST PACKAGE)


cat("\n=========== LOAD ACCURACY ===========\n")

acc_load_mean   <- forecast::accuracy(load_mean,   load_test)
acc_load_naive  <- forecast::accuracy(load_naive,  load_test)
acc_load_snaive <- forecast::accuracy(load_snaive, load_test)
acc_load_drift  <- forecast::accuracy(load_drift,  load_test)
acc_load_arima  <- forecast::accuracy(load_arima_fc, load_test)
acc_load_ets    <- forecast::accuracy(load_ets_fc, load_test)
acc_load_sarima  <- forecast::accuracy(load_sarima_fc, load_test)

print(acc_load_mean)
print(acc_load_naive)
print(acc_load_snaive)
print(acc_load_drift)
print(acc_load_arima)
print(acc_load_ets)
print(acc_load_sarima)


cat("\n=========== SOLAR ACCURACY ===========\n")

acc_solar_mean   <- forecast::accuracy(solar_mean,   solar_test)
acc_solar_naive  <- forecast::accuracy(solar_naive,  solar_test)
acc_solar_snaive <- forecast::accuracy(solar_snaive, solar_test)
acc_solar_drift  <- forecast::accuracy(solar_drift,  solar_test)
acc_solar_arima  <- forecast::accuracy(solar_arima_fc, solar_test)
acc_solar_ets    <- forecast::accuracy(solar_ets_fc, solar_test)
acc_solar_sarima <- forecast::accuracy(solar_sarima_fc, solar_test)

print(acc_solar_mean)
print(acc_solar_naive)
print(acc_solar_snaive)
print(acc_solar_drift)
print(acc_solar_arima)
print(acc_solar_ets)
print(acc_solar_sarima)



## Compare All Models — LOAD

load_results <- rbind(
  Mean     = acc_load_mean,
  Naive    = acc_load_naive,
  Drift    = acc_load_drift,
  SNaive   = acc_load_snaive,
  ETS      = acc_load_ets,
  ARIMA    = acc_load_arima,
  SARIMA   = acc_load_sarima
)

print(load_results)


## Compare All Models — SOLAR

solar_results <- rbind(
  Mean     = acc_solar_mean,
  Naive    = acc_solar_naive,
  Drift    = acc_solar_drift,
  SNaive   = acc_solar_snaive,
  ETS      = acc_solar_ets,
  ARIMA    = acc_solar_arima,
  SARIMA   = acc_solar_sarima
)

print(solar_results)



###  Plot Load Forecasts (FPP2 Style)


window(load_ts, start = start(load_ts)) %>% autoplot() +
  autolayer(load_mean,   series="Mean",           PI = FALSE) +
  autolayer(load_naive,  series="Naive",          PI = FALSE) +
  autolayer(load_snaive, series="Seasonal Naive", PI = FALSE) +
  autolayer(load_ets_fc,    series="ETS",            PI=FALSE)+
  autolayer(load_sarima_fc, series="SARIMA", PI = FALSE)+
  autolayer(load_drift,  series="Drift",          PI = FALSE) +
  autolayer(load_arima_fc, series="ARIMA",        PI = FALSE) +
  autolayer(load_test, series = "Test Data") +
  ggtitle("Load Forecast — Benchmark Models vs ARIMA") +
  ylab("Load") + xlab("Day") + theme_minimal()


## Plot Solar Forecasts (FPP2 Style)


window(solar_ts, start = start(solar_ts)) %>% autoplot() +
  autolayer(solar_mean,   series="Mean",           PI = FALSE) +
  autolayer(solar_naive,  series="Naive",          PI = FALSE) +
  autolayer(solar_ets_fc,    series="ETS",            PI=FALSE)+
  autolayer(solar_snaive, series="Seasonal Naive", PI = FALSE) +
  autolayer(solar_drift,  series="Drift",          PI = FALSE) +
  autolayer(solar_arima_fc, series="ARIMA",        PI = FALSE) +
  autolayer(solar_sarima_fc, series="SARIMA", PI = FALSE)+
  autolayer(solar_test, series = "Test Data") +
  ggtitle("Solar Forecast — Benchmark Models vs ARIMA") +
  ylab("Solar Generation") + xlab("Day") + theme_minimal()



