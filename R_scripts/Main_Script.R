# Library for data Processing
install.packages("tidyverse")
library(tidyverse)
install.packages("conflicted")
library(conflicted)
conflict_scout()
install.packages("ggfortify")
library(ggfortify) 
install.packages("ggplot2")
library(ggplot2)

# Library to forecast
install.packages("forecast", dependencies = TRUE)
library(forecast)
install.packages("tseries")
library(tseries)
install.packages("vctrs")
library(vctrs)
install.packages("modeltime")
library(modeltime)
install.packages("tidymodels")
library(tidymodels)
install.packages("timetk")
library(timetk)
install.packages("fable")
library(fable)
install.packages("tsibble")
library(tsibble)

# Library for Processing date data type
install.packages("lubridate")
library(lubridate)

# Library Simple Moving Average
install.packages("TTR")
library(TTR)

# Library for Processing date data type
install.packages("zoo")
library(zoo)

# Data visualisation
library(plotly)
library(xts)
library(TSstudio)
library(tseries)
library(lubridate)
install.packages("MLmetrics")
library(MLmetrics)
install.packages("hrbrthemes")
install.packages("gdtools")
library(hrbrthemes)
options(xts.warn_dplyr_breaks_lag = FALSE)

# Loading the data
df <- read.csv('/Users/pc3/Desktop/Data_Projects_Repository/Retail_Forecasting/R_Repository/raw_data/mock_kaggle.csv')

# Preparing the data
shoe_sales <- df %>% select(c(data, venda))
shoe_sales$date <- ymd(shoe_sales$data)
colnames(shoe_sales) <- c('date','sales')
sales <- shoe_sales %>% select(c(sales))
head(shoe_sales)
summary(shoe_sales)

shoe_sales$date <- ymd(shoe_sales$date)
monthly_average <- shoe_sales %>%
  mutate(year_month = floor_date(date, unit = "month")) %>%
  group_by(year_month) %>%
  summarise(avg_sales = mean(sales))

# Transforming the data to time series
time_series <- ts(monthly_average$avg_sales, start = c(2014, 1), end=c(2016, 7), frequency = 12)
summary(time_series)
autoplot(time_series) +
  ylab("AVG Count Of Sales") +
  ggtitle("Plot: Shoe Sales Time Series") +
  geom_line(color="#69b3a2") +
  geom_hline(yintercept = 90.46, color="orange") +
  theme_minimal()

# Visualizing the seasonality
ggseasonplot(time_series) +
  ylab("AVG Count Of Sales") +
  ggtitle("Plot: Shoe Sales Seasonality") +
  theme_minimal()

# Visualizing the trend
ggsubseriesplot(time_series) +
  ylab("Trend Line") +
  ggtitle("Plot: Shoe Sales Trend") +
  geom_line(color="#69b3a2") +
  theme_minimal()
print(time_series)

# Decomposing & visualizing the time series
stl_time_series <- stl(time_series, s.window = "periodic")
autoplot(stl_time_series) +
  ggtitle("Plot: (Decomposed) Shoe Sales") +
  geom_line(color="#69b3a2") +
  theme_minimal()

seasonal <- stl_time_series$time.series[, "seasonal"]
plot(seasonal)
trend <- stl_time_series$time.series[, "trend"]
plot(trend)
remainder <- stl_time_series$time.series[, "remainder"]
plot(remainder)

# "The ADF test is based on the null hypothesis that the data series -
# has a unit root, indicating non-stationarity."
adf.test(time_series) # p-value = .7739 (Means the Null hypothesis is TRUE)

# Making the time series stationary
diff_time_series <- diff(time_series)
summary(diff_time_series)
autoplot(diff_time_series) +
  ylab("AVG Count Of Sales") +
  ggtitle("Plot: (Differenced) Shoe Sales ") +
  geom_line(color="#69b3a2") +
  geom_hline(yintercept = 1.114, color="orange") +
  theme_minimal()

print(diff_time_series)
adf.test(diff_time_series) # p-value = 0.024 (Null hypothesis = False = stationary series)

######################################################################################
# My series has some seasonality, and an upward linear trend.
# To remove the trend, I removed first difference to make ts stationary for fitting.
# Will forecast with ARIMA & SMA.
######################################################################################

# Creating function to find MAPE for test & forecast data (also using MAE/RMSE)
MAPE <- function(y_pred, y_true) {
  if (length(y_pred) != length(y_true)) {
    stop("Length of y_pred and y_true should be equal.")
  }
  if (any(is.na(y_pred)) || any(is.na(y_true))) {
    stop("Missing values found in y_pred or y_true.")
  }
  mape_value <- mean(abs((y_true - y_pred) / y_true), na.rm = TRUE) * 100
  return(mape_value)
}

# Cross validation (80% of the data for training & 20% for testing)
train_data <- head(diff_time_series, length(diff_time_series) - 6)
test_data <- tail(diff_time_series, 6)
print(test_data)
train_data_ts <- ts(train_data, start=c(2014, 2), end=c(2016, 1), frequency=12)
test_data_ts <- ts(test_data, start=c(2016, 2), end=c(2016, 7), frequency=12)

autoplot(train_data)
autoplot(train_data_ts) +
  ylab("AVG Count Of Sales") +
  ggtitle("Plot: (Training Set) Shoe Sales ") +
  geom_line(color="#69b3a2") +
  theme_minimal()
autoplot(test_data_ts) +
  ylab("AVG Count Of Sales") +
  ggtitle("Plot: (Test Set) Shoe Sales ") +
  geom_line(color="#69b3a2") +
  theme_minimal()
print(test_data_ts)
autoplot(diff_time_series)

# Fitting the SMA model
fit_sma <- SMA(train_data_ts, n = 3)
tail(round(sales_sma,2),10)
autoplot(fit_sma)

# Forecasting with SMA model & evaluating with MAPE/MAE/RMSE
sales_sma_forecast <- forecast(fit_sma, h = 6)
autoplot(sales_sma_forecast) +
  ylab("AVG Count Of Sales") +
  ggtitle("Plot: Shoe Sales SMA Forecast") +
  theme_minimal()
accuracy(sales_sma_forecast)
autoplot(diff_time_series) +
  autolayer(sales_arima_forecast, series = "ARIMA Forecast") +
  xlab("Year") +
  ggtitle("Actual vs. Forecast") +
  labs(color = "Forecast Type") +
  scale_color_manual(values = c("red"))

print(summary(sales_sma_forecast$mean)) # Mean = 0.40
test_data$sma <- rep(0.396, 6)
sma_mape <- MAPE(test_data$sma, unlist(test_data)[1:6])
sma_mape
sma_mae <- mean(abs(test_data$sma - unlist(test_data)[1:6]))
sma_mae
sma_rmse <- sqrt(mean(test_data$sma - unlist(test_data)[1:6])^2)
sma_rmse
print(test_data)

# Fitting the ARIMA model
fit_arima <- auto.arima(train_data_ts, approximation = FALSE, trace = TRUE) # Residual SD = 30.36
print(summary(fit_arima))
length(fit_arima)
checkresiduals(fit_arima)
autoplot(fit_arima)
sqrt(921.9)

# Forecasting with ARIMA model & evaluating with MAPE/MAE/RMSE
sales_arima_forecast <- forecast(fit_arima, h = 6)
accuracy(sales_arima_forecast)
autoplot(sales_arima_forecast) +
  ylab("AVG Count Of Sales") +
  ggtitle("Plot: Shoe Sales ARIMA Forecast") +
  theme_minimal()
autoplot(diff_time_series) +
  autolayer(sales_arima_forecast, series = "ARIMA Forecast") +
  xlab("Year") +
  ggtitle("Actual vs. Forecast") +
  labs(color = "Forecast Type") +
  scale_color_manual(values = c("green"))

print(summary(sales_arima_forecast$mean)) # Mean = 2.46
test_data$arima <- rep(2.46, 6)
arima_mape <- MAPE(test_data$arima, unlist(test_data)[1:6])
arima_mape
arima_mae <- mean(abs(test_data$arima - unlist(test_data)[1:6]))
arima_mae
arima_rmse <- sqrt(mean(test_data$arima - unlist(test_data)[1:6])^2)
arima_rmse

# Plotting actual vs. forecast (ARIMA)
autoplot(diff_time_series) +
  autolayer(sales_arima_forecast, series = "ARIMA Forecast") +
  xlab("Year") +
  ggtitle("Actual vs. Forecast") +
  labs(color = "Forecast Type") +
  scale_color_manual(values = c("green"))

autoplot(diff_time_series)
autoplot(train_data_ts)
autoplot(sales_arima_forecast)

# Plotting actual vs. forecast (SMA)
autoplot(diff_time_series) +
  autolayer(sales_sma_forecast, series = "SMA Forecast") +
  xlab("Year") +
  ggtitle("Actual vs. Forecast") +
  labs(color = "Forecast Type") +
  scale_color_manual(values = c("red"))

autoplot(train_data_ts)
autoplot(diff_time_series)
autoplot(sales_sma_forecast)

# Plotting actual vs. forecast (ARIMA + SMA Forecast)
autoplot(diff_time_series) +
  autolayer(fit_arima, series = "ARIMA Forecast") +
  autolayer(fit_sma, series = "SMA Forecast") +
  xlab("Year") +
  ggtitle("Actual vs. Forecast") +
  labs(color = "Forecast Type") +
  scale_color_manual(values = c("green", "red"))

############################################################################
# evaluating the models using...
# Mean Absolute Point Error, Mean Absolute Error, Root Mean Squared Error

install.packages("formattable")
install.packages("DT")
library(formattable)
library(data.table)
library(DT)

Model <- c('SMA','ARIMA')
MAPE <- c(round(sma_mape, 2), round(arima_mape, 2))
MAE <- c(round(sma_mae, 2), round(arima_mae, 2))
RMSE <- c(round(sma_rmse, 2), round(arima_rmse, 2))
df <- data.frame(Model, MAPE, MAE, RMSE)
datatable(df)
