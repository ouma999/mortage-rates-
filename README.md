# mortage-rates-
This projects uses prevoious data of mortage rates in the us and tries to creat a predictive model that can be used to predict future intrest rates.
From the project, i draw conclusion that explains the varying intrest rate taking into account, historical, economic or even natural occurences that make make the market behave in a certain manner.
install.packages("openxlsx")
library(openxlsx)

MORTGAGE30US <- read.xlsx("C:/Users/polex/Downloads/MORTGAGE30US.xlsx")
MORTGAGE30US
install.packages('forecast')
library(forecast)
library(zoo)
MORTGAGE30US$observation_date <- as.Date(MORTGAGE30US$observation_date)
#he data begins in the 4th period of the year.there are 52wks in a year.the ts does weekly interpretation.
mortgage_ts <- ts(MORTGAGE30US$MORTGAGE30US, start = c(1971, 4), frequency = 52)
head(mortgage_ts)

plot(mortgage_ts, main="Mortgage Rate Over Time", ylab="Mortgage Rate (%)", xlab="Year", col="blue")


# Load necessary library for smoothing ma
library(TTR)

# Calculate a Simple Moving Average (SMA) with a window size of 12
sma_12 <- SMA(mortgage_ts, n = 12)

# Plot the original data and the moving average
plot(mortgage_ts, type="l", col="blue", lwd=2, xlab="Year", ylab="Mortgage Rate")
lines(sma_12, col="red", lwd=2)  # Add the SMA line
legend("topright", legend=c("Original Data", "12-Point SMA"), col=c("blue", "red"), lwd=2)


# since the sieres is stationary we proceed to use ARIMA INSTEAD OF MA

install.packages('tseries')
library(tseries)
#check whether a time series is stationary
adf.test(mortgage_ts)#Since the p-value (0.4827) is greater than 0.05, you fail to reject the null hypothesis. Therefore, the time series is likely non-stationary
#since it is stationanry we adjust for seasonality by decomposing the model
#break down a time series into trend, seasonal, and residual components
decomposed <- decompose(mortgage_ts)
plot(decomposed)
#steps to remove the seasonal
seasonal_diff_ts <- diff(mortgage_ts, lag = 52)
plot(seasonal_diff_ts)

# Check if the seasonally differenced series is stationary
#After differencing,p value <0.05 hence fixed
adf.test(seasonal_diff_ts)
#After confirming stationarity, you need to choose an appropriate forecasting model
#Fit ARIMA model using auto.arima (which automatically identifies the best p, d, q values)
library(forecast)
arima_model <- auto.arima(mortgage_ts)
summary(arima_model)

#Model Diagnostics
#Once the model is fitted, you need to check if the residuals 
#(the differences between the forecasted and actual values) are random and normally distributed
# Check residuals
checkresiduals(arima_model)

# Forecasting
#Once you've fitted the model and verified its accuracy through diagnostics, you can use it to make predictions (forecasting).

# Forecast the next 12 periods (e.g., 12 weeks ahead)
forecast_values <- forecast(arima_model, h = 12)

# Plot the forecast
plot(forecast_values)

# Display forecast values
print(forecast_values)
library(knitr)

forecast_values_df <- data.frame(
  Time = index(forecast_values$mean), 
  Point_Forecast = round(forecast_values$mean, 4),
  Lo_80 = round(forecast_values$lower[,1], 4),
  Hi_80 = round(forecast_values$upper[,1], 4),
  Lo_95 = round(forecast_values$lower[,2], 4),
  Hi_95 = round(forecast_values$upper[,2], 4)
)
kable(forecast_values_df, caption = "Forecasted Values for the Next 12 Periods")

#Model Evaluation.evaluate the model by comparing the forecasts to the actual values in the the test set
## Train the model on the first part (excluding last 12 observations)
train_data <- mortgage_ts[1:(length(mortgage_ts) - 12)]
model <- auto.arima(train_data)

# Forecast the next 12 periods (last observations)
forecast_values <- forecast(model, h = 12)
# Compare with actual observed values (the last 12 observations in your data)
# low MAE, RMSE, or MAPE), the model can be considered ready for deployment.
actual_values <- mortgage_ts[(length(mortgage_ts) - 11):length(mortgage_ts)]
accuracy(forecast_values, actual_values)
