library(forecast)

# Load cleaned data
stock_data <- read.csv("data/stock_data_clean.csv")
stock_data$Date <- as.Date(stock_data$Date)

# Use TCS for prediction
tcs_data <- subset(
  stock_data,
  Stock == "TCS"
)

# Create time series from adjusted prices
tcs_ts <- ts(
  tcs_data$Adjusted,
  frequency = 252
)

# Automatically choose ARIMA model
model <- auto.arima(tcs_ts)

# Forecast next 30 trading days
forecast_result <- forecast(
  model,
  h = 30
)

print(model)
print(forecast_result)

forecast_df <- data.frame(
  Period = 1:30,
  Point_Forecast = as.numeric(forecast_result$mean),
  Lo80 = as.numeric(forecast_result$lower[, 1]),
  Hi80 = as.numeric(forecast_result$upper[, 1]),
  Lo95 = as.numeric(forecast_result$lower[, 2]),
  Hi95 = as.numeric(forecast_result$upper[, 2])
)

write.csv(
  forecast_df,
  "data/tcs_forecast.csv",
  row.names = FALSE
)