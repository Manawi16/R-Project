library(TTR)

# Load cleaned data
stock_data <- read.csv("data/stock_data_clean.csv")
stock_data$Date <- as.Date(stock_data$Date)

# Use TCS first for technical indicator analysis
tcs_data <- subset(
  stock_data,
  Stock == "TCS"
)

# 20-day Simple Moving Average
tcs_data$SMA20 <- SMA(
  tcs_data$Adjusted,
  n = 20
)

# 50-day Simple Moving Average
tcs_data$SMA50 <- SMA(
  tcs_data$Adjusted,
  n = 50
)

head(tcs_data, 55)

# 14-day Relative Strength Index
tcs_data$RSI14 <- RSI(
  tcs_data$Adjusted,
  n = 14
)

tail(
  tcs_data[, c("Date", "Adjusted", "SMA20", "SMA50", "RSI14")]
)

# MACD indicator
macd_values <- MACD(
  tcs_data$Adjusted,
  nFast = 12,
  nSlow = 26,
  nSig = 9,
  maType = "EMA"
)

tcs_data$MACD <- macd_values[, "macd"]
tcs_data$MACD_Signal <- macd_values[, "signal"]

tail(
  tcs_data[, c(
    "Date",
    "Adjusted",
    "RSI14",
    "MACD",
    "MACD_Signal"
  )]
)

tcs_data$Signal <- "HOLD"

tcs_data$Signal[
  tcs_data$SMA20 > tcs_data$SMA50 &
  tcs_data$RSI14 < 70 &
  tcs_data$MACD > tcs_data$MACD_Signal
] <- "BUY"

tcs_data$Signal[
  tcs_data$SMA20 < tcs_data$SMA50 &
  tcs_data$RSI14 > 30 &
  tcs_data$MACD < tcs_data$MACD_Signal
] <- "SELL"

tail(
  tcs_data[, c(
    "Date",
    "Adjusted",
    "SMA20",
    "SMA50",
    "RSI14",
    "MACD",
    "MACD_Signal",
    "Signal"
  )]
)
write.csv(
  tcs_data,
  "data/tcs_indicators.csv",
  row.names = FALSE
)