# Load cleaned stock data
stock_data <- read.csv("data/stock_data_clean.csv")

# Convert Date column back to Date type
stock_data$Date <- as.Date(stock_data$Date)

# Basic summary
summary(stock_data)

# Number of observations
nrow(stock_data)

# Stocks included
unique(stock_data$Stock)

# Average daily return by stock
avg_return <- aggregate(
  Daily_Return ~ Stock,
  data = stock_data,
  FUN = mean
)

# Daily volatility by stock
volatility <- aggregate(
  Daily_Return ~ Stock,
  data = stock_data,
  FUN = sd
)

colnames(avg_return)[2] <- "Average_Return"
colnames(volatility)[2] <- "Volatility"

risk_return <- merge(avg_return, volatility, by = "Stock")

print(risk_return)
# Annualized Sharpe Ratio
risk_return$Sharpe_Ratio <- (
  risk_return$Average_Return /
  risk_return$Volatility
) * sqrt(252)

print(risk_return)

# Maximum Drawdown by stock
max_drawdown <- aggregate(
  Adjusted ~ Stock,
  data = stock_data,
  FUN = function(x) {
    running_max <- cummax(x)
    drawdown <- (x - running_max) / running_max
    min(drawdown) * 100
  }
)

colnames(max_drawdown)[2] <- "Max_Drawdown"

risk_return <- merge(
  risk_return,
  max_drawdown,
  by = "Stock"
)

print(risk_return)

# Rank stocks based on risk-adjusted performance
risk_return$Rank <- rank(
  -risk_return$Sharpe_Ratio,
  ties.method = "first"
)

risk_return <- risk_return[
  order(risk_return$Rank),
]

print(risk_return)

# Create return table for correlation analysis
return_wide <- reshape(
  stock_data[, c("Date", "Stock", "Daily_Return")],
  idvar = "Date",
  timevar = "Stock",
  direction = "wide"
)

# Remove incomplete rows
return_wide <- na.omit(return_wide)

# Correlation matrix
correlation_matrix <- cor(
  return_wide[, -1]
)

print(correlation_matrix)