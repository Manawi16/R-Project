# Load cleaned data
stock_data <- read.csv("data/stock_data_clean.csv")
stock_data$Date <- as.Date(stock_data$Date)

# Convert daily returns into wide format
return_wide <- reshape(
  stock_data[, c("Date", "Stock", "Daily_Return")],
  idvar = "Date",
  timevar = "Stock",
  direction = "wide"
)

return_wide <- na.omit(return_wide)

# Remove Date column
returns <- return_wide[, -1]

# Equal weight portfolio
num_stocks <- ncol(returns)

weights <- rep(
  1 / num_stocks,
  num_stocks
)

# Mean returns
mean_returns <- colMeans(returns)

# Covariance matrix
cov_matrix <- cov(returns)

# Portfolio expected daily return
portfolio_return <- sum(
  weights * mean_returns
)

# Portfolio daily risk
portfolio_risk <- sqrt(
  t(weights) %*%
    cov_matrix %*%
    weights
)

portfolio_return
portfolio_risk

# Annualized portfolio metrics
annual_return <- portfolio_return * 252

annual_risk <- as.numeric(portfolio_risk) * sqrt(252)

portfolio_sharpe <- annual_return / annual_risk

annual_return
annual_risk
portfolio_sharpe