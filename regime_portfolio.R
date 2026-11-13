# regime_portfolio.R

# 1. Load cleaned stock data and the new regime classifications
stock_data <- read.csv("data/stock_data_clean.csv")
regimes <- read.csv("data/market_regimes.csv")

stock_data$Date <- as.Date(stock_data$Date)
regimes$Date <- as.Date(regimes$Date)

# Merge regime states with individual stock returns
portfolio_data <- merge(stock_data, regimes[, c("Date", "Regime")], by = "Date")

# 2. Function to calculate Risk Parity weights (Inverse Volatility method)
calc_risk_parity <- function(data) {
  # Calculate standard deviation (volatility) for each stock
  vols <- aggregate(Daily_Return ~ Stock, data = data, FUN = sd)
  
  # Calculate inverse volatility (higher volatility = lower inverse score)
  vols$Inv_Vol <- 1 / vols$Daily_Return
  
  # Normalize to get final allocation weights that sum to 100%
  vols$Weight <- vols$Inv_Vol / sum(vols$Inv_Vol)
  
  # Format output
  colnames(vols) <- c("Stock", "Volatility_StdDev", "Inv_Vol", "Target_Weight")
  
  # Add the naive equal weight for comparison
  vols$Naive_Weight <- 0.20 
  
  return(vols[, c("Stock", "Volatility_StdDev", "Naive_Weight", "Target_Weight")])
}

# 3. Calculate baseline Risk Parity (ignoring regimes entirely)
print("=====================================================")
print(" STATIC RISK PARITY (Assuming Market is Constant)")
print("=====================================================")
static_weights <- calc_risk_parity(portfolio_data)
print(static_weights)
cat("\n")

# 4. Calculate Regime-Aware Risk Parity
print("=====================================================")
print(" REGIME 1 ALLOCATION (State 1 Market Conditions)")
print("=====================================================")
regime_1_data <- subset(portfolio_data, Regime == 1)
r1_weights <- calc_risk_parity(regime_1_data)
print(r1_weights)
cat("\n")

print("=====================================================")
print(" REGIME 2 ALLOCATION (State 2 Market Conditions)")
print("=====================================================")
regime_2_data <- subset(portfolio_data, Regime == 2)
r2_weights <- calc_risk_parity(regime_2_data)
print(r2_weights)
cat("\n")
