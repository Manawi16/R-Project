# regime_model.R
library(depmixS4)

# 1. Load cleaned data
stock_data <- read.csv("data/stock_data_clean.csv")
stock_data$Date <- as.Date(stock_data$Date)

# 2. Create a market proxy by averaging daily returns of all 5 stocks
market_returns <- aggregate(Daily_Return ~ Date, data = stock_data, FUN = mean)
market_returns <- na.omit(market_returns)

# 3. Fit a 2-State Hidden Markov Model (HMM)
# We hypothesize 2 regimes based on the Gaussian distribution of returns
set.seed(123) 
hmm_model <- depmix(Daily_Return ~ 1, family = gaussian(), nstates = 2, data = market_returns)
hmm_fit <- fit(hmm_model, verbose = FALSE)

# 4. Extract the inferred hidden states (posterior probabilities)
hmm_states <- posterior(hmm_fit)
market_returns$Regime <- hmm_states$state

# 5. Analyze the statistical characteristics of each regime
mean_ret <- aggregate(Daily_Return ~ Regime, data = market_returns, FUN = mean)
vol_ret <- aggregate(Daily_Return ~ Regime, data = market_returns, FUN = sd)

regime_summary <- merge(mean_ret, vol_ret, by = "Regime")
colnames(regime_summary) <- c("Regime", "Average_Daily_Return", "Volatility_StdDev")

print("--- Hidden Market Regimes Identified ---")
print(regime_summary)

# 6. Save the regime classifications for the portfolio optimizer
write.csv(market_returns, "data/market_regimes.csv", row.names = FALSE)
print("Regime data saved successfully to data/market_regimes.csv")