# xai_model.R
library(xgboost)
library(SHAPforxgboost)
library(data.table)

# 1. Load the technical indicators
tcs_data <- read.csv("data/tcs_indicators.csv")

# 2. Engineer the Target Variable: Will the price close higher tomorrow? (1 = Yes, 0 = No)
tcs_data$Target <- c(ifelse(diff(tcs_data$Adjusted) > 0, 1, 0), NA)
tcs_data <- na.omit(tcs_data)

# 3. Select the features
features <- c("Daily_Return", "SMA20", "SMA50", "RSI14", "MACD", "MACD_Signal")
X <- as.matrix(tcs_data[, features])
y <- tcs_data$Target 

# 4. Convert data to XGBoost's highly optimized DMatrix format
dtrain <- xgb.DMatrix(data = X, label = y)

# 5. Train using the advanced xgb.train() engine
set.seed(42)
xgb_model <- xgb.train(
  data = dtrain, 
  nrounds = 50, 
  objective = "reg:logistic",
  eval_metric = "auc"
)

# 6. Extract SHAP values
shap_values <- shap.values(xgb_model, X_train = X)
shap_data <- shap_values$shap_score

# 7. Save for the dashboard
saveRDS(shap_data, "data/shap_data.rds")
saveRDS(X, "data/xgb_features.rds")

print("XGBoost Model trained and SHAP explanations generated successfully.")