library(quantmod)

stocks <- c(
  "TCS.NS",
  "RELIANCE.NS",
  "INFY.NS",
  "HDFCBANK.NS",
  "ICICIBANK.NS"
)

start_date <- as.Date("2021-01-01")
end_date <- Sys.Date()

getSymbols(
  stocks,
  src = "yahoo",
  from = start_date,
  to = end_date
)

stock_list <- list(
  TCS = TCS.NS,
  RELIANCE = RELIANCE.NS,
  INFOSYS = INFY.NS,
  HDFC_BANK = HDFCBANK.NS,
  ICICI_BANK = ICICIBANK.NS
)

combined_data <- do.call(
  rbind,
  lapply(names(stock_list), function(stock_name) {

    df <- data.frame(
      Date = index(stock_list[[stock_name]]),
      coredata(stock_list[[stock_name]])
    )

    colnames(df) <- c(
      "Date",
      "Open",
      "High",
      "Low",
      "Close",
      "Volume",
      "Adjusted"
    )

    df$Stock <- stock_name

    return(df)
  })
)

row.names(combined_data) <- NULL

combined_data <- combined_data[order(combined_data$Stock, combined_data$Date), ]

combined_data$Daily_Return <- ave(
  combined_data$Adjusted,
  combined_data$Stock,
  FUN = function(x) c(NA, diff(x) / head(x, -1) * 100)
)

clean_data <- na.omit(combined_data)

write.csv(
  clean_data,
  "data/stock_data_clean.csv",
  row.names = FALSE
)