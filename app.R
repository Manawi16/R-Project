library(shiny)
library(ggplot2)

# -----------------------------
# Load datasets
# -----------------------------

stock_data <- read.csv("data/stock_data_clean.csv")
stock_data$Date <- as.Date(stock_data$Date)

tcs_indicators <- read.csv("data/tcs_indicators.csv")
tcs_indicators$Date <- as.Date(tcs_indicators$Date)

forecast_data <- read.csv("data/tcs_forecast.csv")


# -----------------------------
# Risk-return calculations
# -----------------------------

avg_return <- aggregate(
  Daily_Return ~ Stock,
  data = stock_data,
  FUN = mean
)

volatility <- aggregate(
  Daily_Return ~ Stock,
  data = stock_data,
  FUN = sd
)

colnames(avg_return)[2] <- "Average_Return"
colnames(volatility)[2] <- "Volatility"

risk_summary <- merge(
  avg_return,
  volatility,
  by = "Stock"
)

risk_summary$Sharpe_Ratio <- (
  risk_summary$Average_Return /
    risk_summary$Volatility
) * sqrt(252)


# -----------------------------
# UI
# -----------------------------

ui <- fluidPage(

  titlePanel(
    "Stock Market Analysis & Investment Decision Support System"
  ),

  sidebarLayout(

    sidebarPanel(

      selectInput(
        "stock",
        "Select Stock:",
        choices = unique(stock_data$Stock)
      )

    ),

    mainPanel(

      h3("Historical Stock Price"),

      plotOutput("pricePlot"),

      h3("Daily Return"),

      plotOutput("returnPlot"),

      h3("Risk-Return Summary"),

      tableOutput("riskTable")

    )

  )

)


# -----------------------------
# Server
# -----------------------------

server <- function(input, output) {

  selected_stock <- reactive({

    subset(
      stock_data,
      Stock == input$stock
    )

  })


  # Historical price chart
  output$pricePlot <- renderPlot({

    ggplot(
      selected_stock(),
      aes(x = Date, y = Adjusted)
    ) +
      geom_line() +
      labs(
        title = paste(input$stock, "Historical Price"),
        x = "Date",
        y = "Adjusted Price"
      )

  })


  # Daily return chart
  output$returnPlot <- renderPlot({

    ggplot(
      selected_stock(),
      aes(x = Date, y = Daily_Return)
    ) +
      geom_line() +
      labs(
        title = paste(input$stock, "Daily Return"),
        x = "Date",
        y = "Daily Return (%)"
      )

  })


  # Risk-return table
  output$riskTable <- renderTable({

    risk_summary

  })

}


# -----------------------------
# Run application
# -----------------------------

shinyApp(
  ui = ui,
  server = server
)