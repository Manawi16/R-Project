# app.R
library(shiny)
library(ggplot2)
library(plotly)
library(forecast)
library(reshape2)
library(bslib)
library(DT)

# -----------------------------
# 1. Load & Merge Datasets
# -----------------------------
stock_data <- read.csv("data/stock_data_clean.csv")
stock_data$Date <- as.Date(stock_data$Date)

regimes <- read.csv("data/market_regimes.csv")
regimes$Date <- as.Date(regimes$Date)

portfolio_data <- merge(stock_data, regimes[, c("Date", "Regime")], by = "Date")
portfolio_data$Regime <- as.factor(portfolio_data$Regime)

market_vol <- aggregate(Daily_Return ~ Regime, data = regimes, FUN = sd)
low_vol_regime <- as.character(market_vol$Regime[which.min(market_vol$Daily_Return)])
high_vol_regime <- as.character(market_vol$Regime[which.max(market_vol$Daily_Return)])

# -----------------------------
# 2. Helper Functions
# -----------------------------
calc_risk_parity <- function(data) {
  vols <- aggregate(Daily_Return ~ Stock, data = data, FUN = sd)
  vols$Inv_Vol <- 1 / vols$Daily_Return
  vols$Optimal_Weight <- vols$Inv_Vol / sum(vols$Inv_Vol)
  vols$Naive_Weight <- 0.20 
  return(vols)
}

calc_max_drawdown <- function(x) {
  running_max <- cummax(x)
  drawdown <- (x - running_max) / running_max
  return(min(drawdown) * 100)
}

# -----------------------------
# 3. UI
# -----------------------------
ui <- fluidPage(
  
  # Apply dark mode theme with techy accents
  theme = bs_theme(
    bg = "#121212", 
    fg = "#E0E0E0", 
    primary = "#00E676", 
    base_font = font_google("Inter"),
    code_font = font_google("Fira Code"),
    bootswatch = "darkly"
  ),
  
  titlePanel(tags$h2("Institutional Risk Parity & Predictive Forecasting System", style = "font-weight: 700; color: #ffffff;")),
  
  uiOutput("marketSignal"),
  hr(style = "border-top: 1px solid #333;"),
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #1e1e1e; border: 1px solid #333; border-radius: 8px;",
      h4("System Parameters", style = "color: #00E676;"),
      selectInput(
        "stock",
        "Target Asset (Forecasting):",
        choices = unique(stock_data$Stock)
      ),
      hr(style = "border-top: 1px solid #333;"),
      radioButtons(
        "regime_view",
        "Active Market Regime (Optimization):",
        choices = list(
          "Overall (Static Assumption)" = "All",
          "Regime 1 (State A)" = "1",
          "Regime 2 (State B)" = "2"
        ),
        selected = "All"
      ),
      helpText("Terminal is live. Charts support click-drag zooming and exact value isolation.", style = "color: #888;")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dynamic Allocation", 
                 br(),
                 h4("Optimal Risk Parity vs. Naive Equal-Weight", style = "color: #00E676;"),
                 plotlyOutput("weightPlot"),
                 br(),
                 h4("Risk Metrics (Live Datagrid)", style = "color: #00E676;"),
                 DTOutput("riskTable") # Upgraded to interactive datatable
        ),
        tabPanel("Forecasting Engine",
                 br(),
                 h4("Live 30-Day ARIMA Projection", style = "color: #00E676;"),
                 p("Calculating forward-looking statistical trends for the selected asset...", style = "color: #888;"),
                 plotlyOutput("forecastPlot")
        ),
        tabPanel("Market States & Heatmap", 
                 br(),
                 h4("Hidden Markov Model: Market Regimes", style = "color: #00E676;"),
                 plotlyOutput("regimePlot"),
                 hr(style = "border-top: 1px solid #333;"),
                 h4("Asset Correlation Matrix", style = "color: #00E676;"),
                 plotlyOutput("heatmapPlot")
        )
      )
    )
  )
)

# -----------------------------
# 4. Server
# -----------------------------
server <- function(input, output) {
  
  active_data <- reactive({
    if (input$regime_view == "All") {
      portfolio_data
    } else {
      subset(portfolio_data, Regime == input$regime_view)
    }
  })
  
  # Upgraded Neon Banner
  output$marketSignal <- renderUI({
    if (input$regime_view == "All") {
      div(style = "padding: 15px; background-color: #2c2c2c; color: #ffffff; border-left: 5px solid #888; border-radius: 4px; font-family: 'Fira Code', monospace;",
          "SYSTEM STATUS: Aggregate View (Static Market Assumption)")
    } else if (input$regime_view == low_vol_regime) {
      div(style = "padding: 15px; background-color: rgba(0, 230, 118, 0.1); color: #00E676; border-left: 5px solid #00E676; border-radius: 4px; font-family: 'Fira Code', monospace; box-shadow: 0 0 10px rgba(0,230,118,0.2);",
          "🟢 RISK-ON: Low Volatility Market Regime Detected. Standard asset correlation expected.")
    } else {
      div(style = "padding: 15px; background-color: rgba(255, 23, 68, 0.1); color: #FF1744; border-left: 5px solid #FF1744; border-radius: 4px; font-family: 'Fira Code', monospace; box-shadow: 0 0 10px rgba(255,23,68,0.2);",
          "🔴 RISK-OFF: High Volatility Shock Detected. Deleveraging active. Correlations may converge to 1.")
    }
  })
  
  output$weightPlot <- renderPlotly({
    w_data <- calc_risk_parity(active_data())
    melted_w <- melt(w_data[, c("Stock", "Optimal_Weight", "Naive_Weight")], id.vars = "Stock")
    
    p <- ggplot(melted_w, aes(x = Stock, y = value, fill = variable, text = paste("Allocation:", round(value*100, 2), "%"))) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Optimal_Weight" = "#00E676", "Naive_Weight" = "#424242"),
                        labels = c("Risk Parity (Model)", "20% Equal-Weight")) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(y = "Capital Allocation", x = "", fill = "Strategy") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "#E0E0E0"),
        axis.text = element_text(color = "#A0A0A0"),
        panel.grid.major = element_line(color = "#333333"),
        panel.grid.minor = element_blank()
      )
    
    ggplotly(p, tooltip = "text") %>% 
      layout(hovermode = "x unified", paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  # Upgraded Interactive DataGrid
  output$riskTable <- renderDT({
    dat <- active_data()
    
    mean_ret <- aggregate(Daily_Return ~ Stock, data = dat, FUN = mean)
    vol_ret <- aggregate(Daily_Return ~ Stock, data = dat, FUN = sd)
    max_dd <- aggregate(Adjusted ~ Stock, data = dat, FUN = calc_max_drawdown)
    
    res <- merge(mean_ret, vol_ret, by = "Stock")
    res <- merge(res, max_dd, by = "Stock")
    colnames(res) <- c("Asset", "Avg_Daily_Return", "Volatility", "Max_Drawdown_Pct")
    
    res$Annualized_Sharpe <- round((res$Avg_Daily_Return / res$Volatility) * sqrt(252), 2)
    res$Avg_Daily_Return <- round(res$Avg_Daily_Return, 3)
    res$Volatility <- round(res$Volatility, 3)
    res$Max_Drawdown_Pct <- round(res$Max_Drawdown_Pct, 2)
    
    res <- res[order(-res$Annualized_Sharpe), ]
    
    datatable(res, 
              options = list(pageLength = 5, dom = 't', 
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#1e1e1e', 'color': '#00E676'});",
                               "}")),
              rownames = FALSE,
              class = 'cell-border stripe hover') %>%
      formatPercentage(c('Avg_Daily_Return', 'Volatility', 'Max_Drawdown_Pct'), 2) %>%
      formatStyle(columns = colnames(res), backgroundColor = '#121212', color = '#E0E0E0')
  })
  
  output$heatmapPlot <- renderPlotly({
    dat <- active_data()
    ret_wide <- reshape(dat[, c("Date", "Stock", "Daily_Return")], 
                        idvar = "Date", timevar = "Stock", direction = "wide")
    ret_wide <- na.omit(ret_wide)
    colnames(ret_wide) <- gsub("Daily_Return.", "", colnames(ret_wide))
    
    cor_mat <- cor(ret_wide[, -1])
    
    plot_ly(x = colnames(cor_mat), y = rownames(cor_mat), z = cor_mat, 
            type = "heatmap", colorscale = "Viridis") %>%
      layout(margin = list(l=100, b=100), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#E0E0E0'))
  })
  
  output$regimePlot <- renderPlotly({
    p <- ggplot(regimes, aes(x = Date, y = Daily_Return, color = as.factor(Regime), 
                             text = paste("Date:", Date, "<br>Return:", round(Daily_Return, 2), "%<br>Regime:", Regime))) +
      geom_point(alpha = 0.6) +
      scale_color_manual(values = c("1" = "#00b4d8", "2" = "#FF1744")) +
      labs(x = "Date", y = "Market Daily Return (%)", color = "State") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        text = element_text(color = "#E0E0E0"),
        axis.text = element_text(color = "#A0A0A0"),
        panel.grid.major = element_line(color = "#333333")
      )
    
    ggplotly(p, tooltip = "text") %>% layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$forecastPlot <- renderPlotly({
    stock_sub <- subset(portfolio_data, Stock == input$stock)
    ts_data <- ts(stock_sub$Adjusted, frequency = 252)
    
    fit <- auto.arima(ts_data, seasonal = FALSE)
    fc <- forecast(fit, h = 30)
    
    future_dates <- seq(max(stock_sub$Date) + 1, by = "day", length.out = 30)
    
    plot_ly() %>%
      add_lines(x = stock_sub$Date, y = stock_sub$Adjusted, name = "Historical Price", line = list(color = "#E0E0E0")) %>%
      add_lines(x = future_dates, y = fc$mean, name = "30-Day Forecast", line = list(color = "#00E676", dash = "dash")) %>%
      add_ribbons(x = future_dates, ymin = fc$lower[,2], ymax = fc$upper[,2], name = "95% Confidence", fillcolor = "rgba(0, 230, 118, 0.15)", line = list(color = "transparent")) %>%
      layout(xaxis = list(title = "Date", rangeslider = list(type = "date"), gridcolor = '#333333'),
             yaxis = list(title = "Adjusted Price", gridcolor = '#333333'),
             hovermode = "x unified",
             paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             font = list(color = '#E0E0E0'))
  })
}

# -----------------------------
# Run application
# -----------------------------
shinyApp(ui = ui, server = server)