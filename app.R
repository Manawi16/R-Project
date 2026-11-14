# app.R  --  Regime-Aware Risk Parity & Forecasting Terminal (UI v2)
# Packages needed: shiny, plotly, forecast, bslib, DT
# (no longer needed by the app: ggplot2, reshape2, SHAPforxgboost, shinycssloaders)

library(shiny)
library(plotly)
library(forecast)
library(bslib)
library(DT)

# =============================================================
# 1. LOAD & MERGE DATA
# =============================================================
stock_data <- read.csv("data/stock_data_clean.csv")
stock_data$Date <- as.Date(stock_data$Date)

regimes <- read.csv("data/market_regimes.csv")
regimes$Date <- as.Date(regimes$Date)
regimes$Regime <- as.character(regimes$Regime)

portfolio_data <- merge(stock_data, regimes[, c("Date", "Regime")], by = "Date")
portfolio_data$Regime <- as.factor(portfolio_data$Regime)

market_vol      <- aggregate(Daily_Return ~ Regime, data = regimes, FUN = sd)
low_vol_regime  <- as.character(market_vol$Regime[which.min(market_vol$Daily_Return)])
high_vol_regime <- as.character(market_vol$Regime[which.max(market_vol$Daily_Return)])
regimes$State   <- ifelse(regimes$Regime == low_vol_regime, "Calm", "Turbulent")
latest_state    <- regimes$State[which.max(regimes$Date)]

shap_df  <- as.data.frame(readRDS("data/shap_data.rds"))
feat_df  <- as.data.frame(readRDS("data/xgb_features.rds"))

assets    <- sort(unique(stock_data$Stock))
data_asof <- max(stock_data$Date)

# =============================================================
# 2. THEME TOKENS & HELPERS
# =============================================================
INK    <- "#E6EDF7"
MUTED  <- "#8B98B0"
GRID   <- "rgba(148,163,184,0.12)"
CYAN   <- "#22d3ee"
VIOLET <- "#a78bfa"
GREEN  <- "#34d399"
AMBER  <- "#fbbf24"
ROSE   <- "#fb7185"

ASSET_COL <- setNames(c(CYAN, GREEN, VIOLET, AMBER, ROSE),
                      c("HDFC_BANK", "ICICI_BANK", "INFOSYS", "RELIANCE", "TCS"))
asset_col <- function(x) { cols <- ASSET_COL[x]; cols[is.na(cols)] <- "#94a3b8"; unname(cols) }
pretty_name <- function(x) {
  pn <- c(HDFC_BANK = "HDFC Bank", ICICI_BANK = "ICICI Bank", INFOSYS = "Infosys",
          RELIANCE = "Reliance", TCS = "TCS")
  out <- pn[x]; out[is.na(out)] <- x[is.na(out)]; unname(out)
}

# Indian digit grouping: 1234567 -> Rs.12,34,567
inr <- function(x) {
  vapply(round(x), function(v) {
    s <- format(abs(v), scientific = FALSE, trim = TRUE)
    n <- nchar(s)
    if (n > 3) {
      rest <- gsub("(\\d)(?=(\\d{2})+$)", "\\1,", substr(s, 1, n - 3), perl = TRUE)
      s <- paste0(rest, ",", substr(s, n - 2, n))
    }
    paste0(if (v < 0) "-" else "", "\u20b9", s)
  }, character(1))
}

# One consistent look for every plotly chart
tech <- function(p, ...) {
  axis_base <- list(gridcolor = GRID, zerolinecolor = GRID, linecolor = GRID,
                    tickfont = list(color = MUTED, size = 11),
                    title = list(font = list(color = MUTED, size = 12)))
  base <- list(
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    font = list(family = "Inter, system-ui, sans-serif", color = INK, size = 12),
    hoverlabel = list(bgcolor = "#0b1220", bordercolor = CYAN,
                      font = list(family = "Fira Code, monospace", color = INK, size = 12)),
    xaxis = c(axis_base, list(showspikes = TRUE, spikemode = "across", spikecolor = CYAN,
                              spikethickness = 1, spikedash = "dot")),
    yaxis = axis_base,
    margin = list(l = 56, r = 24, t = 64, b = 44),
    legend = list(orientation = "h", x = 1, xanchor = "right", y = 1.02, yanchor = "bottom",
                  font = list(color = MUTED, size = 11)),
    hovermode = "x unified",
    dragmode = "pan"
  )
  args <- modifyList(base, list(...))
  p <- do.call(layout, c(list(p), args))
  p <- config(p, displaylogo = FALSE, responsive = TRUE, scrollZoom = TRUE,
              modeBarButtonsToRemove = list("lasso2d", "select2d", "autoScale2d",
                                            "hoverClosestCartesian", "hoverCompareCartesian",
                                            "toggleSpikelines"))
  p <- plotly_build(p)   # build once and drop the duplicated raw data: ~50% smaller, faster to draw
  p$x$attrs <- NULL
  p
}

# A row of pill buttons inside a chart that changes the view client-side (no server round-trip)
view_buttons <- function(buttons) {
  list(list(type = "buttons", direction = "right", x = 0, xanchor = "left",
            y = 1.02, yanchor = "bottom", showactive = FALSE, active = -1,
            bgcolor = "rgba(34,211,238,0.08)", bordercolor = "rgba(34,211,238,0.35)",
            font = list(color = INK, size = 11), pad = list(r = 6, t = 2, b = 2),
            buttons = buttons))
}
window_button <- function(label, dates, values, days) {
  if (is.na(days)) {
    return(list(label = label, method = "relayout",
                args = list(list(xaxis.autorange = TRUE, yaxis.autorange = TRUE))))
  }
  end <- max(dates); start <- end - days
  vis <- values[dates >= start]
  pad <- diff(range(vis)) * 0.08
  list(label = label, method = "relayout",
       args = list(list(xaxis.range = as.list(as.character(c(start, end))),
                        yaxis.range = as.list(c(min(vis) - pad, max(vis) + pad)))))
}

# ---------- finance helpers ----------
calc_risk_parity <- function(data) {
  vols <- aggregate(Daily_Return ~ Stock, data = data, FUN = sd)
  vols$Inv_Vol         <- 1 / vols$Daily_Return
  vols$Optimal_Weight  <- vols$Inv_Vol / sum(vols$Inv_Vol)
  vols$Naive_Weight    <- 1 / nrow(vols)
  vols
}
calc_max_drawdown <- function(x) {
  running_max <- cummax(x)
  min((x - running_max) / running_max) * 100
}
return_matrix <- function(dat) {
  wide <- reshape(dat[, c("Date", "Stock", "Daily_Return")],
                  idvar = "Date", timevar = "Stock", direction = "wide")
  m <- as.matrix(na.omit(wide)[, -1])
  colnames(m) <- sub("^Daily_Return\\.", "", colnames(m))
  m
}
port_stats <- function(R, w) {
  w   <- w[colnames(R)]
  mu  <- sum(w * colMeans(R)) * 252
  vol <- sqrt(as.numeric(t(w) %*% cov(R) %*% w)) * sqrt(252)
  c(ret = mu, vol = vol, sharpe = mu / vol)
}
weekdays_after <- function(last_date, h) {
  d <- seq(last_date + 1, by = "day", length.out = h * 2 + 4)
  d[!(format(d, "%u") %in% c("6", "7"))][seq_len(h)]
}

# =============================================================
# 3. CSS
# =============================================================
app_css <- "
:root{--ink:#E6EDF7;--muted:#8B98B0;--cyan:#22d3ee;--violet:#a78bfa;--rose:#fb7185;--green:#34d399;--amber:#fbbf24;
      --panel:rgba(17,26,43,.72);--edge:rgba(148,163,184,.16)}
html,body{background:#0b1220;color:var(--ink);font-family:'Inter',system-ui,sans-serif}
body{background:
  radial-gradient(1100px 600px at 8% -10%,rgba(34,211,238,.14),transparent 60%),
  radial-gradient(900px 520px at 100% 0%,rgba(167,139,250,.13),transparent 60%),#0b1220;
  background-attachment:fixed}
body::before{content:'';position:fixed;inset:0;pointer-events:none;z-index:0;
  background-image:linear-gradient(rgba(148,163,184,.05) 1px,transparent 1px),linear-gradient(90deg,rgba(148,163,184,.05) 1px,transparent 1px);
  background-size:46px 46px;-webkit-mask-image:radial-gradient(ellipse at 50% 0%,#000 25%,transparent 75%);mask-image:radial-gradient(ellipse at 50% 0%,#000 25%,transparent 75%)}
.container-fluid{position:relative;z-index:1;max-width:1600px;padding:22px 28px 36px}
::-webkit-scrollbar{width:10px;height:10px}::-webkit-scrollbar-thumb{background:rgba(148,163,184,.25);border-radius:8px}::-webkit-scrollbar-track{background:transparent}

/* header */
.hero{display:flex;align-items:center;justify-content:space-between;gap:16px;flex-wrap:wrap;margin-bottom:16px;animation:rise .7s ease both}
.hero h1{font-size:1.75rem;font-weight:700;letter-spacing:-.02em;margin:0;
  background:linear-gradient(90deg,#fff 0%,var(--cyan) 55%,var(--violet) 100%);-webkit-background-clip:text;background-clip:text;color:transparent}
.hero .sub{color:var(--muted);font-size:.9rem;margin-top:4px}
.chip{display:inline-flex;align-items:center;gap:8px;font:500 .78rem 'Fira Code',monospace;color:var(--muted);
  padding:7px 14px;border:1px solid var(--edge);border-radius:999px;background:var(--panel);backdrop-filter:blur(10px)}
.dot{width:8px;height:8px;border-radius:50%;background:var(--green);box-shadow:0 0 0 0 rgba(52,211,153,.7);animation:pulse 2s infinite}

/* status bar */
.status{padding:13px 18px;border-radius:12px;border:1px solid var(--edge);border-left:4px solid var(--c,#64748b);
  background:linear-gradient(90deg,color-mix(in srgb,var(--c,#64748b) 14%,transparent),var(--panel));
  color:var(--c,var(--ink));font:500 .85rem 'Fira Code',monospace;margin-bottom:16px;animation:rise .6s ease both}

/* layout */
.shell{display:grid;grid-template-columns:292px minmax(0,1fr);gap:20px;align-items:start}
@media(max-width:1000px){.shell{grid-template-columns:1fr}.side{position:static!important}}
.side{position:sticky;top:16px}

/* glass cards */
.glass{position:relative;overflow:hidden;background:var(--panel);border:1px solid var(--edge);border-radius:16px;
  padding:20px 22px;backdrop-filter:blur(14px);box-shadow:0 12px 40px -18px rgba(0,0,0,.7);
  transition:border-color .35s,box-shadow .35s,transform .35s;animation:rise .7s ease both}
.glass:hover{border-color:rgba(34,211,238,.35);box-shadow:0 18px 50px -20px rgba(34,211,238,.28)}
.glass h4{font-size:1.02rem;font-weight:600;margin:0 0 4px;color:#fff}
.glass .hint{color:var(--muted);font-size:.84rem;margin:0 0 10px}
.glass:has(.recalculating)::after{content:'';position:absolute;top:0;left:0;height:2px;width:35%;
  background:linear-gradient(90deg,transparent,var(--cyan),transparent);animation:sweep 1s ease-in-out infinite}
.recalculating{opacity:.4!important;filter:blur(1.5px);transition:opacity .3s,filter .3s}
.shiny-bound-output{transition:opacity .45s ease,filter .45s ease}
.grid2{display:grid;grid-template-columns:minmax(0,1.5fr) minmax(0,1fr);gap:18px;margin-bottom:18px}
@media(max-width:1100px){.grid2{grid-template-columns:1fr}}
.stack>*+*{margin-top:18px}

/* KPI cards */
.kpis{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:14px;margin-bottom:18px}
@media(max-width:1200px){.kpis{grid-template-columns:repeat(2,minmax(0,1fr))}}
@media(max-width:600px){.kpis{grid-template-columns:1fr}}
.kpi{position:relative;padding:16px 18px;border-radius:14px;border:1px solid var(--edge);background:var(--panel);
  backdrop-filter:blur(12px);overflow:hidden;transition:transform .3s,box-shadow .3s,border-color .3s;animation:rise .6s ease both}
.kpi::before{content:'';position:absolute;left:0;top:0;bottom:0;width:3px;background:var(--accent);box-shadow:0 0 18px var(--accent)}
.kpi:hover{transform:translateY(-4px);border-color:var(--accent);box-shadow:0 14px 34px -14px var(--accent)}
.kpi-tag{display:inline-block;margin-bottom:9px;font:600 .6rem 'Fira Code',monospace;letter-spacing:.08em;text-transform:uppercase;
  padding:3px 9px;border-radius:999px;border:1px solid var(--edge);color:var(--muted);background:rgba(255,255,255,.03)}
.kpi-tag.asset{color:var(--amber);border-color:rgba(251,191,36,.4)}.kpi-tag.port{color:var(--cyan);border-color:rgba(34,211,238,.4)}
.hero-right{display:flex;gap:10px;flex-wrap:wrap}
.dot.still{animation:none;background:var(--dc);box-shadow:0 0 10px var(--dc)}
.kpi-label{font-size:.72rem;letter-spacing:.09em;text-transform:uppercase;color:var(--muted);font-weight:600}
.kpi-value{font-size:1.55rem;font-weight:700;margin:6px 0 2px;letter-spacing:-.02em;color:#fff}
.kpi-sub{font:400 .78rem 'Fira Code',monospace;color:var(--muted)}
.kpi:nth-child(2){animation-delay:.07s}.kpi:nth-child(3){animation-delay:.14s}.kpi:nth-child(4){animation-delay:.21s}

/* sidebar controls */
.side h5{font-size:.72rem;letter-spacing:.12em;text-transform:uppercase;color:var(--cyan);font-weight:700;margin:0 0 14px}
.side label.control-label{font-size:.8rem;color:var(--muted);font-weight:500;margin-bottom:8px}
.side .form-group{margin-bottom:20px}
.side hr{border-color:var(--edge);margin:4px 0 20px}
.chips .control-label{display:block;margin-bottom:10px}
.chips .shiny-options-group{display:flex;flex-wrap:wrap;gap:10px 8px}
.chips .shiny-options-group>div{margin:0!important;padding:0!important;min-height:0}
.chips .shiny-options-group label{display:inline-flex;align-items:center;margin:0!important;padding:8px 15px!important;border-radius:999px;cursor:pointer;
  font-size:.82rem;line-height:1.2;white-space:nowrap;color:var(--muted);border:1px solid var(--edge);background:rgba(255,255,255,.02);
  transition:all .25s ease;user-select:none}
.chips .shiny-options-group label:hover{color:#fff;border-color:rgba(34,211,238,.5);transform:translateY(-1px)}
.chips .shiny-options-group input[type=radio]{position:absolute;opacity:0;pointer-events:none;width:0;height:0;margin:0}
.chips .shiny-options-group label:has(input:checked){color:#06121c;font-weight:600;border-color:transparent;
  background:linear-gradient(135deg,var(--cyan),var(--violet));box-shadow:0 6px 18px -6px var(--cyan)}
.side .form-group{margin-bottom:24px}
.form-control{background:rgba(255,255,255,.04)!important;border:1px solid var(--edge)!important;color:var(--ink)!important;border-radius:10px;transition:border-color .25s,box-shadow .25s}
.form-control:focus{border-color:var(--cyan)!important;box-shadow:0 0 0 3px rgba(34,211,238,.18)!important}
.irs--shiny .irs-line{background:rgba(255,255,255,.08);border-color:transparent}
.irs--shiny .irs-bar{background:linear-gradient(90deg,var(--cyan),var(--violet));border-color:transparent}
.irs--shiny .irs-handle{background:#fff;border:3px solid var(--cyan);box-shadow:0 0 12px var(--cyan)}
.irs--shiny .irs-single,.irs--shiny .irs-from,.irs--shiny .irs-to{background:var(--cyan);color:#06121c;font-weight:600}
.irs--shiny .irs-min,.irs--shiny .irs-max{background:transparent;color:var(--muted)}
.irs--shiny .irs-grid-text{color:var(--muted)}
.btn-ghost{width:100%;border-radius:10px;border:1px solid rgba(34,211,238,.4);background:rgba(34,211,238,.08);color:var(--cyan);
  font-weight:600;font-size:.85rem;padding:9px;transition:all .25s}
.btn-ghost:hover{background:rgba(34,211,238,.2);color:#fff;box-shadow:0 8px 22px -10px var(--cyan);transform:translateY(-1px)}

/* tabs */
.nav-pills{gap:6px;margin-bottom:18px;flex-wrap:wrap}
.nav-pills .nav-link{color:var(--muted);border-radius:999px;padding:9px 18px;font-size:.88rem;font-weight:500;border:1px solid transparent;transition:all .3s ease}
.nav-pills .nav-link:hover{color:#fff;background:rgba(255,255,255,.05)}
.nav-pills .nav-link.active{color:#fff;background:linear-gradient(135deg,rgba(34,211,238,.25),rgba(167,139,250,.25));
  border-color:rgba(34,211,238,.5);box-shadow:0 0 26px -8px var(--cyan)}
.tab-content>.tab-pane.active{animation:fadeslide .5s ease}

/* regime stat cards */
.rstats{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:12px;margin-top:6px}
.rstat{padding:12px 14px;border-radius:12px;border:1px solid var(--edge);border-left:3px solid var(--c);background:rgba(255,255,255,.025)}
.rstat b{color:var(--c);font-size:.95rem}.rstat span{display:block;font:400 .78rem 'Fira Code',monospace;color:var(--muted);margin-top:3px}

/* DataTable */
table.dataTable{color:var(--ink)!important;border-collapse:separate!important;border-spacing:0 4px!important;background:transparent!important}
table.dataTable thead th{color:var(--cyan)!important;border-bottom:1px solid var(--edge)!important;font-size:.74rem;letter-spacing:.06em;text-transform:uppercase;background:transparent!important}
table.dataTable tbody tr{background:rgba(255,255,255,.03)!important;transition:background .2s,transform .2s}
table.dataTable tbody tr:hover{background:rgba(34,211,238,.1)!important}
table.dataTable tbody td{border:none!important;padding:10px 12px!important}
table.dataTable tbody td:first-child{border-radius:10px 0 0 10px;font-weight:600}
table.dataTable tbody td:last-child{border-radius:0 10px 10px 0}
.dataTables_wrapper{color:var(--muted)}

.foot{color:var(--muted);font-size:.78rem;text-align:center;margin-top:26px;opacity:.8}
.js-plotly-plot .plotly .modebar{opacity:0;transition:opacity .3s}.glass:hover .js-plotly-plot .plotly .modebar{opacity:1}

@keyframes rise{from{opacity:0;transform:translateY(14px)}to{opacity:1;transform:none}}
@keyframes fadeslide{from{opacity:0;transform:translateX(10px)}to{opacity:1;transform:none}}
@keyframes sweep{0%{left:-35%}100%{left:100%}}
@keyframes pulse{0%{box-shadow:0 0 0 0 rgba(52,211,153,.6)}70%{box-shadow:0 0 0 9px rgba(52,211,153,0)}100%{box-shadow:0 0 0 0 rgba(52,211,153,0)}}
"

kpi_card <- function(label, value, sub, accent = CYAN, tag = NULL, tag_class = "port") {
  div(class = "kpi", style = paste0("--accent:", accent),
      if (!is.null(tag)) span(class = paste("kpi-tag", tag_class), tag),
      div(class = "kpi-label", label), div(class = "kpi-value", value),
      div(class = "kpi-sub", HTML(sub)))
}

# =============================================================
# 4. UI
# =============================================================
ui <- fluidPage(
  title = "Risk Parity Terminal",
  theme = bs_theme(version = 5, bootswatch = "darkly", bg = "#0b1220", fg = "#E6EDF7",
                   primary = "#22d3ee",
                   base_font = "Inter, system-ui, -apple-system, sans-serif",
                   code_font = "Fira Code, monospace"),
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Fira+Code:wght@400;500&display=swap"),
    tags$style(HTML(app_css))
  ),

  div(class = "hero",
      div(h1("Institutional Risk Parity & Predictive Forecasting System"),
          div(class = "sub", "Regime-aware allocation \u00b7 explainable ML \u00b7 ARIMA outlook \u2014 five NSE large caps")),
      div(class = "hero-right",
          span(class = "chip", span(class = "dot still",
                                    style = paste0("--dc:", if (latest_state == "Calm") GREEN else ROSE)),
               paste("LATEST MARKET STATE:", toupper(latest_state))),
          span(class = "chip", span(class = "dot"),
               paste("DATA THROUGH", toupper(format(data_asof, "%d %b %Y")))))),

  uiOutput("marketSignal"),
  uiOutput("kpis"),

  div(class = "shell",
      # ---------------- control deck ----------------
      div(class = "side glass",
          h5("Control deck"),
          div(class = "chips",
              radioButtons("stock", "Target asset", inline = TRUE,
                           choices = setNames(assets, pretty_name(assets)), selected = assets[1])),
          div(class = "chips",
              radioButtons("regime_view", "Market regime (optimisation)", inline = TRUE,
                           choices = setNames(
                             c("All", low_vol_regime, high_vol_regime),
                             c("Overall", paste0("Calm \u00b7 R", low_vol_regime),
                               paste0("Turbulent \u00b7 R", high_vol_regime))),
                           selected = "All")),
          hr(),
          numericInput("capital", "Capital to allocate (\u20b9)", value = 100000, min = 1000, step = 10000),
          sliderInput("horizon", "Forecast horizon (trading days)", min = 7, max = 90, value = 30, step = 1, ticks = FALSE),
          downloadButton("dl_alloc", "Export allocation (CSV)", class = "btn-ghost"),
          p(style = "color:#8B98B0;font-size:.76rem;margin:16px 0 0;line-height:1.5",
            "Tip: scroll to zoom, drag to pan, double-click a chart to reset. Use the pill buttons above each chart for quick windows.")
      ),

      # ---------------- main stage ----------------
      div(
        tabsetPanel(
          id = "tabs", type = "pills",

          tabPanel("Market Overview",
            div(class = "glass",
                h4("Relative performance \u2014 rebased to 100"),
                p(class = "hint", "All five assets start at 100 so growth is directly comparable. Your target asset is highlighted."),
                plotlyOutput("overviewPlot", height = "500px"))),

          tabPanel("Dynamic Allocation",
            div(class = "grid2",
                div(class = "glass",
                    h4("Risk Parity vs. naive equal-weight"),
                    p(class = "hint", "Inverse-volatility weights: calmer assets get more capital so each contributes similar risk."),
                    plotlyOutput("weightPlot", height = "380px")),
                div(class = "glass",
                    h4("Recommended split"),
                    p(class = "hint", "Hover a slice for the rupee amount."),
                    plotlyOutput("donutPlot", height = "380px"))),
            div(class = "glass",
                h4("Risk metrics"),
                p(class = "hint", "Click any column header to sort."),
                DTOutput("riskTable"))),

          tabPanel("Forecasting Engine",
            div(class = "glass",
                h4(textOutput("fcTitle", inline = TRUE)),
                p(class = "hint", "Shaded bands are 80% and 95% prediction intervals."),
                plotlyOutput("forecastPlot", height = "540px"),
                uiOutput("modelNote"))),

          tabPanel("Explainable AI",
            div(class = "glass",
                h4("XGBoost SHAP explanations (TCS proof-of-concept)"),
                p(class = "hint", "Which indicators push tomorrow's up/down prediction, and how hard. Each dot is one trading day."),
                plotlyOutput("shapPlot", height = "540px"))),

          tabPanel("Market States & Heatmap",
            div(class = "stack",
                div(class = "glass",
                    h4("Hidden Markov Model: market regimes"),
                    p(class = "hint", "Each dot is one day. Selecting a regime in the control deck spotlights it."),
                    plotlyOutput("regimePlot", height = "420px"),
                    uiOutput("regimeStats")),
                div(class = "glass",
                    h4("Asset correlation matrix"),
                    p(class = "hint", "Based on the selected regime. Notice how correlations shift in turbulent markets."),
                    plotlyOutput("heatmapPlot", height = "440px"))))
        ),
        div(class = "foot", "Educational project \u2014 historical analysis, not financial advice.")
      )
  )
)

# =============================================================
# 5. SERVER
# =============================================================
server <- function(input, output, session) {

  active_data <- reactive({
    if (input$regime_view == "All") portfolio_data
    else subset(portfolio_data, Regime == input$regime_view)
  })

  capital <- reactive({ v <- input$capital; if (is.null(v) || is.na(v) || v < 0) 0 else v })

  alloc <- reactive({
    w <- calc_risk_parity(active_data())
    w$Allocation <- w$Optimal_Weight * capital()
    w
  })

  risk_tbl <- reactive({
    dat <- active_data()
    res <- merge(merge(aggregate(Daily_Return ~ Stock, dat, mean),
                       aggregate(Daily_Return ~ Stock, dat, sd), by = "Stock"),
                 aggregate(Adjusted ~ Stock, dat, calc_max_drawdown), by = "Stock")
    colnames(res) <- c("Stock", "Avg", "Vol", "MaxDD")
    res$Sharpe <- (res$Avg / res$Vol) * sqrt(252)
    res[order(-res$Sharpe), ]
  })

  # ---- forecasting (model is fitted once per stock, then cached) ----
  fc_fit <- bindCache(reactive({
    s <- stock_data[stock_data$Stock == input$stock, ]
    s <- s[order(s$Date), ]
    list(model = auto.arima(ts(s$Adjusted, frequency = 252), seasonal = FALSE),
         dates = s$Date, y = s$Adjusted)
  }), input$stock)

  fc <- reactive({
    f <- fc_fit()
    list(fit = f, fcst = forecast(f$model, h = input$horizon, level = c(80, 95)),
         dates = weekdays_after(max(f$dates), input$horizon))
  })

  # ---------------- header widgets ----------------
  output$marketSignal <- renderUI({
    if (input$regime_view == "All") {
      div(class = "status", style = "--c:#94a3b8",
          "SYSTEM STATUS \u00b7 Aggregate view \u2014 static market assumption")
    } else if (input$regime_view == low_vol_regime) {
      div(class = "status", style = paste0("--c:", GREEN),
          "\U0001F7E2 RISK-ON \u00b7 Low-volatility regime. Standard asset correlation expected.")
    } else {
      div(class = "status", style = paste0("--c:", ROSE),
          "\U0001F534 RISK-OFF \u00b7 High-volatility shock. Correlations may converge towards 1.")
    }
  })

  output$kpis <- renderUI({
    rt <- risk_tbl(); top <- rt[1, ]
    R  <- return_matrix(active_data())
    w  <- alloc(); wn <- setNames(w$Optimal_Weight, w$Stock)
    eq <- port_stats(R, setNames(rep(1 / ncol(R), ncol(R)), colnames(R)))
    rp <- port_stats(R, wn)
    chg <- (rp["vol"] / eq["vol"] - 1) * 100

    f <- fc(); last <- tail(f$fit$y, 1)
    hi <- tail(f$fcst$upper[, 2], 1); lo <- tail(f$fcst$lower[, 2], 1)

    rank_n <- which(rt$Stock == input$stock); me <- rt[rank_n, ]
    reg_lbl <- if (input$regime_view == "All") "Overall"
               else if (input$regime_view == low_vol_regime) "Calm regime" else "Turbulent regime"

    div(class = "kpis",
        kpi_card("Best risk-adjusted asset", pretty_name(top$Stock),
                 sprintf("%s | Sharpe %.2f | max DD %.1f%%", reg_lbl, top$Sharpe, top$MaxDD),
                 CYAN, "All 5 | follows regime", "port"),
        kpi_card("Risk-parity volatility", sprintf("%.1f%% p.a.", rp["vol"]),
                 sprintf("%s | %+.1f%% vs equal-weight (%.1f%%)", reg_lbl, chg, eq["vol"]),
                 GREEN, "All 5 | follows regime", "port"),
        kpi_card(paste(pretty_name(input$stock), "risk rank"),
                 sprintf("#%d of %d", rank_n, nrow(rt)),
                 sprintf("Sharpe %.2f | vol %.2f%% | max DD %.1f%%", me$Sharpe, me$Vol, me$MaxDD),
                 VIOLET, "Target asset", "asset"),
        kpi_card(paste(pretty_name(input$stock), input$horizon, "day outlook"),
                 inr(tail(f$fcst$mean, 1)),
                 sprintf("95%% range %s - %s", inr(lo), inr(hi)),
                 AMBER, "Target asset | horizon", "asset"))
  })

  # ---------------- Market Overview ----------------
  output$overviewPlot <- renderPlotly({
    p <- plot_ly()
    all_dates <- stock_data$Date
    for (s in assets) {
      d <- stock_data[stock_data$Stock == s, ]; d <- d[order(d$Date), ]
      y <- d$Adjusted / d$Adjusted[1] * 100
      sel <- identical(s, input$stock)
      p <- add_lines(p, x = d$Date, y = y, name = pretty_name(s),
                     line = list(color = asset_col(s), width = if (sel) 3.4 else 1.4),
                     opacity = if (sel) 1 else 0.5,
                     hovertemplate = paste0("%{y:.1f}<extra>", pretty_name(s), "</extra>"))
    }
    rebased <- unlist(lapply(assets, function(s) { d <- stock_data[stock_data$Stock == s, ]
                                                   d <- d[order(d$Date), ]; d$Adjusted / d$Adjusted[1] * 100 }))
    dts <- do.call(c, lapply(assets, function(s) sort(stock_data$Date[stock_data$Stock == s])))
    tech(p,
         yaxis = list(title = list(text = "Index (start = 100)")),
         updatemenus = view_buttons(list(
           window_button("6M", dts, rebased, 183), window_button("1Y", dts, rebased, 365),
           window_button("3Y", dts, rebased, 1095), window_button("All", dts, rebased, NA))))
  })

  # ---------------- Allocation ----------------
  output$weightPlot <- renderPlotly({
    w <- alloc(); nm <- pretty_name(w$Stock)
    plot_ly() %>%
      add_bars(x = nm, y = w$Naive_Weight, name = "Equal-weight",
               marker = list(color = "rgba(148,163,184,.35)", line = list(color = "rgba(148,163,184,.6)", width = 1)),
               text = sprintf("%.1f%%", w$Naive_Weight * 100), textposition = "outside",
               hovertemplate = paste0("<b>%{x}</b><br>Equal-weight: %{y:.1%}<extra></extra>")) %>%
      add_bars(x = nm, y = w$Optimal_Weight, name = "Risk parity (model)",
               marker = list(color = asset_col(w$Stock), line = list(color = "#fff", width = .5)),
               text = sprintf("%.1f%%", w$Optimal_Weight * 100), textposition = "outside",
               customdata = inr(w$Allocation),
               hovertemplate = paste0("<b>%{x}</b><br>Risk parity: %{y:.1%}<br>Capital: %{customdata}<extra></extra>")) %>%
      tech(barmode = "group", bargap = .28, hovermode = "closest", dragmode = FALSE,
           yaxis = list(tickformat = ".0%", title = list(text = "Capital allocation"), rangemode = "tozero"),
           xaxis = list(showspikes = FALSE))
  })

  output$donutPlot <- renderPlotly({
    w <- alloc()
    plot_ly(labels = pretty_name(w$Stock), values = w$Optimal_Weight, type = "pie", hole = .62,
            domain = list(x = c(0.04, 0.96), y = c(0.17, 1)),
            sort = FALSE, direction = "clockwise", textinfo = "percent",
            textfont = list(color = "#06121c", size = 12),
            marker = list(colors = asset_col(w$Stock), line = list(color = "#0b1220", width = 3)),
            customdata = inr(w$Allocation),
            hovertemplate = "<b>%{label}</b><br>%{percent}<br>%{customdata}<extra></extra>") %>%
      tech(showlegend = TRUE, hovermode = "closest", dragmode = FALSE,
           legend = list(orientation = "h", x = .5, xanchor = "center", y = 0.02, yanchor = "top", font = list(color = MUTED)),
           margin = list(l = 10, r = 10, t = 10, b = 50),
           annotations = list(list(text = paste0("<b>", inr(capital()), "</b><br><span style='font-size:11px;color:#8B98B0'>risk parity</span>"),
                                   showarrow = FALSE, x = .5, y = .585, xref = "paper", yref = "paper",
                                   font = list(size = 15, color = INK))))
  })

  output$riskTable <- renderDT({
    rt <- risk_tbl(); w <- alloc()
    rt <- merge(rt, w[, c("Stock", "Optimal_Weight", "Allocation")], by = "Stock")
    rt <- rt[order(-rt$Sharpe), ]
    df <- data.frame(Asset = pretty_name(rt$Stock),
                     `Avg daily return (%)` = rt$Avg, `Volatility (%)` = rt$Vol,
                     `Annualised Sharpe` = rt$Sharpe, `Max drawdown (%)` = rt$MaxDD,
                     `Risk-parity weight (%)` = rt$Optimal_Weight * 100,
                     `Allocation` = inr(rt$Allocation),
                     check.names = FALSE)
    datatable(df, rownames = FALSE, class = "hover",
              options = list(dom = "t", paging = FALSE, order = list(list(3, "desc")),
                             columnDefs = list(list(orderable = FALSE, targets = 6),
                                               list(className = "dt-right", targets = 1:6)))) %>%
      formatRound(c("Avg daily return (%)", "Volatility (%)", "Annualised Sharpe",
                    "Max drawdown (%)", "Risk-parity weight (%)"), 2) %>%
      formatStyle("Annualised Sharpe",
                  background = styleColorBar(c(0, max(df$`Annualised Sharpe`, 0.01) * 1.05), "rgba(34,211,238,.38)", angle = -90),
                  backgroundSize = "96% 62%", backgroundRepeat = "no-repeat", backgroundPosition = "center") %>%
      formatStyle("Max drawdown (%)", color = styleInterval(-25, c(ROSE, AMBER)))
  })

  output$dl_alloc <- downloadHandler(
    filename = function() paste0("allocation_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      w <- alloc()
      write.csv(data.frame(Asset = pretty_name(w$Stock),
                           Risk_Parity_Weight_Pct = round(w$Optimal_Weight * 100, 2),
                           Equal_Weight_Pct = round(w$Naive_Weight * 100, 2),
                           Allocation_INR = round(w$Allocation)), file, row.names = FALSE)
    })

  # ---------------- Forecast ----------------
  output$fcTitle <- renderText(paste0(pretty_name(input$stock), " - ", input$horizon, "-day ARIMA projection"))

  output$forecastPlot <- renderPlotly({
    f <- fc(); hist_d <- f$fit$dates; hist_y <- f$fit$y; fd <- f$dates; fcst <- f$fcst
    last_d <- max(hist_d); last_y <- tail(hist_y, 1)
    xs <- c(last_d, fd)
    col <- asset_col(input$stock)

    ribbon <- function(p, lo, hi, fill, nm, show) {
      add_ribbons(p, x = xs, ymin = c(last_y, lo), ymax = c(last_y, hi), name = nm,
                  fillcolor = fill, line = list(color = "transparent"), hoverinfo = "skip",
                  showlegend = show)
    }
    win_all <- c(hist_y, fcst$upper[, 2], fcst$lower[, 2])
    all_d   <- c(hist_d, fd, fd)

    plot_ly() %>%
      ribbon(fcst$lower[, 2], fcst$upper[, 2], "rgba(251,191,36,.10)", "95% interval", TRUE) %>%
      ribbon(fcst$lower[, 1], fcst$upper[, 1], "rgba(251,191,36,.22)", "80% interval", TRUE) %>%
      add_lines(x = hist_d, y = hist_y, name = "Historical",
                line = list(color = col, width = 2.2),
                hovertemplate = "\u20b9%{y:,.1f}<extra>Historical</extra>") %>%
      add_lines(x = xs, y = c(last_y, as.numeric(fcst$mean)), name = "Forecast",
                line = list(color = AMBER, width = 2.6, dash = "dash"),
                hovertemplate = "\u20b9%{y:,.1f}<extra>Forecast</extra>") %>%
      add_markers(x = last_d, y = last_y, name = "Last close", showlegend = FALSE,
                  marker = list(color = "#fff", size = 9, line = list(color = AMBER, width = 3)),
                  hovertemplate = "\u20b9%{y:,.1f}<extra>Last close</extra>") %>%
      tech(yaxis = list(title = list(text = "Adjusted price (\u20b9)"),
                        range = {
                          vis <- c(hist_y[hist_d >= last_d - 365], fcst$upper[, 2], fcst$lower[, 2])
                          pad <- diff(range(vis)) * .08; as.list(c(min(vis) - pad, max(vis) + pad)) }),
           xaxis = list(range = as.list(as.character(c(last_d - 365, max(fd) + 6)))),
           updatemenus = view_buttons(list(
             window_button("3M", all_d, win_all, 92), window_button("6M", all_d, win_all, 183),
             window_button("1Y", all_d, win_all, 365), window_button("3Y", all_d, win_all, 1095),
             window_button("All", all_d, win_all, NA))))
  })

  output$modelNote <- renderUI({
    m <- fc()$fit$model
    ord <- arimaorder(m)
    walk <- all(ord[c("p", "q")] == 0) && ord["d"] == 1
    div(style = "margin-top:12px;font:400 .8rem 'Fira Code',monospace;color:#8B98B0",
        paste0("Selected model: ", as.character(m), if (walk)
          "  \u00b7  a random walk \u2014 the line is flat by design; the widening bands are the real message."
          else ""))
  })

  # ---------------- SHAP (native plotly, switchable view) ----------------
  output$shapPlot <- renderPlotly({
    feats <- intersect(colnames(shap_df), colnames(feat_df))
    imp <- sort(colMeans(abs(shap_df[, feats])))          # ascending -> biggest on top
    k <- length(imp)
    set.seed(7)
    xs <- ys <- cs <- tx <- c()
    for (j in seq_len(k)) {
      nm <- names(imp)[j]; v <- shap_df[[nm]]; r <- feat_df[[nm]]
      dens <- density(v, n = 256); wdt <- approx(dens$x, dens$y, xout = v)$y; wdt <- wdt / max(wdt)
      q <- quantile(r, c(.02, .98), na.rm = TRUE)
      cval <- pmin(pmax((r - q[1]) / max(q[2] - q[1], 1e-9), 0), 1)
      xs <- c(xs, round(v, 4)); ys <- c(ys, round(j + runif(length(v), -1, 1) * wdt * .38, 3)); cs <- c(cs, round(cval, 3))
      tx <- c(tx, sprintf("<b>%s</b><br>Impact: %+.3f<br>Value: %.2f", nm, v, r))
    }
    p <- plot_ly() %>%
      add_trace(type = "scatter", mode = "markers", x = xs, y = ys, name = "SHAP", showlegend = FALSE,
                text = tx, hoverinfo = "text",
                marker = list(size = 5.5, opacity = .78, color = cs,
                              colorscale = list(c(0, CYAN), c(.5, VIOLET), c(1, ROSE)),
                              cmin = 0, cmax = 1,
                              colorbar = list(title = list(text = "Feature value", font = list(color = MUTED, size = 11)),
                                              tickvals = c(0.04, 0.96), ticktext = c("Low", "High"),
                                              tickfont = list(color = MUTED), thickness = 10, len = .5,
                                              outlinewidth = 0, x = 1.01))) %>%
      add_trace(type = "bar", orientation = "h", x = as.numeric(imp), y = seq_len(k), visible = FALSE,
                width = .55, name = "Importance", showlegend = FALSE,
                marker = list(color = as.numeric(imp), colorscale = list(c(0, "#155e75"), c(1, CYAN))),
                text = sprintf("%.3f", imp), textposition = "outside", textfont = list(color = INK),
                hovertemplate = "<b>%{customdata}</b><br>Mean |impact|: %{x:.3f}<extra></extra>",
                customdata = names(imp))
    tech(p, hovermode = "closest", dragmode = "zoom",
         margin = list(l = 110, r = 70, t = 64, b = 56),
         xaxis = list(title = list(text = "\u2190 pushes prediction DOWN   |   UP \u2192"), showspikes = FALSE),
         yaxis = list(tickmode = "array", tickvals = seq_len(k), ticktext = names(imp),
                      range = c(.4, k + .6), showgrid = FALSE),
         shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = .4, y1 = k + .6,
                            line = list(color = "rgba(255,255,255,.45)", width = 1, dash = "dash"))),
         updatemenus = view_buttons(list(
           list(label = "Every day (beeswarm)", method = "update",
                args = list(list(visible = list(TRUE, FALSE)),
                            list(xaxis = list(title = list(text = "\u2190 pushes prediction DOWN   |   UP \u2192"),
                                              autorange = TRUE)))),
           list(label = "Overall importance", method = "update",
                args = list(list(visible = list(FALSE, TRUE)),
                            list(xaxis = list(title = list(text = "Mean |SHAP| (average influence)"),
                                              autorange = TRUE)))))))
  })

  # ---------------- Regimes & heatmap ----------------
  output$regimePlot <- renderPlotly({
    sel <- input$regime_view
    p <- plot_ly()
    for (st in c("Calm", "Turbulent")) {
      d <- regimes[regimes$State == st, ]
      this_r <- if (st == "Calm") low_vol_regime else high_vol_regime
      alpha <- if (sel == "All" || sel == this_r) .8 else .12
      p <- add_markers(p, x = d$Date, y = d$Daily_Return, name = st,
                       marker = list(size = if (alpha > .5) 6 else 4, opacity = alpha,
                                     color = if (st == "Calm") CYAN else ROSE),
                       text = paste0(format(d$Date, "%d %b %Y"), "<br>Return: ", sprintf("%+.2f%%", d$Daily_Return),
                                     "<br>Regime ", d$Regime, " (", st, ")"),
                       hoverinfo = "text")
    }
    dts <- regimes$Date; y <- regimes$Daily_Return
    tech(p, hovermode = "closest",
         yaxis = list(title = list(text = "Market daily return (%)")),
         updatemenus = view_buttons(list(
           window_button("6M", dts, y, 183), window_button("1Y", dts, y, 365),
           window_button("3Y", dts, y, 1095), window_button("All", dts, y, NA))))
  })

  output$regimeStats <- renderUI({
    cards <- lapply(c("Calm", "Turbulent"), function(st) {
      d <- regimes[regimes$State == st, ]
      col <- if (st == "Calm") CYAN else ROSE
      div(class = "rstat", style = paste0("--c:", col),
          tags$b(paste0(st, " \u00b7 Regime ", d$Regime[1])),
          span(sprintf("%d days (%.0f%% of sample)", nrow(d), 100 * nrow(d) / nrow(regimes))),
          span(sprintf("avg %+.3f%%/day \u00b7 volatility %.2f%%", mean(d$Daily_Return), sd(d$Daily_Return))))
    })
    div(class = "rstats", cards)
  })

  output$heatmapPlot <- renderPlotly({
    R <- return_matrix(active_data()); cm <- cor(R); nm <- pretty_name(colnames(cm))
    zmin <- min(0, floor(min(cm) * 10) / 10)
    ann <- list()
    for (i in seq_len(nrow(cm))) for (j in seq_len(ncol(cm)))
      ann[[length(ann) + 1]] <- list(x = nm[j], y = nm[i], text = sprintf("%.2f", cm[i, j]), showarrow = FALSE,
                                     xref = "x", yref = "y",
                                     font = list(size = 13, color = if (cm[i, j] > .6) "#06121c" else INK))
    plot_ly(x = nm, y = nm, z = cm, type = "heatmap", zmin = zmin, zmax = 1, xgap = 4, ygap = 4,
            colorscale = list(c(0, "#0f172a"), c(.45, "#5b21b6"), c(.75, "#7c3aed"), c(1, CYAN)),
            colorbar = list(thickness = 10, outlinewidth = 0, tickfont = list(color = MUTED), len = .8),
            hovertemplate = "%{y} \u00d7 %{x}<br>correlation %{z:.3f}<extra></extra>") %>%
      tech(hovermode = "closest", dragmode = FALSE, annotations = ann,
           xaxis = list(showspikes = FALSE, showgrid = FALSE, side = "bottom"),
           yaxis = list(autorange = "reversed", showgrid = FALSE),
           margin = list(l = 100, r = 20, t = 10, b = 70))
  })
}

shinyApp(ui = ui, server = server)