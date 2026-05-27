# ==============================================================================
# R Shiny App: Market and Economic Dashboard
# ==============================================================================
# Features:
# - Home summary tab
# - User-selected stock ticker, defaulting to Raspberry Pi Holdings plc: RPI.L
# - Inflation, unemployment and government debt from the World Bank API
# - Brent crude oil from FRED
# - VIX, Bitcoin, gold and silver from Yahoo Finance
# - UK benefit claimant count from the Office for National Statistics
# - Yahoo Finance RSS news feed
#
# Install packages if needed:
# install.packages(c("shiny", "quantmod", "DT", "xml2", "jsonlite"))
# ==============================================================================


# ==============================================================================
# Load Packages
# ==============================================================================

library(shiny)
library(quantmod)
library(DT)
library(xml2)
library(jsonlite)


# ==============================================================================
# Country Setup
# ==============================================================================
# These are the countries used for the World Bank macroeconomic data.
# ISO3 codes are used by the World Bank API.

country_map <- data.frame(
  Country = c(
    "United Kingdom",
    "United States",
    "Germany",
    "France",
    "Japan",
    "Italy",
    "Canada",
    "China"
  ),
  ISO3 = c(
    "GBR",
    "USA",
    "DEU",
    "FRA",
    "JPN",
    "ITA",
    "CAN",
    "CHN"
  ),
  stringsAsFactors = FALSE
)

# Colours used for country line charts.
macro_cols <- c(
  "#1f77b4",
  "#ff7f0e",
  "#2ca02c",
  "#d62728",
  "#9467bd",
  "#8c564b",
  "#e377c2",
  "#17becf"
)


# ==============================================================================
# Helper Functions
# ==============================================================================

# ------------------------------------------------------------------------------
# Safely fetch market/economic time series through quantmod
# ------------------------------------------------------------------------------
# quantmod::getSymbols() may fail if:
# - the internet connection is unavailable
# - the ticker is wrong
# - Yahoo Finance/FRED is temporarily unavailable
#
# This wrapper returns NULL rather than crashing the app.
# ------------------------------------------------------------------------------

safe_get_symbols <- function(symbol, src = "yahoo", from = Sys.Date() - 365) {
  tryCatch(
    {
      raw <- suppressWarnings(
        getSymbols(
          Symbols = symbol,
          src = src,
          from = from,
          auto.assign = FALSE
        )
      )
      
      raw <- na.omit(raw)
      
      if (NROW(raw) == 0) {
        return(NULL)
      }
      
      raw
    },
    error = function(e) {
      NULL
    }
  )
}


# ------------------------------------------------------------------------------
# Convert OHLC data from quantmod into a plain data frame
# ------------------------------------------------------------------------------
# OHLC means:
# - Open
# - High
# - Low
# - Close
#
# This makes the downloaded market data easier to show in a DT table.
# ------------------------------------------------------------------------------

make_price_df <- function(data, include_volume = TRUE) {
  if (is.null(data)) {
    return(data.frame())
  }
  
  df <- data.frame(
    Date = index(data),
    Open = as.numeric(Op(data)),
    High = as.numeric(Hi(data)),
    Low = as.numeric(Lo(data)),
    Close = as.numeric(Cl(data))
  )
  
  if (include_volume) {
    df$Volume <- as.numeric(Vo(data))
  }
  
  df
}


# ------------------------------------------------------------------------------
# Fetch World Bank indicator data
# ------------------------------------------------------------------------------
# Example indicators:
# - FP.CPI.TOTL.ZG       inflation, consumer prices, annual %
# - SL.UEM.TOTL.ZS       unemployment, total % of labour force
# - GC.DOD.TOTL.GD.ZS    central government debt, % of GDP
# ------------------------------------------------------------------------------

fetch_world_bank_indicator <- function(indicator, start_year, end_year) {
  
  countries <- paste(country_map$ISO3, collapse = ";")
  
  url <- paste0(
    "https://api.worldbank.org/v2/country/",
    countries,
    "/indicator/",
    indicator,
    "?format=json&per_page=20000&date=",
    start_year,
    ":",
    end_year
  )
  
  tryCatch(
    {
      wb_raw <- jsonlite::fromJSON(url, flatten = TRUE)
      
      # World Bank JSON usually returns:
      # item 1 = metadata
      # item 2 = data
      if (length(wb_raw) < 2 || is.null(wb_raw[[2]])) {
        return(data.frame())
      }
      
      df <- as.data.frame(wb_raw[[2]], stringsAsFactors = FALSE)
      
      if (nrow(df) == 0) {
        return(data.frame())
      }
      
      required_columns <- c("countryiso3code", "date", "value", "country.value")
      
      if (!all(required_columns %in% names(df))) {
        return(data.frame())
      }
      
      out <- data.frame(
        ISO3 = as.character(df$countryiso3code),
        CountryFromSource = as.character(df$country.value),
        YearNumber = as.integer(df$date),
        Value = as.numeric(df$value),
        stringsAsFactors = FALSE
      )
      
      # Merge with country_map so country names are consistent across the app.
      out <- merge(
        country_map,
        out[, c("ISO3", "YearNumber", "Value")],
        by = "ISO3",
        all.x = TRUE
      )
      
      out <- out[order(out$Country, out$YearNumber), ]
      rownames(out) <- NULL
      
      out
    },
    error = function(e) {
      data.frame()
    }
  )
}


# ------------------------------------------------------------------------------
# Get the latest non-missing World Bank value for each country
# ------------------------------------------------------------------------------
# World Bank series may not have the same latest year for every country.
# This function finds the most recent non-missing value per country.
# ------------------------------------------------------------------------------

latest_by_country <- function(df, value_name) {
  
  if (nrow(df) == 0) {
    return(data.frame())
  }
  
  df <- df[!is.na(df$Value), ]
  
  if (nrow(df) == 0) {
    return(data.frame())
  }
  
  latest_rows <- do.call(
    rbind,
    lapply(split(df, df$Country), function(x) {
      x[which.max(x$YearNumber), ]
    })
  )
  
  latest_rows <- latest_rows[, c("Country", "YearNumber", "Value")]
  
  names(latest_rows) <- c(
    "Country",
    paste0(value_name, " Year"),
    value_name
  )
  
  rownames(latest_rows) <- NULL
  
  latest_rows
}


# ------------------------------------------------------------------------------
# Reusable base R line plot for World Bank macroeconomic data
# ------------------------------------------------------------------------------
# This keeps the three macroeconomic line charts consistent.
# ------------------------------------------------------------------------------

plot_macro_series <- function(df, title, ylab) {
  
  shiny::validate(
    shiny::need(nrow(df) > 0, "World Bank data could not be loaded.")
  )
  
  df <- df[!is.na(df$Value), ]
  
  shiny::validate(
    shiny::need(nrow(df) > 0, "No non-missing values returned for this indicator.")
  )
  
  countries <- unique(df$Country)
  years <- sort(unique(df$YearNumber))
  
  plot(
    NULL,
    xlim = range(df$YearNumber, na.rm = TRUE),
    ylim = range(df$Value, na.rm = TRUE),
    main = title,
    xlab = "Year",
    ylab = ylab,
    xaxt = "n",
    las = 1
  )
  
  axis(1, at = years, labels = years, las = 2)
  grid(nx = NULL, ny = NULL, lty = 2, col = "gray90")
  
  for (i in seq_along(countries)) {
    country_data <- df[df$Country == countries[i], ]
    
    lines(
      country_data$YearNumber,
      country_data$Value,
      type = "o",
      col = macro_cols[i],
      lwd = 2,
      pch = 16
    )
  }
  
  legend(
    "topright",
    legend = countries,
    col = macro_cols[seq_along(countries)],
    lwd = 2,
    pch = 16,
    cex = 0.75,
    bty = "n"
  )
}


# ------------------------------------------------------------------------------
# Fetch UK Claimant Count data from ONS
# ------------------------------------------------------------------------------
# Source:
# ONS time series BCJD
# Claimant Count: K02000001 UK: People: SA: Thousands
#
# This is the seasonally adjusted UK claimant count, measured in thousands.
# It is not the same as the unemployment rate.
# ------------------------------------------------------------------------------

fetch_uk_claimant_count <- function() {
  
  url <- paste0(
    "https://www.ons.gov.uk/",
    "employmentandlabourmarket/peoplenotinwork/outofworkbenefits/",
    "timeseries/bcjd/unem/data"
  )
  
  tryCatch(
    {
      raw <- jsonlite::fromJSON(url, flatten = TRUE)
      
      # ONS time series JSON normally contains a "months" data frame.
      if (!"months" %in% names(raw)) {
        return(data.frame())
      }
      
      df <- as.data.frame(raw$months, stringsAsFactors = FALSE)
      
      if (nrow(df) == 0) {
        return(data.frame())
      }
      
      if (!all(c("date", "value") %in% names(df))) {
        return(data.frame())
      }
      
      # ONS values can sometimes arrive as character strings.
      # gsub removes commas before conversion to numeric.
      claimant_value <- as.numeric(gsub(",", "", as.character(df$value)))
      
      out <- data.frame(
        Period = as.character(df$date),
        Label = if ("label" %in% names(df)) {
          as.character(df$label)
        } else {
          as.character(df$date)
        },
        ClaimantCountThousands = claimant_value,
        stringsAsFactors = FALSE
      )
      
      out <- out[!is.na(out$ClaimantCountThousands), ]
      
      # Sequence is used for plotting because ONS date labels are not always
      # simple R Date objects.
      out$Sequence <- seq_len(nrow(out))
      
      rownames(out) <- NULL
      
      out
    },
    error = function(e) {
      data.frame()
    }
  )
}


# ==============================================================================
# User Interface
# ==============================================================================
# The UI defines the visible layout: tabs, headings, plots and tables.
# ==============================================================================

ui <- fluidPage(
  
  titlePanel("Market and Economic Dashboard"),
  
  tabsetPanel(
    
    # --------------------------------------------------------------------------
    # Home Tab
    # --------------------------------------------------------------------------
    
    tabPanel(
      "Home",
      
      h2("Market and Economic Dashboard"),
      
      p(
        "This dashboard combines live financial market data with official ",
        "macroeconomic indicators. Live market data is pulled from Yahoo Finance ",
        "and FRED. Inflation, unemployment and central government debt data are ",
        "pulled from the World Bank API."
      ),
      
      p(

      
        "The UK claimant count is pulled from the Office for National Statistics."
      ),
      
      hr(),
      
      h3("Live Market Snapshot"),
      uiOutput("homeMarketSummary"),
      
      hr(),
      
      h3("Latest Official Macroeconomic Summary"),
      p(
        "This table shows the latest non-missing World Bank value for each country. ",
        "Different countries may have different latest available years."
      ),
      DTOutput("homeMacroSummaryTable"),
      
      hr(),
      
      h3("Data Source Notes"),
      tags$ul(
        tags$li(strong("Inflation: "), "World Bank indicator FP.CPI.TOTL.ZG."),
        tags$li(strong("Unemployment: "), "World Bank indicator SL.UEM.TOTL.ZS."),
        tags$li(strong("Central government debt: "), "World Bank indicator GC.DOD.TOTL.GD.ZS."),
        tags$li(strong("Bitcoin, gold, silver, VIX and selected stocks: "), "Yahoo Finance via quantmod."),
        tags$li(strong("Brent crude oil: "), "FRED series DCOILBRENTEU via quantmod."),
        tags$li(strong("UK claimant count: "), "ONS time series BCJD, seasonally adjusted UK claimant count in thousands."),
        tags$li(strong("News feed: "), "Yahoo Finance RSS feed for the selected ticker.")
      )
    ),
    
    
    # --------------------------------------------------------------------------
    # Stock Information Tab
    # --------------------------------------------------------------------------
    
    tabPanel(
      "Stock Information",
      
      fluidRow(
        column(
          width = 4,
          
          wellPanel(
            textInput("ticker", "Yahoo Finance Ticker", value = "RPI.L"),
            
            selectInput(
              "period",
              "Historical Horizon",
              choices = c(
                "1 Month" = "1 months",
                "3 Months" = "3 months",
                "6 Months" = "6 months",
                "1 Year" = "1 years"
              ),
              selected = "6 months"
            ),
            
            actionButton("refresh", "Force Refresh Data", class = "btn-primary"),
            
            hr(),
            
            helpText(
              "Default 'RPI.L' maps to Raspberry Pi Holdings plc on the London Stock Exchange."
            )
          )
        ),
        
        column(
          width = 8,
          h3("Price Evolution"),
          plotOutput("pricePlot", height = "400px"),
          
          h3("Latest Session Breakdown"),
          verbatimTextOutput("latestInfo"),
          
          h3("Historical Records"),
          DTOutput("priceTable")
        )
      )
    ),
    
    
    # --------------------------------------------------------------------------
    # Inflation Tab
    # --------------------------------------------------------------------------
    
    tabPanel(
      "Inflation Data",
      
      h3("Inflation, Consumer Prices: Annual %"),
      p("Source: World Bank indicator FP.CPI.TOTL.ZG."),
      
      plotOutput("inflationLinePlot", height = "500px"),
      
      h4("Latest Inflation Values"),
      DTOutput("inflationLatestTable"),
      
      h4("Full Inflation Dataset"),
      DTOutput("inflationTable")
    ),
    
    
    # --------------------------------------------------------------------------
    # Unemployment Tab
    # --------------------------------------------------------------------------
    
    tabPanel(
      "Unemployment Data",
      
      h3("Unemployment, Total: % of Labour Force"),
      p("Source: World Bank indicator SL.UEM.TOTL.ZS. This is the modeled ILO estimate."),
      
      plotOutput("unemploymentLinePlot", height = "500px"),
      
      h4("Latest Unemployment Values"),
      DTOutput("unemploymentLatestTable"),
      
      h4("Full Unemployment Dataset"),
      DTOutput("unemploymentTable")
    ),
    
    
    # --------------------------------------------------------------------------
    # Fuel Price Tab
    # --------------------------------------------------------------------------
    
    tabPanel(
      "Fuel Price Data",
      
      h3("Brent Crude Oil Spot Price"),
      p("Source: FRED series DCOILBRENTEU."),
      
      div(
        style = paste0(
          "font-size: 38px; font-weight: bold; padding: 15px; ",
          "margin-bottom: 25px; background-color: #f8f9fa; ",
          "border-left: 5px solid #2ca02c; text-align: center;"
        ),
        textOutput("oilPriceLarge")
      ),
      
      plotOutput("brentSpotPlot", height = "500px"),
      DTOutput("brentSpotTable")
    ),
    
    
    # --------------------------------------------------------------------------
    # VIX Tab
    # --------------------------------------------------------------------------
    
    tabPanel(
      "VIX Data",
      
      h3("CBOE Volatility Index"),
      p("Source: Yahoo Finance ticker ^VIX."),
      
      div(
        style = paste0(
          "font-size: 38px; font-weight: bold; padding: 15px; ",
          "margin-bottom: 25px; background-color: #f8f9fa; ",
          "border-left: 5px solid #9467bd; text-align: center;"
        ),
        textOutput("vixLarge")
      ),
      
      plotOutput("vixPlot", height = "500px"),
      DTOutput("vixTable")
    ),
    
    
    # --------------------------------------------------------------------------
    # National Debt Tab
    # --------------------------------------------------------------------------
    
    tabPanel(
      "National Debt Data",
      
      h3("Central Government Debt: % of GDP"),
      p(
        "Source: World Bank indicator GC.DOD.TOTL.GD.ZS. ",
        "This is central government debt, not necessarily full general government debt."
      ),
      
      plotOutput("debtLinePlot", height = "500px"),
      
      h4("Latest Debt Values"),
      DTOutput("debtLatestTable"),
      
      h4("Full Debt Dataset"),
      DTOutput("debtTable")
    ),
    
    
    # --------------------------------------------------------------------------
    # Bitcoin Tab
    # --------------------------------------------------------------------------
    
    tabPanel(
      "Bitcoin Data",
      
      h3("Bitcoin Spot Value"),
      p("Source: Yahoo Finance ticker BTC-USD."),
      
      div(
        style = paste0(
          "font-size: 38px; font-weight: bold; padding: 15px; ",
          "margin-bottom: 25px; background-color: #f8f9fa; ",
          "border-left: 5px solid #ff7f0e; text-align: center;"
        ),
        textOutput("bitcoinLarge")
      ),
      
      plotOutput("bitcoinPlot", height = "500px"),
      DTOutput("bitcoinTable")
    ),
    
    
    # --------------------------------------------------------------------------
    # Gold and Silver Tab
    # --------------------------------------------------------------------------
    
    tabPanel(
      "Gold and Silver Data",
      
      h3("Precious Metals"),
      p("Source: Yahoo Finance futures tickers GC=F and SI=F."),
      
      h4("Gold Futures"),
      plotOutput("goldPlot", height = "450px"),
      
      h4("Silver Futures"),
      plotOutput("silverPlot", height = "450px"),
      
      h4("Recent Precious Metals Data"),
      DTOutput("metalsTable")
    ),
    
    
    # --------------------------------------------------------------------------
    # UK Claimant Count Tab
    # --------------------------------------------------------------------------
    
    tabPanel(
      "UK Claimant Count",
      
      h3("UK Benefit Claimant Count"),
      
      p(
        "This tab shows the UK Claimant Count from the Office for National Statistics. ",
        "It measures people claiming unemployment-related benefits. The series used ",
        "here is seasonally adjusted and measured in thousands."
      ),
      
      p(
        strong("Source: "),
        "ONS time series BCJD, Claimant Count: K02000001 UK: People: SA: Thousands."
      ),
      
      div(
        style = paste0(
          "font-size: 34px; font-weight: bold; padding: 15px; ",
          "margin-bottom: 25px; background-color: #f8f9fa; ",
          "border-left: 5px solid #17becf; text-align: center;"
        ),
        textOutput("claimantCountLarge")
      ),
      
      h4("UK Claimant Count Trend"),
      plotOutput("claimantCountPlot", height = "500px"),
      
      h4("Recent UK Claimant Count Records"),
      DTOutput("claimantCountTable")
    ),
    
    
    # --------------------------------------------------------------------------
    # Financial News Feed Tab
    # --------------------------------------------------------------------------
    
    tabPanel(
      "Financial News Feed",
      
      h3(textOutput("newsHeader")),
      p("Live financial news parsed from Yahoo Finance RSS for the selected ticker."),
      hr(),
      DTOutput("newsTable")
    )
  )
)


# ==============================================================================
# Server Logic
# ==============================================================================
# The server defines how the app fetches data and renders plots/tables.
# ==============================================================================

server <- function(input, output, session) {
  
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  
  # ============================================================================
  # Market Data Reactives
  # ============================================================================
  # Reactives are cached until their dependencies change.
  # For example, stock_data() changes when the refresh button is clicked.
  # ============================================================================
  
  stock_data <- eventReactive(
    input$refresh,
    {
      req(input$ticker)
      
      from_date <- switch(
        input$period,
        "1 months" = Sys.Date() - 31,
        "3 months" = Sys.Date() - 93,
        "6 months" = Sys.Date() - 186,
        "1 years" = Sys.Date() - 365,
        Sys.Date() - 186
      )
      
      safe_get_symbols(input$ticker, src = "yahoo", from = from_date)
    },
    ignoreNULL = FALSE
  )
  
  
  brent_spot_data <- reactive({
    safe_get_symbols(
      "DCOILBRENTEU",
      src = "FRED",
      from = Sys.Date() - round(365.25 * 20)
    )
  })
  
  
  vix_data <- reactive({
    safe_get_symbols(
      "^VIX",
      src = "yahoo",
      from = Sys.Date() - round(365.25 * 20)
    )
  })
  
  
  bitcoin_data <- reactive({
    safe_get_symbols(
      "BTC-USD",
      src = "yahoo",
      from = Sys.Date() - round(365.25 * 3)
    )
  })
  
  
  gold_data <- reactive({
    safe_get_symbols(
      "GC=F",
      src = "yahoo",
      from = Sys.Date() - round(365.25 * 3)
    )
  })
  
  
  silver_data <- reactive({
    safe_get_symbols(
      "SI=F",
      src = "yahoo",
      from = Sys.Date() - round(365.25 * 3)
    )
  })
  
  
  # ============================================================================
  # UK Claimant Count Reactive
  # ============================================================================
  # This downloads the ONS BCJD claimant count time series.
  # ============================================================================
  
  uk_claimant_count_data <- reactive({
    fetch_uk_claimant_count()
  })
  
  
  # ============================================================================
  # World Bank Data Reactives
  # ============================================================================
  
  inflation_df <- reactive({
    fetch_world_bank_indicator(
      indicator = "FP.CPI.TOTL.ZG",
      start_year = current_year - 10,
      end_year = current_year
    )
  })
  
  
  unemployment_df <- reactive({
    fetch_world_bank_indicator(
      indicator = "SL.UEM.TOTL.ZS",
      start_year = current_year - 10,
      end_year = current_year
    )
  })
  
  
  debt_df <- reactive({
    fetch_world_bank_indicator(
      indicator = "GC.DOD.TOTL.GD.ZS",
      start_year = current_year - 20,
      end_year = current_year
    )
  })
  
  
  # ============================================================================
  # News Feed Reactive
  # ============================================================================
  # This fetches a Yahoo Finance RSS feed for the selected ticker.
  # It forces all fields to character to avoid DT rendering errors.
  # ============================================================================
  
  news_data <- reactive({
    req(input$ticker)
    
    input$refresh
    
    rss_url <- paste0(
      "https://feeds.finance.yahoo.com/rss/2.0/headline?s=",
      URLencode(input$ticker, reserved = TRUE),
      "&region=US&lang=en-US"
    )
    
    tryCatch(
      {
        xml_stream <- read_xml(rss_url)
        items <- xml_find_all(xml_stream, ".//item")
        
        if (length(items) == 0) {
          return(data.frame(
            Published = character(),
            Headlines = character(),
            Link = character(),
            stringsAsFactors = FALSE
          ))
        }
        
        titles <- as.character(
          xml_text(xml_find_first(items, ".//title"), trim = TRUE)
        )
        
        links <- as.character(
          xml_text(xml_find_first(items, ".//link"), trim = TRUE)
        )
        
        dates <- as.character(
          xml_text(xml_find_first(items, ".//pubDate"), trim = TRUE)
        )
        
        n <- min(length(titles), length(links), length(dates))
        
        if (n == 0) {
          return(data.frame(
            Published = character(),
            Headlines = character(),
            Link = character(),
            stringsAsFactors = FALSE
          ))
        }
        
        data.frame(
          Published = dates[seq_len(n)],
          Headlines = titles[seq_len(n)],
          Link = paste0(
            "<a href='",
            links[seq_len(n)],
            "' target='_blank'>View Source Report</a>"
          ),
          stringsAsFactors = FALSE
        )
      },
      error = function(e) {
        data.frame(
          Published = as.character("Connection error"),
          Headlines = as.character(
            "RSS data feed could not be loaded. Check the ticker, network connection, or Yahoo Finance feed availability."
          ),
          Link = as.character(""),
          stringsAsFactors = FALSE
        )
      }
    )
  })
  
  
  # ============================================================================
  # Home Tab
  # ============================================================================
  
  output$homeMarketSummary <- renderUI({
    
    stock <- stock_data()
    oil <- brent_spot_data()
    vix <- vix_data()
    bitcoin <- bitcoin_data()
    gold <- gold_data()
    silver <- silver_data()
    claimant_count <- uk_claimant_count_data()
    
    # Reusable small summary box.
    make_box <- function(title, value, subtitle, border_colour = "#0b5ed7") {
      column(
        width = 4,
        div(
          style = paste0(
            "background-color: #f8f9fa;",
            "border: 1px solid #dee2e6;",
            "border-left: 5px solid ", border_colour, ";",
            "border-radius: 8px;",
            "padding: 15px;",
            "margin-bottom: 15px;",
            "min-height: 135px;"
          ),
          h4(title, style = "margin-top: 0;"),
          div(value, style = "font-size: 26px; font-weight: bold; margin-bottom: 8px;"),
          div(subtitle, style = "font-size: 12px; color: #6c757d;")
        )
      )
    }
    
    stock_value <- if (!is.null(stock)) {
      round(as.numeric(Cl(tail(stock, 1))), 2)
    } else {
      "Unavailable"
    }
    
    stock_date <- if (!is.null(stock)) {
      paste("As of", as.character(index(tail(stock, 1))))
    } else {
      "Check ticker or connection"
    }
    
    oil_value <- if (!is.null(oil)) {
      paste0("$", round(as.numeric(tail(oil, 1)[, 1]), 2), " / bbl")
    } else {
      "Unavailable"
    }
    
    oil_date <- if (!is.null(oil)) {
      paste("As of", as.character(index(tail(oil, 1))))
    } else {
      "FRED data unavailable"
    }
    
    vix_value <- if (!is.null(vix)) {
      round(as.numeric(Cl(tail(vix, 1))), 2)
    } else {
      "Unavailable"
    }
    
    vix_date <- if (!is.null(vix)) {
      paste("As of", as.character(index(tail(vix, 1))))
    } else {
      "Yahoo Finance data unavailable"
    }
    
    bitcoin_value <- if (!is.null(bitcoin)) {
      paste0("$", format(round(as.numeric(Cl(tail(bitcoin, 1))), 2), big.mark = ","))
    } else {
      "Unavailable"
    }
    
    bitcoin_date <- if (!is.null(bitcoin)) {
      paste("As of", as.character(index(tail(bitcoin, 1))))
    } else {
      "Yahoo Finance data unavailable"
    }
    
    gold_value <- if (!is.null(gold)) {
      paste0("$", format(round(as.numeric(Cl(tail(gold, 1))), 2), big.mark = ","))
    } else {
      "Unavailable"
    }
    
    gold_date <- if (!is.null(gold)) {
      paste("As of", as.character(index(tail(gold, 1))))
    } else {
      "Yahoo Finance data unavailable"
    }
    
    silver_value <- if (!is.null(silver)) {
      paste0("$", round(as.numeric(Cl(tail(silver, 1))), 2))
    } else {
      "Unavailable"
    }
    
    silver_date <- if (!is.null(silver)) {
      paste("As of", as.character(index(tail(silver, 1))))
    } else {
      "Yahoo Finance data unavailable"
    }
    
    claimant_count_value <- if (nrow(claimant_count) > 0) {
      paste0(
        format(
          round(tail(claimant_count, 1)$ClaimantCountThousands, 1),
          big.mark = ","
        ),
        " thousand"
      )
    } else {
      "Unavailable"
    }
    
    claimant_count_date <- if (nrow(claimant_count) > 0) {
      paste("Period", tail(claimant_count, 1)$Label)
    } else {
      "ONS data unavailable"
    }
    
    tagList(
      fluidRow(
        make_box(paste("Selected Stock:", input$ticker), stock_value, stock_date, "#1f77b4"),
        make_box("Brent Crude Oil", oil_value, oil_date, "#2ca02c"),
        make_box("VIX Volatility Index", vix_value, vix_date, "#9467bd")
      ),
      fluidRow(
        make_box("Bitcoin", bitcoin_value, bitcoin_date, "#ff7f0e"),
        make_box("Gold Futures", gold_value, gold_date, "#d62728"),
        make_box("Silver Futures", silver_value, silver_date, "grey40")
      ),
      fluidRow(
        make_box("UK Claimant Count", claimant_count_value, claimant_count_date, "#17becf")
      )
    )
  })
  
  
  output$homeMacroSummaryTable <- renderDT({
    
    inflation_latest <- latest_by_country(inflation_df(), "Inflation Rate (%)")
    unemployment_latest <- latest_by_country(unemployment_df(), "Unemployment Rate (%)")
    debt_latest <- latest_by_country(debt_df(), "Central Government Debt (% GDP)")
    
    shiny::validate(
      shiny::need(nrow(inflation_latest) > 0, "World Bank macroeconomic data could not be loaded.")
    )
    
    summary_df <- merge(
      inflation_latest,
      unemployment_latest,
      by = "Country",
      all = TRUE
    )
    
    summary_df <- merge(
      summary_df,
      debt_latest,
      by = "Country",
      all = TRUE
    )
    
    datatable(
      summary_df,
      options = list(pageLength = 10, searching = FALSE),
      rownames = FALSE
    )
  })
  
  
  # ============================================================================
  # Stock Information Tab
  # ============================================================================
  
  output$pricePlot <- renderPlot({
    data <- stock_data()
    
    shiny::validate(
      shiny::need(!is.null(data), "Ticker unreachable. Check the ticker symbol or network connection.")
    )
    
    plot(
      x = index(data),
      y = as.numeric(Cl(data)),
      type = "l",
      main = paste("Closing Price:", input$ticker),
      ylab = "Close Price",
      xlab = "Date",
      col = "#1f77b4",
      lwd = 2
    )
    
    grid(nx = NULL, ny = NULL, lty = 2, col = "gray85")
  })
  
  
  output$latestInfo <- renderPrint({
    data <- stock_data()
    
    shiny::validate(
      shiny::need(!is.null(data), "Data has not loaded.")
    )
    
    latest <- tail(data, 1)
    
    cat("Ticker symbol:       ", input$ticker, "\n")
    cat("Latest date:         ", as.character(index(latest)), "\n")
    cat("Open:                ", round(as.numeric(Op(latest)), 2), "\n")
    cat("High:                ", round(as.numeric(Hi(latest)), 2), "\n")
    cat("Low:                 ", round(as.numeric(Lo(latest)), 2), "\n")
    cat("Close:               ", round(as.numeric(Cl(latest)), 2), "\n")
    cat("Volume:              ", format(as.numeric(Vo(latest)), big.mark = ","), "\n")
  })
  
  
  output$priceTable <- renderDT({
    data <- stock_data()
    
    shiny::validate(
      shiny::need(!is.null(data), "No tabular data available.")
    )
    
    datatable(
      tail(make_price_df(data, include_volume = TRUE), 30),
      options = list(pageLength = 10, order = list(list(0, "desc"))),
      rownames = FALSE
    )
  })
  
  
  # ============================================================================
  # Inflation Tab
  # ============================================================================
  
  output$inflationLinePlot <- renderPlot({
    plot_macro_series(
      inflation_df(),
      "Inflation, Consumer Prices: Annual %",
      "Annual inflation (%)"
    )
  })
  
  
  output$inflationLatestTable <- renderDT({
    datatable(
      latest_by_country(inflation_df(), "Inflation Rate (%)"),
      options = list(pageLength = 10, searching = FALSE),
      rownames = FALSE
    )
  })
  
  
  output$inflationTable <- renderDT({
    datatable(
      inflation_df(),
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  
  # ============================================================================
  # Unemployment Tab
  # ============================================================================
  
  output$unemploymentLinePlot <- renderPlot({
    plot_macro_series(
      unemployment_df(),
      "Unemployment, Total: % of Labour Force",
      "Unemployment rate (%)"
    )
  })
  
  
  output$unemploymentLatestTable <- renderDT({
    datatable(
      latest_by_country(unemployment_df(), "Unemployment Rate (%)"),
      options = list(pageLength = 10, searching = FALSE),
      rownames = FALSE
    )
  })
  
  
  output$unemploymentTable <- renderDT({
    datatable(
      unemployment_df(),
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  
  # ============================================================================
  # Fuel Price Tab
  # ============================================================================
  
  output$oilPriceLarge <- renderText({
    data <- brent_spot_data()
    
    shiny::validate(
      shiny::need(!is.null(data), "FRED Brent crude oil data could not be loaded.")
    )
    
    latest <- tail(data, 1)
    
    paste0(
      "Latest Brent Spot Price: $",
      round(as.numeric(latest[, 1]), 2),
      " USD / bbl — As of: ",
      as.character(index(latest))
    )
  })
  
  
  output$brentSpotPlot <- renderPlot({
    data <- brent_spot_data()
    
    shiny::validate(
      shiny::need(!is.null(data), "Brent crude oil data could not be loaded.")
    )
    
    plot(
      x = index(data),
      y = as.numeric(data[, 1]),
      type = "l",
      col = "#2ca02c",
      lwd = 1.5,
      main = "Brent Crude Oil Spot Price: 20-Year View",
      xlab = "Date",
      ylab = "USD per barrel",
      las = 1
    )
    
    grid(nx = NULL, ny = NULL, lty = 2, col = "gray85")
  })
  
  
  output$brentSpotTable <- renderDT({
    data <- brent_spot_data()
    
    shiny::validate(
      shiny::need(!is.null(data), "Brent crude oil table could not be loaded.")
    )
    
    df <- data.frame(
      Date = index(data),
      BrentSpotPriceUSD = as.numeric(data[, 1])
    )
    
    datatable(
      tail(df, 30),
      options = list(pageLength = 10, order = list(list(0, "desc"))),
      rownames = FALSE
    )
  })
  
  
  # ============================================================================
  # VIX Tab
  # ============================================================================
  
  output$vixLarge <- renderText({
    data <- vix_data()
    
    shiny::validate(
      shiny::need(!is.null(data), "VIX data could not be loaded.")
    )
    
    latest <- tail(data, 1)
    
    paste0(
      "Latest VIX Level: ",
      round(as.numeric(Cl(latest)), 2),
      " — As of: ",
      as.character(index(latest))
    )
  })
  
  
  output$vixPlot <- renderPlot({
    data <- vix_data()
    
    shiny::validate(
      shiny::need(!is.null(data), "VIX data could not be loaded.")
    )
    
    plot(
      x = index(data),
      y = as.numeric(Cl(data)),
      type = "l",
      col = "#9467bd",
      lwd = 1.5,
      main = "VIX Index: 20-Year View",
      xlab = "Date",
      ylab = "VIX Level",
      las = 1
    )
    
    grid(nx = NULL, ny = NULL, lty = 2, col = "gray85")
  })
  
  
  output$vixTable <- renderDT({
    data <- vix_data()
    
    shiny::validate(
      shiny::need(!is.null(data), "VIX table could not be loaded.")
    )
    
    datatable(
      tail(make_price_df(data, include_volume = FALSE), 30),
      options = list(pageLength = 10, order = list(list(0, "desc"))),
      rownames = FALSE
    )
  })
  
  
  # ============================================================================
  # National Debt Tab
  # ============================================================================
  
  output$debtLinePlot <- renderPlot({
    plot_macro_series(
      debt_df(),
      "Central Government Debt: % of GDP",
      "Debt to GDP (%)"
    )
  })
  
  
  output$debtLatestTable <- renderDT({
    datatable(
      latest_by_country(debt_df(), "Central Government Debt (% GDP)"),
      options = list(pageLength = 10, searching = FALSE),
      rownames = FALSE
    )
  })
  
  
  output$debtTable <- renderDT({
    datatable(
      debt_df(),
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  
  # ============================================================================
  # Bitcoin Tab
  # ============================================================================
  
  output$bitcoinLarge <- renderText({
    data <- bitcoin_data()
    
    shiny::validate(
      shiny::need(!is.null(data), "Bitcoin data could not be loaded.")
    )
    
    latest <- tail(data, 1)
    
    paste0(
      "Bitcoin Spot Price: $",
      format(round(as.numeric(Cl(latest)), 2), big.mark = ","),
      " USD — As of: ",
      as.character(index(latest))
    )
  })
  
  
  output$bitcoinPlot <- renderPlot({
    data <- bitcoin_data()
    
    shiny::validate(
      shiny::need(!is.null(data), "Bitcoin price data could not be loaded.")
    )
    
    plot(
      x = index(data),
      y = as.numeric(Cl(data)),
      type = "l",
      col = "#ff7f0e",
      lwd = 1.8,
      main = "Bitcoin Price: Three-Year View",
      xlab = "Date",
      ylab = "USD",
      las = 1
    )
    
    grid(nx = NULL, ny = NULL, lty = 2, col = "gray85")
  })
  
  
  output$bitcoinTable <- renderDT({
    data <- bitcoin_data()
    
    shiny::validate(
      shiny::need(!is.null(data), "Bitcoin table could not be loaded.")
    )
    
    datatable(
      tail(make_price_df(data, include_volume = TRUE), 30),
      options = list(pageLength = 10, order = list(list(0, "desc"))),
      rownames = FALSE
    )
  })
  
  
  # ============================================================================
  # Gold and Silver Tab
  # ============================================================================
  
  output$goldPlot <- renderPlot({
    gold <- gold_data()
    
    shiny::validate(
      shiny::need(!is.null(gold), "Gold data could not be loaded.")
    )
    
    plot(
      x = index(gold),
      y = as.numeric(Cl(gold)),
      type = "l",
      col = "#d62728",
      lwd = 1.8,
      main = "Gold Futures Price: Three-Year View",
      xlab = "Date",
      ylab = "USD per troy ounce",
      las = 1
    )
    
    grid(nx = NULL, ny = NULL, lty = 2, col = "gray85")
  })
  
  
  output$silverPlot <- renderPlot({
    silver <- silver_data()
    
    shiny::validate(
      shiny::need(!is.null(silver), "Silver data could not be loaded.")
    )
    
    plot(
      x = index(silver),
      y = as.numeric(Cl(silver)),
      type = "l",
      col = "grey40",
      lwd = 1.8,
      main = "Silver Futures Price: Three-Year View",
      xlab = "Date",
      ylab = "USD per troy ounce",
      las = 1
    )
    
    grid(nx = NULL, ny = NULL, lty = 2, col = "gray85")
  })
  
  
  output$metalsTable <- renderDT({
    gold <- gold_data()
    silver <- silver_data()
    
    shiny::validate(
      shiny::need(!is.null(gold) && !is.null(silver), "Precious metals data could not be loaded.")
    )
    
    gold_df <- data.frame(
      Date = index(gold),
      GoldCloseUSD = as.numeric(Cl(gold))
    )
    
    silver_df <- data.frame(
      Date = index(silver),
      SilverCloseUSD = as.numeric(Cl(silver))
    )
    
    metals_df <- merge(gold_df, silver_df, by = "Date", all = FALSE)
    
    datatable(
      tail(metals_df, 30),
      options = list(pageLength = 10, order = list(list(0, "desc"))),
      rownames = FALSE
    )
  })
  
  
  # ============================================================================
  # UK Claimant Count Tab
  # ============================================================================
  
  output$claimantCountLarge <- renderText({
    data <- uk_claimant_count_data()
    
    shiny::validate(
      shiny::need(nrow(data) > 0, "UK claimant count data could not be loaded from ONS.")
    )
    
    latest <- tail(data, 1)
    
    paste0(
      "Latest UK Claimant Count: ",
      format(round(latest$ClaimantCountThousands, 1), big.mark = ","),
      " thousand — Period: ",
      latest$Label
    )
  })
  
  
  output$claimantCountPlot <- renderPlot({
    data <- uk_claimant_count_data()
    
    shiny::validate(
      shiny::need(nrow(data) > 0, "UK claimant count data could not be loaded from ONS.")
    )
    
    # Show the latest 10 years of monthly data if enough data exists.
    plot_data <- tail(data, 120)
    
    plot(
      x = plot_data$Sequence,
      y = plot_data$ClaimantCountThousands,
      type = "l",
      col = "#17becf",
      lwd = 1.8,
      main = "UK Claimant Count, Seasonally Adjusted",
      xlab = "Recent monthly periods",
      ylab = "Claimant count, thousands",
      las = 1
    )
    
    grid(nx = NULL, ny = NULL, lty = 2, col = "gray85")
  })
  
  
  output$claimantCountTable <- renderDT({
    data <- uk_claimant_count_data()
    
    shiny::validate(
      shiny::need(nrow(data) > 0, "UK claimant count data could not be loaded from ONS.")
    )
    
    datatable(
      tail(data[, c("Period", "Label", "ClaimantCountThousands")], 120),
      options = list(
        pageLength = 25,
        order = list(list(0, "desc"))
      ),
      rownames = FALSE
    ) |>
      formatRound(
        columns = "ClaimantCountThousands",
        digits = 1
      )
  })
  
  
  # ============================================================================
  # Financial News Feed Tab
  # ============================================================================
  
  output$newsHeader <- renderText({
    paste("Live Corporate and Valuation Developments:", input$ticker)
  })
  
  
  output$newsTable <- renderDT({
    df <- news_data()
    
    shiny::validate(
      shiny::need(is.data.frame(df), "News data could not be converted into a table."),
      shiny::need(nrow(df) > 0, "No news items found for this ticker.")
    )
    
    # Force all values to character to prevent:
    # Error: is.character(txt) is not TRUE
    df[] <- lapply(df, as.character)
    
    datatable(
      df,
      escape = FALSE,
      options = list(
        pageLength = 10,
        searchHighlight = TRUE,
        order = list(list(0, "desc"))
      ),
      rownames = FALSE
    )
  })
}


# ==============================================================================
# Launch App
# ==============================================================================

shinyApp(ui = ui, server = server)