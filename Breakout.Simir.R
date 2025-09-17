library(shiny)
library(ggplot2)
library(dplyr)
library(TTR)
library(scales)
library(httr)
library(jsonlite)

# UI часть
ui <- fluidPage(
  titlePanel("Interactive Trading Strategy - Channel Breakout"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Режим работы
      radioButtons("mode", "Data Mode:",
                   choices = c("Simulated Data" = "sim", "Real MOEX Data" = "real"),
                   selected = "sim"),
      
      # Параметры для симуляции
      conditionalPanel(
        condition = "input.mode == 'sim'",
        sliderInput("seed", "Simulation Seed:", 
                   min = 1, max = 1000, value = 123, step = 1),
        sliderInput("n_days", "Simulation Days:", 
                   min = 50, max = 500, value = 200, step = 50)
      ),
      
      # Параметры для реальных данных
      conditionalPanel(
        condition = "input.mode == 'real'",
        selectInput("ticker", "MOEX Ticker:", 
                   choices = c("SBER", "GAZP", "LKOH", "ROSN", "VTBR", "YNDX", "MGNT"),
                   selected = "SBER"),
        dateRangeInput("date_range", "Data Period:",
                      start = Sys.Date() - 365,
                      end = Sys.Date())
      ),
      
      # Параметры стратегии
      sliderInput("length", "Channel Length:", 
                 min = 10, max = 100, value = 20, step = 5),
      sliderInput("sma_period", "SMA Period:", 
                 min = 5, max = 50, value = 20, step = 5),
      sliderInput("risk_percent", "Risk per Trade (%):", 
                 min = 1, max = 50, value = 20, step = 1),
      sliderInput("channel_growth", "Channel Growth for Entry (%):", 
                 min = 1, max = 10, value = 3, step = 0.5),
      sliderInput("entry_offset", "Order Offset (% from channel):", 
                 min = 1, max = 10, value = 5, step = 0.5),
      
      actionButton("run", "Run Backtest", class = "btn-primary")
    ),
    
    mainPanel(
      width = 9,
      plotOutput("strategy_plot", height = "400px"),
      plotOutput("equity_plot", height = "200px"),
      br(),
      h4("Backtest Results:"),
      tableOutput("results_table"),
      br(),
      h4("Trade Details:"),
      div(style = 'overflow-x: scroll; height: 300px;',
          tableOutput("trades_table"))
    )
  )
)

# Server часть
server <- function(input, output, session) {
  
  # Функция для получения реальных данных с MOEX
  getMOEX <- function(ticker, start_date, end_date) {
    tryCatch({
      url <- paste0('https://iss.moex.com/iss/engines/stock/markets/shares/securities/', 
                   ticker, '/candles.json?from=', start_date, 
                   '&till=', end_date, '&interval=24')
      response <- GET(url)
      ticker_data <- fromJSON(content(response, "text"))
      
      if (!is.null(ticker_data$candles$data)) {
        test <- as.data.frame(ticker_data$candles$data)
        colnames(test) <- ticker_data[["candles"]][["columns"]]
        
        # Преобразование данных
        numeric_cols <- c("open", "high", "low", "close", "volume")
        for(col in numeric_cols) {
          if(col %in% colnames(test)) {
            test[[col]] <- as.numeric(as.character(test[[col]]))
          }
        }
        
        test$end <- as.Date(test$end)
        test <- test[order(test$end), ]
        
        return(test)
      } else {
        stop("Failed to get data")
      }
    }, error = function(e) {
      # Возвращаем симуляционные данные в случае ошибки
      warning("MOEX data error: ", e$message)
      return(simulate_data(input$seed, 100))
    })
  }
  
  # Функция для симуляции данных
  simulate_data <- function(seed, n_days) {
    set.seed(seed)
    dates <- seq.Date(Sys.Date() - n_days, Sys.Date(), by = "day")
    prices <- cumprod(c(100, 1 + rnorm(n_days, 0.001, 0.02)))
    
    data <- data.frame(
      end = dates,
      open = prices * 0.99,
      high = prices * 1.02,
      low = prices * 0.98,
      close = prices,
      volume = sample(10000:50000, n_days + 1, replace = TRUE)
    )
    
    return(data)
  }
  
  # Reactive function for data and calculations
  strategy_data <- eventReactive(input$run, {
    # Parameters
    initial_capital <- 100000
    length_val <- input$length
    sma_period_val <- input$sma_period
    risk_percent_val <- input$risk_percent / 100
    channel_growth_val <- input$channel_growth / 100
    entry_offset_val <- input$entry_offset / 100
    
    # Get data based on mode
    if (input$mode == "sim") {
      data <- simulate_data(input$seed, input$n_days)
      data_source <- "Simulation"
    } else {
      data <- getMOEX(input$ticker, input$date_range[1], input$date_range[2])
      data_source <- paste("MOEX", input$ticker)
    }
    
    # Calculate indicators
    data$ema1 <- TTR::EMA(data$close, sma_period_val)
    data$upperW <- TTR::runMax(data$close, length_val * 2)
    data$lowerW <- TTR::runMin(data$close, length_val * 2)
    data <- data[complete.cases(data), ]
    
    # Initialize variables
    capital <- initial_capital
    position <- 0
    entry_price <- 0
    entry_date <- NULL
    pending_order <- FALSE
    pending_price <- 0
    trades <- data.frame()
    equity <- rep(initial_capital, nrow(data))
    signals <- rep("No signal", nrow(data))
    
    # Main trading loop
    for(i in 2:nrow(data)) {
      current <- data[i, ]
      prev <- data[i-1, ]
      
      # Entry condition
      if(!is.na(prev$upperW) && !is.na(current$upperW)) {
        channel_growth <- (current$upperW - prev$upperW) / prev$upperW
        buySignal <- channel_growth >= channel_growth_val
      } else {
        buySignal <- FALSE
      }
      
      order_price <- current$upperW * (1 + entry_offset_val)
      exitBuy <- current$close < current$ema1
      
      # Process pending order
      if(pending_order && current$high >= pending_price) {
        risk_amount <- capital * risk_percent_val
        position_size <- risk_amount / pending_price
        
        position <- floor(position_size)
        entry_price <- pending_price
        entry_date <- current$end
        capital <- capital - (position * entry_price)
        
        trades <- rbind(trades, data.frame(
          EntryDate = entry_date,
          ExitDate = NA,
          EntryPrice = entry_price,
          ExitPrice = NA,
          Shares = position,
          Type = "BUY",
          Reason = "Order Executed",
          stringsAsFactors = FALSE
        ))
        
        signals[i] <- "Order Executed"
        pending_order <- FALSE
      }
      
      # Process open position
      if(position > 0) {
        equity[i] <- capital + (position * current$close)
        
        if(exitBuy) {
          capital <- capital + (position * current$close)
          profit <- position * (current$close - entry_price)
          
          trades <- rbind(trades, data.frame(
            EntryDate = entry_date,
            ExitDate = current$end,
            EntryPrice = entry_price,
            ExitPrice = current$close,
            Shares = position,
            Type = "SELL",
            Reason = "Exit Signal",
            stringsAsFactors = FALSE
          ))
          
          position <- 0
          signals[i] <- "Exit"
        }
      } else if(buySignal && !pending_order) {
        pending_order <- TRUE
        pending_price <- order_price
        signals[i] <- "Order Placed"
      }
      
      if(!pending_order && position == 0) {
        equity[i] <- capital
      }
    }
    
    # Close any open position at the end
    if (position > 0) {
      last_price <- tail(data$close, 1)
      capital <- capital + (position * last_price)
      equity[length(equity)] <- capital
      
      if (nrow(trades) > 0) {
        last_trade <- which(is.na(trades$ExitDate) & trades$Type == "BUY")
        if (length(last_trade) > 0) {
          trades$ExitDate[last_trade] <- tail(data$end, 1)
          trades$ExitPrice[last_trade] <- last_price
          trades$Type[last_trade] <- "SELL"
          trades$Reason[last_trade] <- "End of Period"
        }
      }
    }
    
    # Calculate trade results
    if(nrow(trades) > 0) {
      trades$Profit <- ifelse(trades$Type == "SELL", 
                             trades$Shares * (trades$ExitPrice - trades$EntryPrice),
                             NA)
      trades$ReturnPct <- ifelse(trades$Type == "SELL",
                                round((trades$ExitPrice - trades$EntryPrice) / trades$EntryPrice * 100, 2),
                                NA)
    }
    
    # Return results
    list(
      data = data,
      trades = trades,
      equity = equity,
      signals = signals,
      final_capital = tail(equity, 1),
      data_source = data_source
    )
  })
  
  # Strategy plot
  output$strategy_plot <- renderPlot({
    result <- strategy_data()
    data <- result$data
    
    plot_data <- data
    plot_data$signal <- result$signals[1:nrow(data)]
    
    ggplot(plot_data, aes(x = end)) +
      geom_line(aes(y = close, color = "Price"), size = 1) +
      geom_line(aes(y = upperW, color = "Upper Channel"), size = 0.8, alpha = 0.7) +
      geom_line(aes(y = ema1, color = "SMA"), size = 0.8, linetype = "dashed") +
      geom_point(data = subset(plot_data, signal != "No signal"), 
                 aes(y = close, color = signal), size = 3) +
      scale_color_manual(values = c(
        "Price" = "black", 
        "Upper Channel" = "red", 
        "SMA" = "blue",
        "Order Placed" = "orange",
        "Order Executed" = "green",
        "Exit" = "purple"
      )) +
      labs(title = paste("Trading Strategy -", result$data_source),
           subtitle = paste("Channel:", input$length, "days | SMA:", input$sma_period),
           x = "Date", y = "Price") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Equity plot
  output$equity_plot <- renderPlot({
    result <- strategy_data()
    data <- result$data
    
    equity_df <- data.frame(
      Date = data$end,
      Equity = result$equity[1:nrow(data)]
    )
    
    ggplot(equity_df, aes(x = Date, y = Equity)) +
      geom_line(color = "blue", size = 1) +
      labs(title = "Equity Curve", x = "Date", y = "Capital") +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma)
  })
  
  # Results table
  output$results_table <- renderTable({
    result <- strategy_data()
    trades <- result$trades
    
    if(nrow(trades) > 0) {
      executed_trades <- trades[trades$Type == "SELL", ]
      
      if(nrow(executed_trades) > 0) {
        total_trades <- nrow(executed_trades)
        profitable_trades <- sum(executed_trades$Profit > 0, na.rm = TRUE)
        total_profit <- sum(executed_trades$Profit, na.rm = TRUE)
        max_profit <- max(executed_trades$Profit, na.rm = TRUE)
        max_loss <- min(executed_trades$Profit, na.rm = TRUE)
        win_rate <- profitable_trades / total_trades
        
        data.frame(
          Metric = c("Data Source", "Total Trades", "Profitable Trades", 
                    "Win Rate", "Total Profit", 
                    "Max Profit", "Max Loss", "Final Capital"),
          Value = c(result$data_source,
                    total_trades, 
                    profitable_trades,
                    paste0(round(win_rate * 100, 1), "%"), 
                    round(total_profit, 2),
                    round(max_profit, 2),
                    round(max_loss, 2),
                    round(result$final_capital, 2))
        )
      }
    }
  })
  
  # Trades table
  output$trades_table <- renderTable({
    result <- strategy_data()
    trades <- result$trades
    
    if(nrow(trades) > 0) {
      trades %>%
        select(EntryDate, ExitDate, Type, EntryPrice, ExitPrice, Shares, Profit, ReturnPct, Reason) %>%
        mutate(
          Profit = ifelse(is.na(Profit), "-", round(Profit, 2)),
          ReturnPct = ifelse(is.na(ReturnPct), "-", paste0(ReturnPct, "%")),
          EntryDate = as.character(EntryDate),
          ExitDate = as.character(ExitDate)
        ) %>%
        arrange(desc(EntryDate))
    }
  })
}

# Run the application
shinyApp(ui, server)
