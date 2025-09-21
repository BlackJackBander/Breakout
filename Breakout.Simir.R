library(shiny)
library(ggplot2)
library(dplyr)
library(TTR)
library(scales)
library(httr)
library(jsonlite)
library(purrr)
library(furrr)
library(gridExtra)
library(lubridate)

# Подключаем библиотеку для работы с данными MOEX
source("moex_data_lib.R")

# UI часть
ui <- fluidPage(
  titlePanel("📊 Interactive Trading Strategy - Channel Breakout"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Режим работы
      radioButtons("mode", "Режим работы:",
                   choices = c("Симуляция данных" = "sim", "Реальные данные MOEX" = "real"),
                   selected = "sim"),
      
      # Параметры для симуляции
      conditionalPanel(
        condition = "input.mode == 'sim'",
        sliderInput("seed", "Seed для симуляции:", 
                   min = 1, max = 1000, value = 123, step = 1),
        sliderInput("n_days", "Дней для симуляции:", 
                   min = 50, max = 500, value = 200, step = 50),
        dateRangeInput("sim_date_range", "Период тестирования:",
                      start = Sys.Date() - 365,
                      end = Sys.Date())
      ),
      
      # Параметры для реальных данных
      conditionalPanel(
        condition = "input.mode == 'real'",
        selectInput("ticker", "Тикер MOEX:", 
                   choices = c('X5','INGO','AKRN','CHMF','EUTR','FESH','GOLD','HEAD','LEAS','LNZL','RASP','MTSS','MVID','POSI','RKKE','SBER','SIBN','SMLT','T','TASB','TATN','TATNP','UTAR','TGKA','UPRO','VKCO','TRMK','BSPB','MTLR','ENPG','LKOH','GECO','PHOR','PLZL','SNGSP','YDEX','IRAO','SNGS','GAZP','AFLT','DIAS','VTBR','SBMX','SBGD','SBRB','RENI','ROSN','PIKK','WUSH','AFKS')
                   selected = "SBER"),
        actionButton("update_data", "🔄 Обновить данные"),
        checkboxInput("force_update", "Принудительное обновление", value = FALSE),
        dateRangeInput("real_date_range", "Период тестирования:",
                      start = as.Date("2020-01-01"),
                      end = Sys.Date())
      ),
      
      # Параметры стратегии
      sliderInput("length", "Длина канала:", 
                 min = 5, max = 100, value = 20, step = 5),
      sliderInput("sma_period", "Длина SMA:", 
                 min = 5, max = 100, value = 20, step = 5),
      sliderInput("risk_percent", "Риск на сделку (%):", 
                 min = 1, max = 100, value = 20, step = 1),
      sliderInput("channel_growth", "Рост канала для входа (%):", 
                 min = 1, max = 30, value = 3, step = 0.5),
      sliderInput("entry_offset", "Отступ для ордера (% от канала):", 
                 min = 1, max = 50, value = 5, step = 0.5),
      
      # Новая опция: Take Profit
      sliderInput("take_profit", "Take Profit (% от входа):", 
                 min = 0, max = 100, value = 0, step = 1),
      helpText("0% = отключено. При достижении Take Profit закрывается 90% позиции"),
      
      # Опция: включение/выключение отмены ордеров
      checkboxInput("enable_cancel", "Включить отмену ордеров", value = TRUE),
      helpText("При отключении: ордера не отменяются, даже если условие входа перестает выполняться"),
      
      actionButton("run", "🔄 Запустить тестирование", class = "btn-primary")
    ),
    
    mainPanel(
      width = 9,
      plotOutput("strategy_plot", height = "400px"),
      plotOutput("equity_plot", height = "200px"),
      br(),
      h4("📈 Результаты тестирования:"),
      tableOutput("results_table"),
      br(),
      h4("💼 Детали сделок:"),
      div(style = 'overflow-x: scroll; height: 300px;',
          tableOutput("trades_table"))
    )
  )
)

# Server часть
server <- function(input, output, session) {
  # Reactive value для хранения данных
  moex_data <- reactiveVal()
  strategy_data_value <- reactiveVal(NULL)
  
  # Автоматический пересчет при изменении параметров
  observe({
    # Список всех параметров, которые должны запускать пересчет
    input$length
    input$sma_period
    input$risk_percent
    input$channel_growth
    input$entry_offset
    input$enable_cancel
    input$take_profit
    input$mode
    input$seed
    input$n_days
    input$sim_date_range
    input$real_date_range
    input$ticker
    
    # Запускаем пересчет с небольшой задержкой для избежания множественных вычислений
    invalidateLater(500)
    isolate({
      if (!is.null(moex_data()) || input$mode == "sim") {
        calculate_strategy()
      }
    })
  })
  
  # Обработчик кнопки обновления данных
  observeEvent(input$update_data, {
    if (input$mode == "real") {
      showNotification("Загрузка данных MOEX...", type = "message")
      
      tryCatch({
        data <- loadMOEXData(input$ticker, input$force_update)
        moex_data(data)
        
        # Обновляем доступные даты для выбора периода
        if (!is.null(data) && nrow(data) > 0) {
          min_date <- min(data$end)
          max_date <- max(data$end)
          
          updateDateRangeInput(session, "real_date_range",
                              start = max(min_date, as.Date("2020-01-01")),
                              end = max_date,
                              min = min_date,
                              max = max_date)
        }
        
        showNotification("Данные успешно загружены!", type = "message")
        calculate_strategy()
      }, error = function(e) {
        showNotification(paste("Ошибка загрузки данных:", e$message), type = "error")
      })
    }
  })
  
  # Обработчик кнопки запуска тестирования
  observeEvent(input$run, {
    calculate_strategy()
  })
  
  # Функция для расчета стратегии
  calculate_strategy <- function() {
    # Parameters
    initial_capital <- 100000
    length_val <- input$length
    sma_period_val <- input$sma_period
    risk_percent_val <- input$risk_percent / 100
    channel_growth_val <- input$channel_growth / 100
    entry_offset_val <- input$entry_offset / 100
    enable_cancel <- input$enable_cancel
    take_profit_pct <- input$take_profit / 100  # Новая опция Take Profit
    
    # Get data based on mode
    if (input$mode == "sim") {
      data <- simulate_data(input$seed, input$n_days)
      # Фильтруем симуляционные данные по выбранному периоду
      data <- filterDataByDateRange(data, input$sim_date_range[1], input$sim_date_range[2])
      data_source <- "Симуляция"
    } else {
      # Используем заранее загруженные данные
      data <- moex_data()
      if (is.null(data)) {
        return(NULL)
      } else {
        # Фильтруем по выбранному периоду
        data <- filterDataByDateRange(data, input$real_date_range[1], input$real_date_range[2])
        data_source <- paste("MOEX", input$ticker)
      }
    }
    
    # Проверяем, что данные есть после фильтрации
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    # Calculate indicators
    data$upperW <- TTR::runMax(data$close, length_val * 2)
    data$lowerW <- TTR::runMin(data$close, length_val / 2)
    data$sma_exit <- TTR::SMA(data$close, sma_period_val)
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
    take_profit_triggered <- FALSE  # Флаг срабатывания Take Profit
    
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
      exitBuy <- current$close < current$sma_exit || current$close <= current$lowerW
      
      # Process pending order
      if(pending_order) {
        # Проверяем сработал ли ордер
        if (current$high >= pending_price) {
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
          pending_price <- 0
          take_profit_triggered <- FALSE  # Сброс флага при новом входе
        }
        
        # ЛОГИКА ОТМЕНЫ ОРДЕРОВ - зависит от настройки
        if (enable_cancel && !buySignal) {
          pending_order <- FALSE
          pending_price <- 0
          signals[i] <- "Order Canceled"
        }
      }
      
      # Process open position
      if(position > 0) {
        current_portfolio_value <- capital + (position * current$close)
        equity[i] <- current_portfolio_value
        
        # Проверка Take Profit (только если не сработал ранее и TP > 0)
        if (take_profit_pct > 0 && !take_profit_triggered) {
          take_profit_price <- entry_price * (1 + take_profit_pct)
          
          if (current$high >= take_profit_price) {
            # Закрываем 90% позиции по Take Profit
            shares_to_close <- floor(position * 0.9)
            profit <- shares_to_close * (take_profit_price - entry_price)
            
            # Обновляем капитал и позицию
            capital <- capital + (shares_to_close * take_profit_price)
            position <- position - shares_to_close
            
            # Записываем частичное закрытие
            trades <- rbind(trades, data.frame(
              EntryDate = entry_date,
              ExitDate = current$end,
              EntryPrice = entry_price,
              ExitPrice = take_profit_price,
              Shares = shares_to_close,
              Type = "SELL",
              Reason = "Take Profit (90%)",
              stringsAsFactors = FALSE
            ))
            
            take_profit_triggered <- TRUE
            signals[i] <- "Take Profit"
          }
        }
        
        # Выход по SMA (для оставшейся позиции)
        if(exitBuy && position > 0) {
          capital <- capital + (position * current$close)
          profit <- position * (current$close - entry_price)
          
          trades <- rbind(trades, data.frame(
            EntryDate = entry_date,
            ExitDate = current$end,
            EntryPrice = entry_price,
            ExitPrice = current$close,
            Shares = position,
            Type = "SELL",
            Reason = ifelse(take_profit_triggered, "Exit Signal (SMA) - Remainder", "Exit Signal (SMA)"),
            stringsAsFactors = FALSE
          ))
          
          position <- 0
          entry_price <- 0
          entry_date <- NULL
          take_profit_triggered <- FALSE
          signals[i] <- "Exit"
        }
      } else if(buySignal && !pending_order) {
        pending_order <- TRUE
        pending_price <- order_price
        signals[i] <- "Order Placed"
        
        trades <- rbind(trades, data.frame(
          EntryDate = current$end,
          ExitDate = NA,
          EntryPrice = order_price,
          ExitPrice = NA,
          Shares = NA,
          Type = "PENDING",
          Reason = "Pending Order",
          stringsAsFactors = FALSE
        ))
      }
      
      if(pending_order && position == 0) {
        equity[i] <- capital
      } else if(!pending_order && position == 0) {
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
      
      trades$EntryDate <- as.character(as.Date(trades$EntryDate))
      trades$ExitDate <- as.character(as.Date(trades$ExitDate))
    }
    
    # Сохраняем результаты
    result <- list(
      data = data,
      trades = trades,
      equity = equity,
      signals = signals,
      final_capital = tail(equity, 1),
      data_source = data_source,
      enable_cancel = enable_cancel,
      take_profit = input$take_profit
    )
    
    strategy_data_value(result)
  }
  
  # Функция для фильтрации данных по выбранному периоду
  filterDataByDateRange <- function(data, start_date, end_date) {
    if (!is.null(data) && nrow(data) > 0) {
      filtered_data <- data %>%
        filter(end >= as.Date(start_date) & end <= as.Date(end_date))
      return(filtered_data)
    }
    return(data)
  }
  
  # Strategy plot
  output$strategy_plot <- renderPlot({
    result <- strategy_data_value()
    if (is.null(result)) return(NULL)
    
    data <- result$data
    plot_data <- data
    plot_data$signal <- result$signals[1:nrow(data)]
    
    # Фильтруем сигналы в зависимости от настройки
    if (!result$enable_cancel) {
      plot_data$signal[plot_data$signal == "Order Canceled"] <- "No signal"
    }
    
    ggplot(plot_data, aes(x = end)) +
      geom_line(aes(y = close, color = "Price"), size = 1) +
      geom_line(aes(y = upperW, color = "Upper Channel"), size = 0.8, alpha = 0.7) +
      geom_line(aes(y = lowerW, color = "Upper Channel"), size = 0.8, alpha = 0.7) +
      geom_line(aes(y = sma_exit, color = "SMA (Exit)"), size = 0.8, linetype = "dashed") +
      geom_point(data = subset(plot_data, signal != "No signal"), 
                 aes(y = close, color = signal), size = 3) +
      scale_color_manual(values = c(
        "Price" = "black", 
        "Upper Channel" = "red", 
        "SMA (Exit)" = "blue",
        "Order Placed" = "orange",
        "Order Executed" = "green",
        "Exit" = "purple",
        "Take Profit" = "darkgreen",
        "Order Canceled" = "gray"
      )) +
      labs(title = paste("Торговая стратегия -", result$data_source),
           subtitle = paste("Канал:", input$length, "дней | SMA (выход):", input$sma_period,
                           "| Take Profit:", input$take_profit, "%",
                           "| Отмена ордеров:", ifelse(result$enable_cancel, "ВКЛ", "ВЫКЛ")),
           x = "Дата", y = "Цена") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  # Equity plot
  output$equity_plot <- renderPlot({
    result <- strategy_data_value()
    if (is.null(result)) return(NULL)
    
    data <- result$data
    equity_df <- data.frame(
      Date = data$end,
      Equity = result$equity[1:nrow(data)],
      Signal = result$signals[1:nrow(data)]
    )
    
    # Фильтруем сигналы в зависимости от настройки
    if (!result$enable_cancel) {
      equity_df$Signal[equity_df$Signal == "Order Canceled"] <- "No signal"
    }
    
    signal_points <- equity_df %>%
      filter(Signal != "No signal") %>%
      mutate(
        Color = case_when(
          Signal == "Order Placed" ~ "orange",
          Signal == "Order Executed" ~ "green",
          Signal == "Exit" ~ "purple",
          Signal == "Take Profit" ~ "darkgreen",
          Signal == "Order Canceled" ~ "gray",
          TRUE ~ "black"
        ),
        Shape = case_when(
          Signal == "Order Placed" ~ 17,
          Signal == "Order Executed" ~ 16,
          Signal == "Exit" ~ 15,
          Signal == "Take Profit" ~ 18,
          Signal == "Order Canceled" ~ 4,
          TRUE ~ 1
        )
      )
    
    p <- ggplot(equity_df, aes(x = Date, y = Equity)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = 100000, linetype = "dashed", color = "red", alpha = 0.7) +
      geom_point(data = signal_points, 
                 aes(color = Signal, shape = Signal), size = 3, alpha = 0.8) +
      scale_color_manual(values = c(
        "Order Placed" = "orange",
        "Order Executed" = "green",
        "Exit" = "purple",
        "Take Profit" = "darkgreen",
        "Order Canceled" = "gray"
      )) +
      scale_shape_manual(values = c(
        "Order Placed" = 17,
        "Order Executed" = 16,
        "Exit" = 15,
        "Take Profit" = 18,
        "Order Canceled" = 4
      )) +
      labs(title = "Динамика капитала", 
           subtitle = paste("Красная линия - начальный капитал (100,000 руб.) |",
                           "Take Profit:", input$take_profit, "% |",
                           "Отмена ордеров:", ifelse(result$enable_cancel, "ВКЛ", "ВЫКЛ")),
           x = "Дата", y = "Капитал") +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma) +
      theme(legend.position = "bottom")
    
    p
  })
  
  # Results table
  output$results_table <- renderTable({
    result <- strategy_data_value()
    if (is.null(result)) return(NULL)
    
    trades <- result$trades
    equity_series <- result$equity[1:nrow(result$data)]
    
    if(nrow(trades) > 0) {
      executed_trades <- trades[trades$Type == "SELL", ]
      
      if(nrow(executed_trades) > 0) {
        total_trades <- nrow(executed_trades)
        profitable_trades <- sum(executed_trades$Profit > 0, na.rm = TRUE)
        total_profit <- sum(executed_trades$Profit, na.rm = TRUE)
        max_profit <- max(executed_trades$Profit, na.rm = TRUE)
        max_loss <- min(executed_trades$Profit, na.rm = TRUE)
        win_rate <- profitable_trades / total_trades
        
        max_equity <- max(equity_series, na.rm = TRUE)
        min_equity <- min(equity_series, na.rm = TRUE)
        max_drawdown <- round((max_equity - min(equity_series)) / max_equity * 100, 1)
        
        # Анализ сделок с Take Profit
        tp_trades <- executed_trades[grepl("Take Profit", executed_trades$Reason), ]
        tp_count <- nrow(tp_trades)
        tp_profit <- ifelse(tp_count > 0, sum(tp_trades$Profit, na.rm = TRUE), 0)
        
        data.frame(
          Метрика = c("Режим", "Всего сделок", "Прибыльных сделок", 
                     "Процент прибыльных", "Общая прибыль", 
                     "Макс. прибыль", "Макс. убыток", "Конечный капитал",
                     "Макс. капитал", "Мин. капитал", "Макс. просадка",
                     "Отмена ордеров", "Take Profit", "Сделки с TP", "Прибыль от TP"),
          Значение = c(result$data_source,
                      total_trades, 
                      profitable_trades,
                      paste0(round(win_rate * 100, 1), "%"), 
                      round(total_profit, 2),
                      round(max_profit, 2),
                      round(max_loss, 2),
                      round(result$final_capital, 2),
                      round(max_equity, 2),
                      round(min_equity, 2),
                      paste0(max_drawdown, "%"),
                      ifelse(result$enable_cancel, "ВКЛ", "ВЫКЛ"),
                      paste0(input$take_profit, "%"),
                      tp_count,
                      round(tp_profit, 2))
        )
      }
    }
  })
  
  # Trades table
  output$trades_table <- renderTable({
    result <- strategy_data_value()
    if (is.null(result)) return(NULL)
    
    trades <- result$trades
    
    if(nrow(trades) > 0) {
      # Фильтруем только исполненные сделки (BUY/SELL)
      executed_trades <- trades %>%
        filter(Type %in% c("BUY", "SELL")) %>%
        select(EntryDate, ExitDate, Type, EntryPrice, ExitPrice, Shares, Profit, ReturnPct, Reason) %>%
        mutate(
          Profit = ifelse(is.na(Profit), "-", round(Profit, 2)),
          ReturnPct = ifelse(is.na(ReturnPct), "-", paste0(ReturnPct, "%")),
          EntryDate = as.character(EntryDate),
          ExitDate = as.character(ExitDate)
        ) %>%
        arrange(desc(EntryDate))
      
      executed_trades
    }
  })
}

# Run the application
shinyApp(ui, server)
