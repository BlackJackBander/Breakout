library(quantmod)
library(TTR)
library(data.table)
library(ggplot2)
library(lubridate)
library(caret)
library(randomForest)
library(e1071)
library(zoo)

# 1. ФУНКЦИЯ ДЕТЕКЦИИ ПРОБОЕВ С ИСПРАВЛЕННЫМ АЛГОРИТМОМ
detect_breakouts_corrected <- function(symbol, period = 20, volume_multiplier = 1.2, 
                                       min_hold_bars = 3, max_retracement = 0.03) {
  
  cat("Загрузка данных для", symbol, "...\n")
  data <- getSymbols(symbol, src = "yahoo", from = "2024-01-01", to = Sys.Date(), auto.assign = FALSE)
  colnames(data) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  
  # Расчет канала Дончиана
  data$DonchianHigh <- runMax(Hi(data), period)
  data$DonchianLow <- runMin(Lo(data), period)
  data$DonchianMid <- (data$DonchianHigh + data$DonchianLow) / 2
  
  # Объемные показатели
  data$VolumeSMA <- SMA(Vo(data), period)
  data$VolumeRatio <- Vo(data) / data$VolumeSMA
  data$VolumeConfirmed <- data$VolumeRatio > volume_multiplier
  
  # Обнаружение пробоев
  data$BreakoutHigh <- Cl(data) > lag(data$DonchianHigh)
  data$BreakoutLow <- Cl(data) < lag(data$DonchianLow)
  
  # Фильтры для подтверждения пробоев
  adx_data <- ADX(HLC(data), 14)
  data$ADX <- adx_data$ADX
  data$TrendStrength <- data$ADX > 25
  
  macd_data <- MACD(Cl(data), 12, 26, 9)
  data$MACD <- macd_data$macd
  data$MACD_Signal <- macd_data$signal
  data$MACD_Bullish <- data$MACD > data$MACD_Signal
  
  data$VolumeTrend <- Vo(data) > SMA(Vo(data), 5)
  
  # НОВЫЙ АЛГОРИТМ: Определение ложных пробоев на основе последующего движения
  data$FalseBreakoutHigh <- FALSE
  data$FalseBreakoutLow <- FALSE
  
  # Анализ каждого пробоя
  for (i in which(data$BreakoutHigh | data$BreakoutLow)) {
    if (i + min_hold_bars <= nrow(data)) {
      
      if (data$BreakoutHigh[i]) {
        # Пробой вверх считается ложным, если цена возвращается ниже канала
        lowest_next <- min(data$Low[i:(i + min_hold_bars)])
        if (lowest_next < data$DonchianHigh[i] * (1 - max_retracement)) {
          data$FalseBreakoutHigh[i] <- TRUE
        }
      }
      
      if (data$BreakoutLow[i]) {
        # Пробой вниз считается ложным, если цена возвращается выше канала
        highest_next <- max(data$High[i:(i + min_hold_bars)])
        if (highest_next > data$DonchianLow[i] * (1 + max_retracement)) {
          data$FalseBreakoutLow[i] <- TRUE
        }
      }
    }
  }
  
  # Определение истинных пробоев
  data$TrueBreakoutHigh <- data$BreakoutHigh & !data$FalseBreakoutHigh
  data$TrueBreakoutLow <- data$BreakoutLow & !data$FalseBreakoutLow
  
  # Заполнение NA значений
  data$FalseBreakoutHigh <- na.fill(data$FalseBreakoutHigh, FALSE)
  data$FalseBreakoutLow <- na.fill(data$FalseBreakoutLow, FALSE)
  data$TrueBreakoutHigh <- na.fill(data$TrueBreakoutHigh, FALSE)
  data$TrueBreakoutLow <- na.fill(data$TrueBreakoutLow, FALSE)
  data$BreakoutHigh <- na.fill(data$BreakoutHigh, FALSE)
  data$BreakoutLow <- na.fill(data$BreakoutLow, FALSE)
  
  cat("Данные обработаны. Всего пробоев:", sum(data$BreakoutHigh | data$BreakoutLow, na.rm = TRUE), "\n")
  return(data)
}

# 2. ФУНКЦИЯ ВИЗУАЛИЗАЦИИ
visualize_breakouts <- function(data, symbol, start_date, end_date) {
  
  plot_data <- data[paste0(start_date, "/", end_date)]
  
  if (nrow(plot_data) == 0) {
    cat("Нет данных для указанного диапазона дат\n")
    return(NULL)
  }
  
  plot_df <- data.frame(
    Date = index(plot_data),
    Close = as.numeric(plot_data$Close),
    DonchianHigh = as.numeric(plot_data$DonchianHigh),
    DonchianLow = as.numeric(plot_data$DonchianLow),
    TrueBreakoutHigh = as.logical(plot_data$TrueBreakoutHigh),
    TrueBreakoutLow = as.logical(plot_data$TrueBreakoutLow),
    FalseBreakoutHigh = as.logical(plot_data$FalseBreakoutHigh),
    FalseBreakoutLow = as.logical(plot_data$FalseBreakoutLow)
  )
  
  p <- ggplot(plot_df, aes(x = Date)) +
    geom_line(aes(y = Close, color = "Цена"), linewidth = 1) +
    geom_line(aes(y = DonchianHigh, color = "Верхний канал"), linetype = "dashed", linewidth = 0.8) +
    geom_line(aes(y = DonchianLow, color = "Нижний канал"), linetype = "dashed", linewidth = 0.8) +
    geom_ribbon(aes(ymin = DonchianLow, ymax = DonchianHigh), fill = "gray", alpha = 0.2)
  
  # Истинные пробои (зеленые)
  true_high <- plot_df[plot_df$TrueBreakoutHigh, ]
  true_low <- plot_df[plot_df$TrueBreakoutLow, ]
  
  if (nrow(true_high) > 0) {
    p <- p + geom_point(data = true_high, aes(y = Close), 
                        color = "green", size = 4, shape = 24, fill = "green")
  }
  
  if (nrow(true_low) > 0) {
    p <- p + geom_point(data = true_low, aes(y = Close), 
                        color = "green", size = 4, shape = 25, fill = "green")
  }
  
  # Ложные пробои (красные)
  false_high <- plot_df[plot_df$FalseBreakoutHigh, ]
  false_low <- plot_df[plot_df$FalseBreakoutLow, ]
  
  if (nrow(false_high) > 0) {
    p <- p + geom_point(data = false_high, aes(y = Close), 
                        color = "red", size = 4, shape = 24, fill = "red")
  }
  
  if (nrow(false_low) > 0) {
    p <- p + geom_point(data = false_low, aes(y = Close), 
                        color = "red", size = 4, shape = 25, fill = "red")
  }
  
  p <- p +
    labs(title = paste("Анализ пробоев Дончиана для", symbol, "\nЗеленые - истинные, Красные - ложные"),
         x = "Дата", y = "Цена") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_color_manual(values = c("Цена" = "black", 
                                  "Верхний канал" = "blue", 
                                  "Нижний канал" = "blue"))
  
  print(p)
  return(p)
}

# 3. ФУНКЦИЯ СТАТИСТИКИ
calculate_breakout_stats <- function(data, start_date, end_date) {
  
  period_data <- data[paste0(start_date, "/", end_date)]
  
  stats <- list()
  stats$total_breakouts <- sum(period_data$BreakoutHigh | period_data$BreakoutLow, na.rm = TRUE)
  stats$true_breakouts <- sum(period_data$TrueBreakoutHigh | period_data$TrueBreakoutLow, na.rm = TRUE)
  stats$false_breakouts <- sum(period_data$FalseBreakoutHigh | period_data$FalseBreakoutLow, na.rm = TRUE)
  stats$accuracy <- ifelse(stats$total_breakouts > 0, stats$true_breakouts / stats$total_breakouts * 100, 0)
  
  stats$high_breakouts <- sum(period_data$BreakoutHigh, na.rm = TRUE)
  stats$low_breakouts <- sum(period_data$BreakoutLow, na.rm = TRUE)
  stats$true_high <- sum(period_data$TrueBreakoutHigh, na.rm = TRUE)
  stats$true_low <- sum(period_data$TrueBreakoutLow, na.rm = TRUE)
  
  return(stats)
}

# 4. ИСПРАВЛЕННАЯ ТОРГОВАЯ СТРАТЕГИЯ
simple_breakout_strategy <- function(data, initial_capital = 100000) {
  
  capital <- initial_capital
  position <- 0
  entry_price <- 0
  position_type <- ""  # "LONG" или "SHORT"
  trades <- data.frame()
  
  for (i in 21:nrow(data)) {
    
    current_date <- index(data[i])
    current_price <- as.numeric(data$Close[i])
    
    # Вход в длинную позицию при истинном пробое вверх
    if (data$TrueBreakoutHigh[i] && position == 0) {
      entry_price <- current_price
      position_size <- capital * 0.1 / entry_price
      position <- position_size
      position_type <- "LONG"
      
      trades <- rbind(trades, data.frame(
        Date = current_date,
        Action = "BUY",
        Price = entry_price,
        Type = "True Breakout High",
        PnL = 0,
        Capital = capital,
        Position = position_size,
        stringsAsFactors = FALSE
      ))
      
      next
    }
    
    # Вход в короткую позицию при истинном пробое вниз
    if (data$TrueBreakoutLow[i] && position == 0) {
      entry_price <- current_price
      position_size <- capital * 0.1 / entry_price
      position <- position_size
      position_type <- "SHORT"
      
      trades <- rbind(trades, data.frame(
        Date = current_date,
        Action = "SHORT",
        Price = entry_price,
        Type = "True Breakout Low", 
        PnL = 0,
        Capital = capital,
        Position = position_size,
        stringsAsFactors = FALSE
      ))
      
      next
    }
    
    # Выход из позиции (если есть открытая позиция)
    if (position > 0 && entry_price > 0) {
      exit_condition <- FALSE
      exit_type <- ""
      
      # Для длинной позиции
      if (position_type == "LONG") {
        if (data$TrueBreakoutLow[i]) {
          exit_condition <- TRUE
          exit_type <- "Opposite Breakout"
        } else if (data$FalseBreakoutHigh[i]) {
          exit_condition <- TRUE
          exit_type <- "False Breakout"
        }
      }
      # Для короткой позиции
      else if (position_type == "SHORT") {
        if (data$TrueBreakoutHigh[i]) {
          exit_condition <- TRUE
          exit_type <- "Opposite Breakout"
        } else if (data$FalseBreakoutLow[i]) {
          exit_condition <- TRUE
          exit_type <- "False Breakout"
        }
      }
      
      # Выход по времени (максимум 10 баров удержания)
      if (position > 0 && nrow(trades) > 0) {
        last_trade_date <- trades$Date[nrow(trades)]
        bars_held <- as.numeric(difftime(current_date, last_trade_date, units = "days"))
        if (bars_held >= 10) {
          exit_condition <- TRUE
          exit_type <- "Time Exit"
        }
      }
      
      if (exit_condition) {
        exit_price <- current_price
        
        # Расчет PnL
        if (position_type == "LONG") {
          pnl <- (exit_price - entry_price) * position
        } else {
          pnl <- (entry_price - exit_price) * position
        }
        
        capital <- capital + pnl
        
        trades <- rbind(trades, data.frame(
          Date = current_date,
          Action = "SELL",
          Price = exit_price,
          Type = exit_type,
          PnL = pnl,
          Capital = capital,
          Position = 0,
          stringsAsFactors = FALSE
        ))
        
        # Сброс позиции
        position <- 0
        entry_price <- 0
        position_type <- ""
      }
    }
  }
  
  # Закрытие позиции в конце периода (если осталась открытой)
  if (position > 0 && entry_price > 0) {
    exit_price <- as.numeric(tail(data$Close, 1))
    
    if (position_type == "LONG") {
      pnl <- (exit_price - entry_price) * position
    } else {
      pnl <- (entry_price - exit_price) * position
    }
    
    capital <- capital + pnl
    
    trades <- rbind(trades, data.frame(
      Date = tail(index(data), 1),
      Action = "CLOSE",
      Price = exit_price,
      Type = "End of Period",
      PnL = pnl,
      Capital = capital,
      Position = 0,
      stringsAsFactors = FALSE
    ))
  }
  
  return(list(capital = capital, trades = trades))
}

# 5. АЛЬТЕРНАТИВНАЯ ПРОСТАЯ СТРАТЕГИЯ
very_simple_strategy <- function(data, initial_capital = 100000) {
  
  capital <- initial_capital
  trades <- data.frame()
  in_position <- FALSE
  entry_price <- 0
  entry_date <- NULL
  position_type <- ""
  
  for (i in 21:nrow(data)) {
    current_date <- index(data[i])
    current_price <- as.numeric(data$Close[i])
    
    # Вход при истинном пробое
    if (!in_position && data$TrueBreakoutHigh[i]) {
      entry_price <- current_price
      entry_date <- current_date
      in_position <- TRUE
      position_type <- "LONG"
      
      trades <- rbind(trades, data.frame(
        Date = current_date,
        Action = "ENTER",
        Price = entry_price,
        Type = "LONG",
        PnL = 0,
        Capital = capital,
        stringsAsFactors = FALSE
      ))
    }
    
    if (!in_position && data$TrueBreakoutLow[i]) {
      entry_price <- current_price
      entry_date <- current_date
      in_position <- TRUE
      position_type <- "SHORT"
      
      trades <- rbind(trades, data.frame(
        Date = current_date,
        Action = "ENTER",
        Price = entry_price,
        Type = "SHORT", 
        PnL = 0,
        Capital = capital,
        stringsAsFactors = FALSE
      ))
    }
    
    # Выход из позиции
    if (in_position) {
      exit_condition <- FALSE
      exit_type <- ""
      
      # Для длинной позиции
      if (position_type == "LONG") {
        if (data$TrueBreakoutLow[i]) {
          exit_condition <- TRUE
          exit_type <- "Opposite"
        } else if (data$FalseBreakoutHigh[i]) {
          exit_condition <- TRUE
          exit_type <- "False Breakout"
        }
      }
      # Для короткой позиции
      else if (position_type == "SHORT") {
        if (data$TrueBreakoutHigh[i]) {
          exit_condition <- TRUE
          exit_type <- "Opposite"
        } else if (data$FalseBreakoutLow[i]) {
          exit_condition <- TRUE
          exit_type <- "False Breakout"
        }
      }
      
      # Выход по времени
      if (in_position && !is.null(entry_date)) {
        bars_held <- as.numeric(difftime(current_date, entry_date, units = "days"))
        if (bars_held >= 10) {
          exit_condition <- TRUE
          exit_type <- "Time Exit"
        }
      }
      
      if (exit_condition) {
        exit_price <- current_price
        
        # Расчет PnL (10% капитала на сделку)
        position_size <- capital * 0.1 / entry_price
        
        if (position_type == "LONG") {
          pnl <- (exit_price - entry_price) * position_size
        } else {
          pnl <- (entry_price - exit_price) * position_size
        }
        
        capital <- capital + pnl
        
        trades <- rbind(trades, data.frame(
          Date = current_date,
          Action = "EXIT",
          Price = exit_price,
          Type = exit_type,
          PnL = pnl,
          Capital = capital,
          stringsAsFactors = FALSE
        ))
        
        in_position <- FALSE
        entry_price <- 0
        entry_date <- NULL
        position_type <- ""
      }
    }
  }
  
  return(list(capital = capital, trades = trades))
}

# 6. ФУНКЦИЯ АНАЛИЗА ТОЧНОСТИ ПО МЕСЯЦАМ
monthly_accuracy_analysis <- function(data) {
  months <- unique(format(index(data), "%Y-%m"))
  monthly_stats <- data.frame()
  
  for (month in months) {
    month_start <- as.Date(paste0(month, "-01"))
    month_end <- ceiling_date(month_start, "month") - days(1)
    
    stats <- calculate_breakout_stats(data, month_start, month_end)
    
    monthly_stats <- rbind(monthly_stats, data.frame(
      Month = month,
      Total = stats$total_breakouts,
      True = stats$true_breakouts,
      False = stats$false_breakouts,
      Accuracy = stats$accuracy
    ))
  }
  
  return(monthly_stats)
}

# 7. ОСНОВНОЙ КОД
cat("=== АНАЛИЗ ПРОБОЕВ ДОНЧИАНА С ИСПРАВЛЕННЫМ АЛГОРИТМОМ ===\n")

# Настройки
symbol <- "MSFT"
start_date <- as.Date("2024-01-01")
end_date <- Sys.Date()

# Загрузка и обработка данных
cat("1. Загрузка и обработка данных...\n")
breakout_data <- detect_breakouts_corrected(symbol)

# Визуализация
cat("2. Визуализация пробоев...\n")
breakout_plot <- visualize_breakouts(breakout_data, symbol, start_date, end_date)

# Статистика
cat("3. Расчет статистики...\n")
stats <- calculate_breakout_stats(breakout_data, start_date, end_date)

cat("\n=== СТАТИСТИКА ЗА ПЕРИОД С ИЮЛЯ 2024 ===\n")
cat("Всего пробоев:", stats$total_breakouts, "\n")
cat("Истинных пробоев:", stats$true_breakouts, "\n") 
cat("Ложных пробоев:", stats$false_breakouts, "\n")
cat("Точность алгоритма:", round(stats$accuracy, 1), "%\n")
cat("Пробоев вверх:", stats$high_breakouts, "\n")
cat("Пробоев вниз:", stats$low_breakouts, "\n")
cat("Истинных вверх:", stats$true_high, "\n")
cat("Истинных вниз:", stats$true_low, "\n")

# Анализ по месяцам
cat("\n4. Анализ точности по месяцам...\n")
monthly_stats <- monthly_accuracy_analysis(breakout_data)
print(monthly_stats)

# График точности по месяцам
accuracy_plot <- ggplot(monthly_stats, aes(x = Month, y = Accuracy, group = 1)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 3) +
  labs(title = "Точность определения пробоев по месяцам",
       x = "Месяц", y = "Точность (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 100)

print(accuracy_plot)

# ТЕСТИРОВАНИЕ ТОРГОВЫХ СТРАТЕГИЙ
cat("\n5. Тестирование торговых стратегий...\n")

# Попробуем основную стратегию
tryCatch({
  cat("Запуск основной стратегии...\n")
  trading_results <- simple_breakout_strategy(breakout_data, 100000)
  cat("Основная стратегия выполнена успешно!\n")
}, error = function(e) {
  cat("Ошибка в основной стратегии:", e$message, "\n")
  cat("Запуск альтернативной стратегии...\n")
  trading_results <- very_simple_strategy(breakout_data, 100000)
  cat("Альтернативная стратегия выполнена успешно!\n")
})

# АНАЛИЗ РЕЗУЛЬТАТОВ
cat("\n=== РЕЗУЛЬТАТЫ ТОРГОВЛИ ===\n")
cat("Конечный капитал: $", round(trading_results$capital, 2), "\n")
cat("Прибыль/убыток: $", round(trading_results$capital - 100000, 2), "\n")

if (nrow(trading_results$trades) > 0) {
  # Подсчет сделок
  entry_trades <- trading_results$trades[trading_results$trades$Action %in% c("BUY", "SHORT", "ENTER"), ]
  exit_trades <- trading_results$trades[trading_results$trades$Action %in% c("SELL", "EXIT", "CLOSE"), ]
  
  cat("Количество сделок:", nrow(entry_trades), "\n")
  
  if (nrow(exit_trades) > 0) {
    profitable_trades <- sum(exit_trades$PnL > 0, na.rm = TRUE)
    win_rate <- profitable_trades / nrow(exit_trades) * 100
    avg_profit <- mean(exit_trades$PnL, na.rm = TRUE)
    
    cat("Процент прибыльных сделок:", round(win_rate, 1), "%\n")
    cat("Средняя прибыль на сделку: $", round(avg_profit, 2), "\n")
    cat("Максимальная прибыль: $", round(max(exit_trades$PnL, na.rm = TRUE), 2), "\n")
    cat("Максимальный убыток: $", round(min(exit_trades$PnL, na.rm = TRUE), 2), "\n")
    
    cat("\nПоследние 5 сделок:\n")
    print(tail(trading_results$trades, 10))
  }
} else {
  cat("Не было совершено ни одной сделки\n")
}

# ГРАФИК КРИВОЙ КАПИТАЛА
if (nrow(trading_results$trades) > 0) {
  equity_data <- trading_results$trades[, c("Date", "Capital")]
  equity_data <- equity_data[!duplicated(equity_data$Date), ]
  
  equity_plot <- ggplot(equity_data, aes(x = as.Date(Date), y = Capital)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_hline(yintercept = 100000, linetype = "dashed", color = "red") +
    labs(title = "Кривая капитала торговой стратегии",
         x = "Дата", y = "Капитал ($)") +
    theme_minimal()
  
  print(equity_plot)
}

# Дополнительный анализ объема
cat("\n6. Анализ объемов...\n")
volume_stats <- breakout_data[, c("Volume", "VolumeRatio", "VolumeConfirmed")]
volume_summary <- data.frame(
  Средний_Объем = mean(volume_stats$Volume, na.rm = TRUE),
  Среднее_Отношение = mean(volume_stats$VolumeRatio, na.rm = TRUE),
  Процент_Подтвержденных = mean(volume_stats$VolumeConfirmed, na.rm = TRUE) * 100
)

print(volume_summary)

cat("\n=== АНАЛИЗ ЗАВЕРШЕН ===\n")

# Сохранение результатов
output_file <- paste0("breakout_analysis_", symbol, "_", Sys.Date(), ".RData")
save(breakout_data, trading_results, monthly_stats, file = output_file)
cat("Результаты сохранены в файл:", output_file, "\n")
