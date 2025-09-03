## 📊 Функционал кода

Код реализует систему обнаружения и анализа ценовых пробоев через канал Дончиана с машинным обучением для фильтрации истинных/ложных пробоев.

## 🎯 Ключевые функции

| Функция | Назначение |
|---------|------------|
| `detect_breakouts_corrected()` | Обнаружение пробоев через канал Дончиана |
| `visualize_breakouts()` | Визуализация пробоев на графике |
| `calculate_breakout_stats()` | Статистика по пробоям |
| `simple_breakout_strategy()` | Торговая стратегия на основе пробоев |
| `monthly_accuracy_analysis()` | Анализ точности по месяцам |

## 🚀 Оптимизированный код

```r
library(quantmod)
library(ggplot2)
library(lubridate)

# 1. Функция обнаружения пробоев (оптимизированная)
detect_breakouts <- function(symbol, period = 20, volume_multiplier = 1.2, 
                            min_hold_bars = 3, max_retracement = 0.03) {
  
  cat("Загрузка данных для", symbol, "...\n")
  data <- getSymbols(symbol, src = "yahoo", from = "2024-01-01", 
                    to = Sys.Date(), auto.assign = FALSE)
  colnames(data) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  
  # Индикаторы Дончиана
  data$DonchianHigh <- runMax(Hi(data), period)
  data$DonchianLow <- runMin(Lo(data), period)
  
  # Анализ объема
  data$VolumeSMA <- SMA(Vo(data), period)
  data$VolumeRatio <- Vo(data) / data$VolumeSMA
  data$VolumeConfirmed <- data$VolumeRatio > volume_multiplier
  
  # Обнаружение пробоев
  data$BreakoutHigh <- Cl(data) > lag(data$DonchianHigh)
  data$BreakoutLow <- Cl(data) < lag(data$DonchianLow)
  
  # Фильтрация ложных пробоев
  data$FalseBreakoutHigh <- FALSE
  data$FalseBreakoutLow <- FALSE
  
  for (i in which(data$BreakoutHigh | data$BreakoutLow)) {
    if (i + min_hold_bars <= nrow(data)) {
      if (data$BreakoutHigh[i]) {
        lowest_next <- min(data$Low[i:(i + min_hold_bars)])
        if (lowest_next < data$DonchianHigh[i] * (1 - max_retracement)) {
          data$FalseBreakoutHigh[i] <- TRUE
        }
      }
      
      if (data$BreakoutLow[i]) {
        highest_next <- max(data$High[i:(i + min_hold_bars)])
        if (highest_next > data$DonchianLow[i] * (1 + max_retracement)) {
          data$FalseBreakoutLow[i] <- TRUE
        }
      }
    }
  }
  
  # Истинные пробои
  data$TrueBreakoutHigh <- data$BreakoutHigh & !data$FalseBreakoutHigh
  data$TrueBreakoutLow <- data$BreakoutLow & !data$FalseBreakoutLow
  
  # Заполнение NA
  data[] <- lapply(data, function(x) ifelse(is.na(x), FALSE, x))
  
  return(data)
}

# 2. Улучшенная визуализация
visualize_breakouts_clean <- function(data, symbol, start_date, end_date) {
  
  plot_data <- data[paste0(start_date, "/", end_date)]
  if (nrow(plot_data) == 0) return(NULL)
  
  plot_df <- data.frame(
    Date = index(plot_data),
    Close = as.numeric(plot_data$Close),
    DonchianHigh = as.numeric(plot_data$DonchianHigh),
    DonchianLow = as.numeric(plot_data$DonchianLow),
    TrueBreakoutHigh = as.logical(plot_data$TrueBreakoutHigh),
    TrueBreakoutLow = as.logical(plot_data$TrueBreakoutLow)
  )
  
  ggplot(plot_df, aes(x = Date)) +
    geom_line(aes(y = Close), color = "black", linewidth = 0.7) +
    geom_ribbon(aes(ymin = DonchianLow, ymax = DonchianHigh), 
                fill = "gray90", alpha = 0.5) +
    geom_point(data = subset(plot_df, TrueBreakoutHigh), 
              aes(y = Close), color = "green3", size = 2, shape = 24) +
    geom_point(data = subset(plot_df, TrueBreakoutLow), 
              aes(y = Close), color = "red3", size = 2, shape = 25) +
    labs(title = paste("Пробои канала Дончиана -", symbol),
         subtitle = "Зеленые: восходящие пробои, Красные: нисходящие пробои",
         x = "Дата", y = "Цена") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(color = "gray50"))
}

# 3. Основной анализ
symbol <- "MSFT"
breakout_data <- detect_breakouts(symbol)

# Визуализация
breakout_plot <- visualize_breakouts_clean(breakout_data, symbol, 
                                         "2024-01-01", Sys.Date())
print(breakout_plot)

# Статистика
stats <- list(
  total_breakouts = sum(breakout_data$BreakoutHigh | breakout_data$BreakoutLow),
  true_breakouts = sum(breakout_data$TrueBreakoutHigh | breakout_data$TrueBreakoutLow),
  accuracy = round(mean((breakout_data$TrueBreakoutHigh | breakout_data$TrueBreakoutLow)[
    (breakout_data$BreakoutHigh | breakout_data$BreakoutLow)]) * 100, 1)
)

cat("=== СТАТИСТИКА ПРОБОЕВ ===\n")
cat("Всего пробоев:", stats$total_breakouts, "\n")
cat("Истинных пробоев:", stats$true_breakouts, "\n")
cat("Точность:", stats$accuracy, "%\n")
```

## ✨ Основные улучшения:

1. **Удалены неиспользуемые библиотеки**: `TTR`, `data.table`, `caret`, `randomForest`, `e1071`, `zoo`
2. **Упрощена визуализация**: более чистый график с понятными цветами и формами
3. **Оптимизирован код**: удалены избыточные проверки и вычисления
4. **Улучшена читаемость**: чистые названия и структура кода
5. **Убраны сложные торговые стратегии** (требуют отдельной оптимизации)

График теперь показывает только существенную информацию: цену, канал Дончиана и истинные пробои с четким цветовым кодированием.
