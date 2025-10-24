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

# ===== ОПТИМАЛЬНЫЕ ПАРАМЕТРЫ =====
# Create a data matrix for new strategy parameters
first <- matrix(c(
  # Ticker, Length, SMA, Risk, Growth, Offset
  10, 40, 5, 2, 3,    # RENI
  15, 10, 5, 3, 1,    # ROSN
  10, 20, 5, 2, 1,    # SBER
  5, 10, 5, 1, 1,     # SBGD
  10, 10, 5, 2, 1,    # SBMX
  20, 30, 5, 1, 1,    # SBRB
  10, 30, 5, 3, 1,    # SIBN
  10, 20, 5, 4, 1,    # SMLT
  10, 10, 5, 3, 1,    # SNGS
  5, 10, 5, 1, 1,     # SNGSP
  10, 20, 3, 3, 1,    # T
  5, 20, 5, 1, 1,     # TASB
  10, 10, 5, 4, 1,    # TATN
  10, 20, 5, 5, 1,    # TATNP
  5, 70, 5, 5, 1,     # TGKA
  10, 100, 5, 1, 1,   # TRMK
  5, 90, 5, 1, 1,     # UPRO
  5, 80, 5, 2, 5,     # UTAR
  35, 30, 5, 1, 1,    # VKCO
  15, 30, 5, 1, 3     # VTBR
), ncol = 5, byrow = TRUE)

rownames(first) <- c("RENI", "ROSN", "SBER", "SBGD", "SBMX", "SBRB", "SIBN", 
                     "SMLT", "SNGS", "SNGSP", "T", "TASB", "TATN", "TATNP", 
                     "TGKA", "TRMK", "UPRO", "UTAR", "VKCO", "VTBR")
colnames(first) <- c("Length", "SMA", "Risk", "Growth", "Offset")

# Create a data matrix for strategy parameters
two <- matrix(c(
  # Ticker, Length, SMA, Risk, Growth, Offset
  10, 10, 5, 3, 2,    # AFKS
  10, 20, 5, 2, 1,    # AFLT
  10, 20, 5, 3, 1,    # BSPB
  5, 100, 5, 1, 1,    # CHMF
  10, 90, 5, 3, 1,    # DIAS
  10, 70, 5, 1, 2,    # ENPG
  15, 100, 5, 1, 1,   # EUTR
  10, 60, 5, 2, 3,    # FESH
  5, 70, 5, 2, 3,     # GAZP
  5, 100, 5, 1, 1,    # GECO
  15, 40, 4, 1, 2,    # HEAD
  10, 10, 5, 2, 1,    # IRAO
  5, 80, 5, 1, 2,     # LEAS
  5, 40, 5, 5, 1,     # MTLR
  10, 20, 5, 1, 1,    # MTSS
  15, 50, 5, 1, 1,    # MVID
  10, 10, 5, 1, 1,    # PIKK
  10, 10, 5, 1, 1,    # PLZL
  10, 20, 5, 2, 1,    # POSI
  5, 10, 5, 4, 1      # RASP
), ncol = 5, byrow = TRUE)

rownames(two) <- c("AFKS", "AFLT", "BSPB", "CHMF", "DIAS", "ENPG", "EUTR", 
                   "FESH", "GAZP", "GECO", "HEAD", "IRAO", "LEAS", "MTLR", 
                   "MTSS", "MVID", "PIKK", "PLZL", "POSI", "RASP")
colnames(two) <- c("Length", "SMA", "Risk", "Growth", "Offset")

# Create a data matrix for new strategy parameters
three <- matrix(c(
  # Ticker, Length, SMA, Risk, Growth, Offset
  10, 20, 5, 1, 4,    # WUSH
  10, 20, 4, 3, 1     # YDEX
), ncol = 5, byrow = TRUE)

rownames(three) <- c("WUSH", "YDEX")
colnames(three) <- c("Length", "SMA", "Risk", "Growth", "Offset")

# Объединяем все параметры
optimise_param <- rbind(first, two, three)

# Функция для получения оптимальных параметров по тикеру
get_optimal_params <- function(ticker) {
  if (ticker %in% rownames(optimise_param)) {
    return(as.list(optimise_param[ticker, ]))
  } else {
    # Возвращаем параметры по умолчанию, если тикер не найден
    return(list(Length = 10, SMA = 20, Risk = 2, Growth = 1, Offset = 1))
  }
}

# Функция для обработки и нормализации данных MOEX
process_moex_data <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }
  
  # Преобразуем все числовые колонки к numeric
  numeric_columns <- c('open', 'high', 'low', 'close', 'volume')
  
  for (col in numeric_columns) {
    if (col %in% names(data)) {
      data[[col]] <- as.numeric(as.character(data[[col]]))
    }
  }
  
  # Убеждаемся, что дата в правильном формате
  if ('end' %in% names(data)) {
    data$end <- as.Date(data$end)
  }
  
  # Удаляем строки с NA в ключевых колонках
  data <- data[complete.cases(data[, c('end', 'close')]), ]
  
  return(data)
}

# UI часть
ui <- fluidPage(
  # Темное оформление (остается без изменений)
  tags$head(
    tags$style(HTML("
      /* Основные стили */
      body {
        background-color: #1e1e1e;
        color: #e0e0e0;
        font-size: 12px;
      }
      
      /* Уменьшаем размеры элементов */
      .form-group {
        margin-bottom: 8px;
      }
      
      .control-label {
        font-size: 11px;
        font-weight: bold;
      }
      
      .btn {
        font-size: 11px;
        padding: 4px 8px;
        margin-bottom: 5px;
      }
      
      .slider-input {
        font-size: 11px;
      }
      
      /* Стили для выпадающих списков */
      .selectize-input {
        background-color: #2d2d2d !important;
        color: #e0e0e0 !important;
        border-color: #4d4d4d !important;
        font-size: 11px;
        padding: 4px 8px;
      }
      
      .selectize-dropdown {
        background-color: #2d2d2d !important;
        color: #e0e0e0 !important;
        border-color: #4d4d4d !important;
      }
      
      .selectize-dropdown-content div {
        background-color: #2d2d2d !important;
        color: #e0e0e0 !important;
        font-size: 11px;
        padding: 4px 8px;
      }
      
      .selectize-dropdown-content div.active {
        background-color: #3d3d3d !important;
        color: #ffffff !important;
      }
      
      /* Стили для календаря */
      .datepicker {
        background-color: #2d2d2d !important;
        color: #e0e0e0 !important;
        border-color: #4d4d4d !important;
      }
      
      .datepicker table {
        background-color: #2d2d2d !important;
        color: #e0e0e0 !important;
      }
      
      .datepicker table tr td {
        background-color: #2d2d2d !important;
        color: #e0e0e0 !important;
        border-color: #4d4d4d !important;
      }
      
      .datepicker table tr td.active {
        background-color: #007acc !important;
        color: #ffffff !important;
      }
      
      .datepicker table tr td:hover {
        background-color: #3d3d3d !important;
      }
      
      /* Стили для панелей */
      .well {
        background-color: #252526;
        border-color: #3d3d3d;
        padding: 10px;
        margin-bottom: 8px;
      }
      
      /* Стили для таблиц */
      .table {
        color: #e0e0e0;
        font-size: 11px;
      }
      
      /* Заголовки */
      h1, h2, h3, h4 {
        color: #ffffff;
        font-size: 14px;
        margin-top: 8px;
        margin-bottom: 8px;
      }
      
      /* Новая структура макета */
      .main-panel-container {
        display: flex;
        flex-direction: column;
        height: 100vh;
      }
      
      .charts-results-container {
        display: flex;
        flex: 1;
        gap: 10px;
        margin-bottom: 10px;
      }
      
      .charts-container {
        flex: 3;
        display: flex;
        flex-direction: column;
        gap: 10px;
      }
      
      .results-container {
        flex: 1;
        overflow-y: auto;
        background-color: #252526;
        padding: 10px;
        border-radius: 5px;
        border: 1px solid #3d3d3d;
      }
      
      .table-container {
        flex: 0 0 300px;
        overflow: auto;
        border-top: 2px solid #3d3d3d;
      }
      
      /* Стили для графиков */
      .plot-container {
        background-color: #252526;
        border-radius: 5px;
        padding: 10px;
        border: 1px solid #3d3d3d;
      }
    "))
  ),
  
  titlePanel("Interactive Trading Strategy - Channel Breakout"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      style = "height: 95vh; overflow-y: auto;",
      
      # Режим работы
      radioButtons("mode", "Режим работы:",
                   choices = c("Симуляция данных" = "sim", "Реальные данные MOEX" = "real"),
                   selected = "real"),
      
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
                    choices = c("ABIO", "ABRD", "AFKS", "AFLT", "AKRN", "ALRS", "AMEZ", "APRI", "APTK", "AQUA", 
                                "ARSA", "ASSB", "ASTR", "AVAN", "BANE", "BANEP", "BELU", "BISVP", "BLNG", "BRZL", 
                                "BSPB", "BSPBP", "CARM", "CBOM", "CHGZ", "CHKZ", "CHMF", "CHMK", "CNRU", "CNTL", 
                                "CNTLP", "DATA", "DELI", "DIAS", "DIOD", "DVEC", "DZRD", "DZRDP", "EELT", "ELFV", 
                                "ELMT", "ENPG", "ETLN", "EUTR", "FEES", "FESH", "FIXR", "FLOT", "GAZA", "GAZAP", 
                                "GAZC", "GAZP", "GAZS", "GAZT", "GCHE", "GECO", "GEMA", "GEMC", "GMKN", "GTRK", 
                                "HEAD", "HIMCP", "HNFG", "HYDR", "IGST", "IGSTP", "IRAO", "IRKT", "IVAT", "JNOS", 
                                "JNOSP", "KAZT", "KAZTP", "KBSB", "KCHE", "KCHEP", "KFBA", "KGKC", "KGKCP", "KLSB", 
                                "KLVZ", "KMAZ", "KMEZ", "KOGK", "KRKN", "KRKNP", "KRKOP", "KROT", "KROTP", "KRSB", 
                                "KRSBP", "KUZB", "KZOS", "KZOSP", "LEAS", "LENT", "LIFE", "LKOH", "LMBZ", "LNZL", 
                                "LNZLP", "LPSB", "LSNG", "LSNGP", "LSRG", "LVHK", "MAGE", "MAGEP", "MAGN", "MBNK", 
                                "MDMG", "MFGS", "MFGSP", "MGKL", "MGNT", "MGNZ", "MGTS", "MGTSP", "MISB", "MISBP", 
                                "MOEX", "MRKC", "MRKK", "MRKP", "MRKS", "MRKU", "MRKV", "MRKY", "MRKZ", "MRSB", 
                                "MSNG", "MSRS", "MSTT", "MTLR", "MTLRP", "MTSS", "MVID", "NAUK", "NFAZ", "NKHP", 
                                "NKNC", "NKNCP", "NKSH", "NLMK", "NMTP", "NNSB", "NNSBP", "NSVZ", "NVTK", "OGKB", 
                                "OKEY", "OMZZP", "OZON", "OZPH", "PAZA", "PHOR", "PIKK", "PLZL", "PMSB", "PMSBP", 
                                "POSI", "PRFN", "PRMB", "PRMD", "QIWI", "RAGR", "RASP", "RBCM", "RDRB", "RENI", 
                                "RGSS", "RKKE", "RNFT", "ROLO", "ROSN", "ROST", "RTGZ", "RTKM", "RTKMP", "RTSB", 
                                "RTSBP", "RUAL", "RUSI", "RZSB", "SAGO", "SAGOP", "SARE", "SAREP", "SBER", "SBERP", 
                                "SELG", "SFIN", "SGZH", "SIBN", "SLEN", "SMLT", "SNGS", "SNGSP", "SOFL", "SPBE", 
                                "STSB", "STSBP", "SVAV", "SVCB", "SVET", "SVETP", "T", "TASB", "TASBP", "TATN", 
                                "TATNP", "TGKA", "TGKB", "TGKBP", "TGKN", "TNSE", "TORS", "TORSP", "TRMK", "TRNFP", 
                                "TTLK", "TUZA", "UGLD", "UKUZ", "UNAC", "UNKL", "UPRO", "URKZ", "USBN", "UTAR", 
                                "UWGN", "VEON-RX", "VGSB", "VGSBP", "VJGZ", "VJGZP", "VKCO", "VLHZ", "VRSB", 
                                "VRSBP", "VSEH", "VSMO", "VSYD", "VSYDP", "VTBR", "WTCM", "WTCMP", "WUSH", "X5", 
                                "YAKG", "YDEX", "YKEN", "YKENP", "YRSB", "YRSBP", "ZAYM", "ZILL", "ZVEZ"),
                    selected = "SBER"),
        actionButton("update_data", "Обновить данные"),
        checkboxInput("force_update", "Принудительное обновление", value = FALSE),
        dateRangeInput("real_date_range", "Период тестирования:",
                       start = as.Date("2020-01-01"),
                       end = Sys.Date())
      ),
      
      # Параметры стратегии
      sliderInput("length", "Длина канала:", 
                  min = 5, max = 100, value = 20, step = 1),
      sliderInput("sma_period", "Длина RMA:", 
                  min = 5, max = 150, value = 20, step = 1),
      sliderInput("risk_percent", "Риск на сделку (%):", 
                  min = 1, max = 100, value = 20, step = 1),
      sliderInput("channel_growth", "Рост канала для входа (%):", 
                  min = 0, max = 10, value = 3, step = 0.1),
      sliderInput("entry_offset", "Отступ для ордера (% от канала):", 
                  min = 0, max = 10, value = 5, step = 0.1),
      actionButton("auto_params", "Автозагрузка параметров", class = "btn-success"),
      actionButton("reset_params", "Сброс параметров", class = "btn-warning btn-sm"),
      # Новая опция: Take Profit
      sliderInput("take_profit", "Take Profit (% от входа):", 
                  min = 0, max = 100, value = 0, step = 1),
      helpText("0% = отключено. При достижении Take Profit закрывается 90% позиции"),
      
      # Опция: включение/выключение отмены ордеров
      checkboxInput("enable_cancel", "Включить отмену ордеров", value = TRUE),
      helpText("При отключении: ордера не отменяются, даже если условие входа перестает выполняться"),
      
      actionButton("run", "Запустить тестирование", class = "btn-primary")
    ),
    
    mainPanel(
      width = 9,
      class = "main-panel-container",
      
      # Контейнер для графиков и результатов (рядом)
      div(class = "charts-results-container",
          
          # Левый блок - графики
          div(class = "charts-container",
              div(class = "plot-container",
                  plotOutput("strategy_plot", height = "350px")
              ),
              div(class = "plot-container",
                  plotOutput("equity_plot", height = "250px")
              )
          ),
          
          # Правый блок - таблица результатов
          div(class = "results-container",
              h4("Результаты тестирования:"),
              tableOutput("results_table")
          )
      ),
      
      # Контейнер для таблицы сделок (без изменений)
      div(class = "table-container",
          h4("Детали сделок:"),
          div(style = 'height: 200px; overflow-y: auto;',
              tableOutput("trades_table"))
      )
    )
  )
)

# Server часть
server <- function(input, output, session) {
  # Reactive value для хранения данных
  moex_data <- reactiveVal()
  strategy_data_value <- reactiveVal(NULL)
  
  # Автозагрузка оптимальных параметров при смене тикера
  observeEvent(input$ticker, {
    if (input$mode == "real") {
      params <- get_optimal_params(input$ticker)
      
      updateSliderInput(session, "length", value = params$Length)
      updateSliderInput(session, "sma_period", value = params$SMA)
      updateSliderInput(session, "risk_percent", value = params$Risk)
      updateSliderInput(session, "channel_growth", value = params$Growth)
      updateSliderInput(session, "entry_offset", value = params$Offset)
      
      showNotification(
        paste("Оптимальные параметры для", input$ticker, "загружены автоматически!"),
        type = "message", duration = 3
      )
    }
  })
  
  # Ручная автозагрузка параметров по кнопке
  observeEvent(input$auto_params, {
    if (input$mode == "real") {
      params <- get_optimal_params(input$ticker)
      
      updateSliderInput(session, "length", value = params$Length)
      updateSliderInput(session, "sma_period", value = params$SMA)
      updateSliderInput(session, "risk_percent", value = params$Risk)
      updateSliderInput(session, "channel_growth", value = params$Growth)
      updateSliderInput(session, "entry_offset", value = params$Offset)
      
      showNotification(
        paste("Оптимальные параметры для", input$ticker, "загружены!"),
        type = "message", duration = 3
      )
    }
  })
  
  # Сброс параметров к значениям по умолчанию
  observeEvent(input$reset_params, {
    updateSliderInput(session, "length", value = 20)
    updateSliderInput(session, "sma_period", value = 20)
    updateSliderInput(session, "risk_percent", value = 20)
    updateSliderInput(session, "channel_growth", value = 3)
    updateSliderInput(session, "entry_offset", value = 5)
    
    showNotification("Параметры сброшены к значениям по умолчанию", type = "warning")
  })
  
  # Обработчик кнопки обновления данных
  observeEvent(input$update_data, {
    if (input$mode == "real") {
      showNotification("Загрузка данных MOEX...", type = "message")
      
      tryCatch({
        # Загружаем данные
        data <- loadMOEXData(input$ticker, input$force_update)
        
        # Обрабатываем и нормализуем данные
        data <- process_moex_data(data)
        
        if (is.null(data) || nrow(data) == 0) {
          stop("Не удалось загрузить данные или данные пустые")
        }
        
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
    take_profit_pct <- input$take_profit / 100
    
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
    
    # Убеждаемся, что данные в правильном формате
    data$close <- as.numeric(data$close)
    data$high <- as.numeric(data$high)
    data$low <- as.numeric(data$low)
    
    # Calculate indicators
    data$upperW <- TTR::runMax(data$close, length_val * 2)
    data$lowerW <- TTR::runMin(data$close, (length_val *2)/1.2)
    data$sma_exit <- TTR::EMA(data$close, (sma_period_val*2))
    data <- data[complete.cases(data), ]
    
    # Initialize variables
    capital <- initial_capital
    position <- 0
    entry_price <- 0
    entry_date <- NULL
    pending_order <- FALSE
    pending_price <- 0
    pending_date <- NULL
    trades <- data.frame()
    equity <- rep(initial_capital, nrow(data))
    signals <- rep("No signal", nrow(data))
    take_profit_triggered <- FALSE
    
    # Вспомогательная функция для добавления сделок
    add_trade <- function(entry_date, exit_date, entry_price, exit_price, shares, type, reason) {
      trade_df <- data.frame(
        EntryDate = if(!is.null(entry_date)) as.character(entry_date) else NA,
        ExitDate = if(!is.null(exit_date)) as.character(exit_date) else NA,
        EntryPrice = if(!is.null(entry_price)) entry_price else NA,
        ExitPrice = if(!is.null(exit_price)) exit_price else NA,
        Shares = if(!is.null(shares)) shares else 0,
        Type = type,
        Reason = reason,
        stringsAsFactors = FALSE
      )
      
      if (nrow(trades) == 0) {
        return(trade_df)
      } else {
        return(rbind(trades, trade_df))
      }
    }
    
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
          entry_date <- pending_date
          capital <- capital - (position * entry_price)
          
          trades <- add_trade(
            entry_date = entry_date,
            exit_date = NA,
            entry_price = entry_price,
            exit_price = NA,
            shares = position,
            type = "BUY",
            reason = "Order Executed"
          )
          
          signals[i] <- "Order Executed"
          pending_order <- FALSE
          pending_price <- 0
          pending_date <- NULL
          take_profit_triggered <- FALSE
          
          # ОБНОВЛЯЕМ КАПИТАЛ ПОСЛЕ ОТКРЫТИЯ ПОЗИЦИИ
          equity[i] <- capital + (position * current$close)
        } else {
          # ЛОГИКА ОТМЕНЫ ОРДЕРОВ - зависит от настройки
          if (enable_cancel && !buySignal) {
            # ИСПРАВЛЕНИЕ: Добавляем запись об отмене ордера в таблицу trades
            trades <- add_trade(
              entry_date = pending_date,
              exit_date = current$end,
              entry_price = pending_price,
              exit_price = NA,
              shares = 0,
              type = "CANCEL",
              reason = "Order Canceled"
            )
            
            pending_order <- FALSE
            pending_price <- 0
            pending_date <- NULL
            signals[i] <- "Order Canceled"
            
            # ВАЖНО: капитал НЕ меняется при отмене ордера
            equity[i] <- capital
          } else {
            # Ордер все еще pending - капитал не меняется
            equity[i] <- capital
          }
        }
      }
      
      # Process open position
      if(position > 0) {
        current_portfolio_value <- capital + (position * current$close)
        equity[i] <- current_portfolio_value
        
        # Проверка Take Profit
        if (take_profit_pct > 0 && !take_profit_triggered) {
          take_profit_price <- entry_price * (1 + take_profit_pct)
          
          if (current$high >= take_profit_price) {
            shares_to_close <- floor(position * 0.9)
            profit <- shares_to_close * (take_profit_price - entry_price)
            
            capital <- capital + (shares_to_close * take_profit_price)
            position <- position - shares_to_close
            
            trades <- add_trade(
              entry_date = entry_date,
              exit_date = current$end,
              entry_price = entry_price,
              exit_price = take_profit_price,
              shares = shares_to_close,
              type = "SELL",
              reason = "Take Profit (90%)"
            )
            
            take_profit_triggered <- TRUE
            signals[i] <- "Take Profit"
            
            # Обновляем капитал после частичного закрытия
            equity[i] <- capital + (position * current$close)
          }
        }
        
        # Выход по RMA
        if(exitBuy && position > 0) {
          capital <- capital + (position * current$close)
          profit <- position * (current$close - entry_price)
          
          trades <- add_trade(
            entry_date = entry_date,
            exit_date = current$end,
            entry_price = entry_price,
            exit_price = current$close,
            shares = position,
            type = "SELL",
            reason = ifelse(take_profit_triggered, "Exit Signal (RMA) - Remainder", "Exit Signal (RMA)")
          )
          
          position <- 0
          entry_price <- 0
          entry_date <- NULL
          take_profit_triggered <- FALSE
          signals[i] <- "Exit"
          
          # Обновляем капитал после полного закрытия позиции
          equity[i] <- capital
        }
      } else if(buySignal && !pending_order) {
        pending_order <- TRUE
        pending_price <- order_price
        pending_date <- current$end
        signals[i] <- "Order Placed"
        
        # Капитал не меняется при размещении отложенного ордера
        equity[i] <- capital
      } else {
        # Нет открытых позиций и нет отложенных ордеров
        equity[i] <- capital
      }
    }
    
    # Close any open position at the end
    if (position > 0) {
      last_price <- tail(data$close, 1)
      capital <- capital + (position * last_price)
      equity[length(equity)] <- capital
      
      if (nrow(trades) > 0) {
        # Находим последнюю открытую позицию
        open_trades <- which(is.na(trades$ExitDate) & trades$Type == "BUY")
        if (length(open_trades) > 0) {
          for(trade_idx in open_trades) {
            trades$ExitDate[trade_idx] <- as.character(tail(data$end, 1))
            trades$ExitPrice[trade_idx] <- last_price
            trades$Type[trade_idx] <- "SELL"
            trades$Reason[trade_idx] <- "End of Period"
          }
        }
      }
    }
    
    # Close any pending orders at the end
    if (pending_order) {
      trades <- add_trade(
        entry_date = pending_date,
        exit_date = tail(data$end, 1),
        entry_price = pending_price,
        exit_price = NA,
        shares = 0,
        type = "CANCEL",
        reason = "Pending Order Expired"
      )
      
      # Капитал НЕ меняется при отмене отложенного ордера
      equity[length(equity)] <- capital
    }
    
    # Calculate trade results
    if(nrow(trades) > 0) {
      trades$Profit <- ifelse(trades$Type == "SELL", 
                              trades$Shares * (trades$ExitPrice - trades$EntryPrice),
                              NA)
      trades$ReturnPct <- ifelse(trades$Type == "SELL",
                                 round((trades$ExitPrice - trades$EntryPrice) / trades$EntryPrice * 100, 2),
                                 NA)
      
      # Убеждаемся, что даты в правильном формате
      trades$EntryDate <- as.character(trades$EntryDate)
      trades$ExitDate <- as.character(trades$ExitDate)
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
    
    if (!result$enable_cancel) {
      plot_data$signal[plot_data$signal == "Order Canceled"] <- "No signal"
    }
    
    dark_theme <- theme(
      plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
      panel.background = element_rect(fill = "#252526"),
      panel.grid.major = element_line(color = "#3d3d3d", size = 0.2),
      panel.grid.minor = element_line(color = "#3d3d3d", size = 0.1),
      text = element_text(color = "#e0e0e0"),
      axis.text = element_text(color = "#e0e0e0"),
      legend.background = element_rect(fill = "#252526"),
      legend.key = element_rect(fill = "#252526"),
      legend.text = element_text(color = "#e0e0e0")
    )
    
    ggplot(plot_data, aes(x = end)) +
      geom_line(aes(y = close, color = "Price"), size = 1) +
      geom_line(aes(y = upperW, color = "Upper Channel"), size = 0.8, alpha = 0.7) +
      geom_line(aes(y = lowerW, color = "Upper Channel"), size = 0.8, alpha = 0.7) +
      geom_line(aes(y = sma_exit, color = "SMA (Exit)"), size = 0.8, linetype = "dashed") +
      geom_point(data = subset(plot_data, signal != "No signal"), 
                 aes(y = close, color = signal), size = 3) +
      scale_color_manual(values = c(
        "Price" = "gray", 
        "Upper Channel" = "red", 
        "SMA (Exit)" = "pink",
        "Order Placed" = "orange",
        "Order Executed" = "green",
        "Exit" = "purple",
        "Take Profit" = "darkgreen",
        "Order Canceled" = "brown"
      )) +
      labs(title = paste("Торговая стратегия -", result$data_source),
           subtitle = paste("Канал:", input$length, "дней | SMA (выход):", input$sma_period,
                            "| Take Profit:", input$take_profit, "%",
                            "| Отмена ордеров:", ifelse(result$enable_cancel, "ВКЛ", "ВЫКЛ")),
           x = "Дата", y = "Цена") +
      dark_theme +
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
          TRUE ~ "white"
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
    # Темная тема для ggplot
    dark_theme <- theme(
      plot.background = element_rect(fill = "#1e1e1e", color = "#1e1e1e"),
      panel.background = element_rect(fill = "#252526"),
      panel.grid.major = element_line(color = "#3d3d3d", size = 0.2),
      panel.grid.minor = element_line(color = "#3d3d3d", size = 0.1),
      text = element_text(color = "#e0e0e0"),
      axis.text = element_text(color = "#e0e0e0"),
      legend.background = element_rect(fill = "#252526"),
      legend.key = element_rect(fill = "#252526"),
      legend.text = element_text(color = "#e0e0e0")
    )
    
    p <- ggplot(equity_df, aes(x = Date, y = Equity)) +
      geom_line(color = "yellow", size = 1) +
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
      dark_theme+
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
        
        tp_trades <- executed_trades[grepl("Take Profit", executed_trades$Reason), ]
        tp_count <- nrow(tp_trades)
        tp_profit <- ifelse(tp_count > 0, sum(tp_trades$Profit, na.rm = TRUE), 0)
        
        # Расчет Profit Factor
        gross_profit <- sum(executed_trades$Profit[executed_trades$Profit > 0], na.rm = TRUE)
        gross_loss <- abs(sum(executed_trades$Profit[executed_trades$Profit < 0], na.rm = TRUE))
        
        if (gross_loss == 0) {
          profit_factor <- Inf  # Бесконечность, если нет убытков
        } else {
          profit_factor <- round(gross_profit / gross_loss, 2)
        }
        
        data.frame(
          Метрика = c("Режим", "Всего сделок", "Прибыльных сделок", 
                      "Процент прибыльных", "Общая прибыль", 
                      "Макс. прибыль", "Макс. убыток", "Конечный капитал",
                      "Макс. капитал", "Мин. капитал", "Макс. просадка",
                      "Отмена ордеров", "Take Profit", "Сделки с TP", "Прибыль от TP","Profit Factor"),
          Значение = c(result$data_source,
                       total_trades, 
                       profitable_trades,
                       paste0(round(win_rate * 100, 1), "%"), 
                       round(total_profit, 2),
                       round(max_profit, 2),
                       round(max_loss, 2),
                       format(round(result$final_capital, 2), nsmall = 2, big.mark = ","),# round(result$final_capital, 2),
                       format(round(max_equity, 2), nsmall = 2, big.mark = ","), #round(max_equity, 2),
                       format(round(min_equity, 2), nsmall = 2, big.mark = ","), #round(min_equity, 2), 
                       paste0(max_drawdown, "%"),
                       ifelse(result$enable_cancel, "ВКЛ", "ВЫКЛ"),
                       paste0(input$take_profit, "%"),
                       tp_count,
                       round(tp_profit, 2),
                       ifelse(is.infinite(profit_factor), "100", as.character(profit_factor)))
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
