# moex_data_lib.R
library(httr)
library(jsonlite)
library(dplyr)

# Функция для получения данных с MOEX за указанный период
getMOEX <- function(ticker, start_date, end_date) {
  tryCatch({
    url <- paste0('https://iss.moex.com/iss/engines/stock/markets/shares/securities/', 
                 ticker, '/candles.json?from=', start_date, 
                 '&till=', end_date, '&interval=24')
    response <- GET(url)
    ticker_data <- fromJSON(content(response, "text"))
    
    if (!is.null(ticker_data$candles$data) && length(ticker_data$candles$data) > 0) {
      test <- as.data.frame(ticker_data$candles$data)
      
      # Проверяем соответствие количества колонок
      if (ncol(test) == length(ticker_data[["candles"]][["columns"]])) {
        colnames(test) <- ticker_data[["candles"]][["columns"]]
      } else {
        # Если не совпадает, используем стандартные имена
        colnames(test) <- paste0("V", 1:ncol(test))
      }
      
      # Преобразование данных
      numeric_cols <- c("open", "high", "low", "close", "volume")
      for(col in numeric_cols) {
        if(col %in% colnames(test)) {
          test[[col]] <- as.numeric(as.character(test[[col]]))
        }
      }
      
      # Обработка дат
      if ("end" %in% colnames(test)) {
        test$end <- as.Date(test$end)
      } else if ("begin" %in% colnames(test)) {
        test$end <- as.Date(test$begin)
      } else {
        # Если нет дат, создаем последовательность
        test$end <- seq.Date(as.Date(start_date), by = "day", length.out = nrow(test))
      }
      
      test <- test[order(test$end), ]
      
      return(test)
    } else {
      warning("No data found for ", ticker, " from ", start_date, " to ", end_date)
      return(NULL)
    }
  }, error = function(e) {
    warning("Error fetching data for ", ticker, ": ", e$message)
    return(NULL)
  })
}

# Функция для получения данных за несколько лет
getMOEXMultiYear <- function(ticker, start_year = 2000) {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  all_data <- data.frame()
  
  for (year in start_year:current_year) {
    start_date <- paste0(year, "-01-01")
    end_date <- paste0(year, "-12-31")
    
    if (year == current_year) {
      end_date <- as.character(Sys.Date())
    }
    
    year_data <- getMOEX(ticker, start_date, end_date)
    
    if (!is.null(year_data) && nrow(year_data) > 0) {
      all_data <- bind_rows(all_data, year_data)
      message("Downloaded data for ", ticker, " year ", year)
    } else {
      message("No data for ", ticker, " in year ", year)
    }
    
    # Пауза между запросами
    Sys.sleep(1)
  }
  
  return(all_data)
}

# Функция для загрузки и обновления данных
loadMOEXData <- function(ticker, force_update = FALSE) {
  file_name <- paste0(ticker, ".csv")
  
  # Если файл существует и не требуется принудительное обновление
  if (file.exists(file_name) && !force_update) {
    data <- read.csv(file_name, stringsAsFactors = FALSE)
    data$end <- as.Date(data$end)
    
    # Проверяем, нужно ли обновить данные
    last_date <- max(data$end)
    if (last_date < Sys.Date() - 7) {  # Обновляем если данные старше недели
      new_data <- getMOEX(ticker, as.character(last_date + 1), as.character(Sys.Date()))
      
      if (!is.null(new_data) && nrow(new_data) > 0) {
        data <- bind_rows(data, new_data) %>%
          distinct(end, .keep_all = TRUE)  # Удаляем дубликаты
        write.csv(data, file_name, row.names = FALSE)
        message("Updated data for ", ticker)
      }
    }
    
    return(data)
  } else {
    # Загружаем все данные с 2000 года
    data <- getMOEXMultiYear(ticker, 2000)
    
    if (!is.null(data) && nrow(data) > 0) {
      write.csv(data, file_name, row.names = FALSE)
      message("Saved data to ", file_name)
      return(data)
    } else {
      stop("Failed to download data for ", ticker)
    }
  }
}

# Функция для симуляции данных (резервная)
simulate_data <- function(seed, n_days) {
  set.seed(seed)
  dates <- seq.Date(Sys.Date() - n_days, Sys.Date(), by = "day")
  prices <- 1000 * cumprod(c(1, 1 + rnorm(n_days, 0.001, 0.02)))
  
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
