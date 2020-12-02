suppressMessages({
  library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)
  library(forecast)
  library(prophet)
  library(qgam)
  library(lubridate)
})

#' Preprocess raw weekly sales data.
#'
#' Creates new columns for Primary Key, Day, Weekday,
#' Month, Year, and each holiday (i.e., Christmas,
#' Thanksgiving, Labor Day, Superbowl and Easter).
#' Primary Key is the factor interaction between Store
#' and Department.
#'
#' @param raw_df A data frame of raw weekly sales data.
#' Must contain columns named Date, Store, Dept, and
#' IsHoliday. The Date column must be formated as "%m/%d/%Y".
#'
#' @return A data frame.
#'
#' @examples
#' test_df <- data.frame(Date = rep('12/01/2020', times = 10),
#'                       Store = rep(c('1', '2'), times = 5),
#'                       Dept = rep(c('3', '4'), times = 5),
#'                       IsHoliday = rep(0, times = 10))
#' processed_df <- preprocess(test_df)
#' head(processed_df)
preprocess <- function(raw_df){
  #convert data types
  raw_df$Date <- as.Date(raw_df$Date, format = "%m/%d/%Y")
  raw_df$Store <- as.factor(raw_df$Store)
  raw_df$Dept <- as.factor(raw_df$Dept)

  #compute new variables
  raw_df$Primary_Key <- interaction(raw_df$Store, raw_df$Dept)
  raw_df$Weekly_Sales_boxcox <- BoxCox(raw_df$Weekly_Sales, 0.3)
  raw_df$Day <- as.numeric(strftime(raw_df$Date, '%d'))
  raw_df$Weekday <- as.factor(strftime(raw_df$Date, '%A'))
  raw_df$Week <-  as.factor(strftime(raw_df$Date, '%V'))
  raw_df$Month <- as.factor(strftime(raw_df$Date, '%B'))
  raw_df$Year <- as.factor(strftime(raw_df$Date, '%y'))
  raw_df$DayMonthNormalized <- raw_df$Day/days_in_month(raw_df$Date)
  raw_df$DayYearNormalized <- yday(raw_df$Date)/yday(as.Date(paste0("12/31/", raw_df$Year), format = "%m/%d/%Y"))
  raw_df$DaysFromStart <- as.numeric(raw_df$Date - min(raw_df$Date))
  raw_df$IsChristmas <- as.logical(raw_df$IsHoliday & raw_df$Month == 'December')
  raw_df$IsThanksgiving <- as.logical(raw_df$IsHoliday & raw_df$Month == 'November')
  raw_df$IsLaborDay <- as.logical(raw_df$IsHoliday & raw_df$Month == 'September')
  raw_df$IsSuperBowl <- as.logical(raw_df$IsHoliday & raw_df$Month == 'February')
  raw_df$IsEaster <- as.logical(raw_df$Date == '2010-04-09' | raw_df$Date == '2011-04-29' | raw_df$Date == '2012-04-13')

  #compute holiday lags for qgam
  n <- nrow(raw_df)
  raw_df$OneWeekBeforeEaster <- as.factor(c(as.numeric(raw_df$IsEaster[2:n]), 0))
  raw_df$TwoWeeksBeforeEaster <- as.factor(c(as.numeric(raw_df$IsEaster[3:n]), 0, 0))
  raw_df$OneWeekAfterEaster <- as.factor(c(0, as.numeric(raw_df$IsEaster[1:(n-1)])))
  raw_df$TwoWeeksAfterEaster <- as.factor(c(0, 0, as.numeric(raw_df$IsEaster[1:(n-2)])))
  raw_df$WeekOfEaster <- as.factor(raw_df$IsEaster)

  raw_df$OneWeekBeforeChristmas <- as.factor(c(as.numeric(raw_df$IsChristmas[2:n]), 0))
  raw_df$TwoWeeksBeforeChristmas <- as.factor(c(as.numeric(raw_df$IsChristmas[3:n]), 0, 0))
  raw_df$OneWeekAfterChristmas <- as.factor(c(0, as.numeric(raw_df$IsChristmas[1:(n-1)])))
  raw_df$TwoWeeksAfterChristmas <- as.factor(c(0, 0, as.numeric(raw_df$IsChristmas[1:(n-2)])))
  raw_df$WeekOfChristmas <- as.factor(raw_df$IsChristmas)

  raw_df$OneWeekBeforeThanksgiving <- as.factor(c(as.numeric(raw_df$IsThanksgiving[2:n]), 0))
  raw_df$TwoWeeksBeforeThanksgiving <- as.factor(c(as.numeric(raw_df$IsThanksgiving[3:n]), 0, 0))
  raw_df$OneWeekAfterThanksgiving <- as.factor(c(0, as.numeric(raw_df$IsThanksgiving[1:(n-1)])))
  raw_df$TwoWeeksAfterThanksgiving <- as.factor(c(0, 0, as.numeric(raw_df$IsThanksgiving[1:(n-2)])))
  raw_df$WeekOfThanksgiving <- as.factor(raw_df$IsThanksgiving)

  raw_df$OneWeekBeforeSuperBowl <- as.factor(c(as.numeric(raw_df$IsSuperBowl[2:n]), 0))
  raw_df$TwoWeeksBeforeSuperBowl <- as.factor(c(as.numeric(raw_df$IsSuperBowl[3:n]), 0, 0))
  raw_df$OneWeekAfterSuperBowl <- as.factor(c(0, as.numeric(raw_df$IsSuperBowl[1:(n-1)])))
  raw_df$TwoWeeksAfterSuperBowl <- as.factor(c(0, 0, as.numeric(raw_df$IsSuperBowl[1:(n-2)])))
  raw_df$WeekOfSuperBowl <- as.factor(raw_df$IsSuperBowl)

  raw_df$OneWeekBeforeLaborDay <- as.factor(c(as.numeric(raw_df$IsLaborDay[2:n]), 0))
  raw_df$TwoWeeksBeforeLaborDay <- as.factor(c(as.numeric(raw_df$IsLaborDay[3:n]), 0, 0))
  raw_df$OneWeekAfterLaborDay <- as.factor(c(0, as.numeric(raw_df$IsLaborDay[1:(n-1)])))
  raw_df$TwoWeeksAfterLaborDay <- as.factor(c(0, 0, as.numeric(raw_df$IsLaborDay[1:(n-2)])))
  raw_df$WeekOfLaborDay <- as.factor(raw_df$IsLaborDay)

  return(raw_df)
}

#' Split training and test data for modeling.
#'
#' @param weekly_df A data frame of weekly sales data.
#' Must contain a Date column.
#'
#' @param weeks Number of weeks to reserve for testing.
#' Test data will be taken from the last weeks in the
#' input data frame. Must be less than the total number of
#' weeks in the input data frame.
#'
#' @return A list of two data frames named train and test.
#'
#' @examples
#' test_df <- data.frame(Date = c('12/01/2020', '12/08/2020'),
#'                       Weekly_Sales = c(1000, 2000))
#' split_list <- train_test_split(test_df, weeks = 1)
#' head(split_list$train)
#' head(split_list$test)
train_test_split <- function(weekly_df, weeks = 3){
  last_weeks <- sort(unique(weekly_df$Date), decreasing = TRUE)[1:weeks]
  test <- weekly_df[weekly_df$Date %in% last_weeks,]
  train <- weekly_df[!(weekly_df$Date %in% last_weeks),]
  return(list('train' = train,
              'test' = test))
}

#' Get the data frame corresponding to a given Primary Key.
#'
#' Given a multi-key data frame and the desired key,
#' returns the sub-frame with Primary_Key equal to key.
#' Can optionally fill missing weeks in the series with NA.
#'
#' @param multi_key_df A data frame of weekly sales data.
#' Must contain columns for Date and Primary_Key.
#'
#' @param key Level of the Primary_Key column to filter for.
#'
#' @param add_na Boolean specifying whether to fill missing
#' weeks with NA.
#'
#' @return A data frame.
#'
#' @examples
#' test_df <- data.frame(Date = c('12/01/2020', '12/07/2020', '12/01/2020'),
#'                       Primary_Key = c('1.1', '1.1', '1.2'),
#'                       Weekly_Sales = c(1000, 2000, 1500))
#' key_df <- get_key_df(test_df, key = '1.2', add_na = FALSE)
#' key_df_na <- get_key_df(test_df, key = '1.2', add_na = TRUE)
#' head(key_df)
#' head(key_df_na)
get_key_df <- function(multi_key_df, key, add_na = TRUE){
  key_df <- multi_key_df %>%
    filter(Primary_Key == key)
  if (add_na){
    num_dates <- length(unique(multi_key_df$Date))
    dates_df <- data.frame('Date' = sort(unique(multi_key_df$Date)),
                           'Primary_Key' = rep(key, num_dates))
    suppressMessages({
      key_df <- left_join(dates_df, key_df)
    })
  }
  return(key_df)
}

#' Produce a nested data frame for each key.
#'
#' Given a multi-key data frame, nests it by each level
#' of Primary_Key. Can optionally fill missing weeks in
#' the series with NA.
#'
#' @param multi_key_df A data frame of weekly sales data.
#' Must contain columns for Date and Primary_Key.
#'
#' @param add_na Boolean specifying whether to fill missing
#' weeks with NA.
#'
#' @return A data frame two columns: Primary_Key and data.
#' The data column is a list column that contains a separate
#' data frame for each level of Primary_Key.
#'
#' @examples
#' test_df <- data.frame(Date = c('12/01/2020', '12/07/2020', '12/01/2020'),
#'                       Primary_Key = c('1.1', '1.1', '1.2'),
#'                       Weekly_Sales = c(1000, 2000, 1500))
#' nested_df <- nest_by_key(test_df, add_na = FALSE)
#' nested_df_na <- nest_by_key(test_df, add_na = TRUE)
#' head(nested_df)
#' head(nested_df_na)
nest_by_key <- function(multi_key_df, add_na = TRUE){
  multi_key_df <- multi_key_df %>%
    select(Date, Weekly_Sales, Primary_Key,
           DaysFromStart, DayYearNormalized, DayMonthNormalized,
           TwoWeeksBeforeEaster, OneWeekBeforeEaster, WeekOfEaster,
           OneWeekAfterEaster, TwoWeeksAfterEaster, TwoWeeksBeforeThanksgiving,
           OneWeekBeforeThanksgiving, WeekOfThanksgiving)
  if (add_na){
    num_keys <- length(unique(multi_key_df$Primary_Key))
    num_dates <- length(unique(multi_key_df$Date))
    dates_df <- data.frame('Date' = rep(sort(unique(multi_key_df$Date)), times = num_keys),
                           'Primary_Key' = rep(unique(multi_key_df$Primary_Key), each = num_dates))
    suppressMessages({
      multi_key_df <- left_join(dates_df, multi_key_df)
    })
  }
  nested_by_key <- multi_key_df %>%
    group_by(Primary_Key) %>%
    nest
  return(nested_by_key)
}

#' Produce a holiday data frame formatted for Prophet.
#'
#' @param train A data frame of weekly sales training
#' data. Must a Date column and logical columns for each
#' of the holidays of interest. The names of the holiday
#' columns should be formatted in camel case as IsHoliday
#' where "Holiday" is the holiday of interest (e.g.,
#' IsChristmas, IsEaster, etc.).
#'
#' @param test A data frame of weekly sales testing
#' data. Must be formatted similarly to the train data frame.
#'
#' @param holidays A list of holidays to include. For each
#' holiday, a corresponding "IsHoliday" column must be present
#' the train and test data frames.
#'
#' @param lower_window Int specifying a range of days prior to
#' the date to be included as holidays. For example,
#' lower_window = -14 will include the two weeks prior to the
#' date as holidays. Must be a negative number. See Prophet
#' docs for more information.
#'
#' @param upper_window Int specifying a range of days after
#' the date to be included as holidays. For example,
#' upper_window = 14 will include 2 weeks after the date as
#' holidays. Must be a positive number. See Prophet
#' docs for more information.
#'
#' @return A data frame with columns holiday (character) and
#' ds (date type) and optionally columns lower_window and
#' upper_window which specify a range of days around the date
#' to be included as holidays.
#'
#' @examples
#' train_start_date <- as.Date('06/12/2019', format = '%d/%m/%Y')
#' test_start_date <- as.Date('04/12/2020', format = '%d/%m/%Y')
#' train_df <- data.frame(Date = seq(train_start_date, length.out = 10, by = 7),
#'                       IsChristmas = as.logical(c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0)))
#' test_df <- data.frame(Date = seq(test_start_date, length.out = 10, by = 7),
#'                       IsChristmas = as.logical(c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0)))
#' holiday_df <- get_holiday_df(train_df, test_df,
#' holidays = c('Christmas'),
#' lower_window = -14, upper_window = 7)
#' head(holiday_df)
get_holiday_df <- function(train, test, holidays, lower_window = 0, upper_window = 0){

  capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                             {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  }

  holiday_col <- c()
  ds_col <- c()
  for (h in holidays){
    bool_name <- paste0('Is', capwords(h))
    isHoliday_train <- train[, colnames(train) == bool_name]
    isHoliday_test <- test[, colnames(test) == bool_name]
    num_holiday <- length(unique(train$Date[isHoliday_train])) + length(unique(test$Date[isHoliday_test]))
    holiday_col <- c(holiday_col, rep(h, num_holiday))
    holiday_dates <- c(unique(train$Date[isHoliday_train]),
                               unique(test$Date[isHoliday_test]))
    ds_col <- c(ds_col, holiday_dates)
  }
  holiday_df <- data.frame(holiday = holiday_col,
                           ds = as.Date(ds_col, origin ="1970-01-01"),
                           lower_window = lower_window,
                           upper_window = upper_window)
  return(holiday_df)
}

#' Perform time series cross-validation on all models.
#'
#' Given weekly sales data for a single key, and a list of
#' cutoff dates, trains on data ocurring before the cutoff
#' dates and tests on data ocurring after. Trains a separate
#' model for each cutoff date.
#'
#' @param data A data frame of weekly sales data.
#' Must contain columns for Date, Primary_Key, and
#' Weekly_Sales. Weeks with missing data should be filled
#' with NA.
#'
#' @param cutoff_dates A list of cutoff dates for creating
#' train and test splits on each cross-validation loop.
#'
#' @param holidays A holiday data frame formatted for use
#' with Prophet.
#'
#' @return A data frame with the following columns: error,
#' obs, mean_diff, med_diff, horizon, and model. The error
#' column is yhat - y for each predicition. It is set to NA
#' for preditction with no corresponding observation. The
#' obs column is the corresponding observations. The
#' mean_diff column is the mean of the diff of the weekly
#' sales for each set of training data. The med_diff column
#' is the median instead of the mean. The horizon column
#' gives the number of weeks each prediciton is from the
#' last date in the training data. The model column gives
#' the name of the model that made each prediction.
ts_cv <- function(data, cutoff_dates, holidays, fit_qgam = FALSE) {
  if (sum(!is.na(data$Weekly_Sales)) < 100){
    return(NA)
  }
  tryCatch(
    {
      #rename cols
      data <- data %>%
        rename(ds = Date) %>%
        rename(y = Weekly_Sales)

      #init vars
      prophet_error <- c()
      qgam_error <- c()
      naive_error <- c()
      meanf_error <- c()
      medf_error <- c()
      obs <- c()
      mean_diff <- c()
      med_diff <- c()

      for (i in seq(1, length(cutoff_dates))) {
        #get train and test sets
        date_idx <- which(data$ds == cutoff_dates[i])
        train <- data[1:date_idx,]
        test <- data[(date_idx+1):(date_idx+3),]

        #fit prophet
        prophet_model <- prophet(growth = 'linear',
                                 yearly.seasonality = 6,
                                 weekly.seasonality = FALSE,
                                 daily.seasonality = FALSE,
                                 seasonality.mode = "additive",
                                 holidays = holidays,
                                 fit = FALSE)
        prophet_model <- add_seasonality(prophet_model, name='monthly', period=30.5, fourier.order=1)
        prophet_model <- fit.prophet(prophet_model, df = train)

        #get forecast
        future = make_future_dataframe(prophet_model, periods = 3, freq = 'week')
        prophet_forecast = predict(prophet_model, future)

        if (fit_qgam){
          #fit qgam
          qgam_model <- qgam(y ~ WeekOfEaster +
                               OneWeekBeforeEaster + TwoWeeksBeforeEaster +
                               TwoWeeksBeforeThanksgiving + OneWeekBeforeThanksgiving +
                               WeekOfThanksgiving +
                               s(DaysFromStart, bs = "gp", k=45) +
                               s(DayYearNormalized, bs = "ad", k=52) +
                               s(DayMonthNormalized, bs = "cp"),
                             control = list(progress = FALSE),
                             qu = 0.5,
                             data = train)

          #get forecast
          qgam_forecast <- predict(object = qgam_model,
                                   newdata = test,
                                   type = "response")
        }

        #get test errors
        prophet_error <- c(prophet_error, prophet_forecast$yhat[-(1:nrow(train))] - test$y)
        if (fit_qgam){
          qgam_error <- c(qgam_error, qgam_forecast - test$y)
        }
        naive_error <- c(naive_error, train$y[nrow(train)] - test$y)
        meanf_error <- c(meanf_error, mean(train$y, na.rm = TRUE) - test$y)
        medf_error <- c(medf_error, median(train$y, na.rm = TRUE) - test$y)
        obs <- c(obs, test$y)
        mean_diff <- c(mean_diff, rep(mean(abs(diff(train$y)), na.rm = TRUE), 3))
        med_diff <- c(med_diff, rep(median(abs(diff(train$y)), na.rm = TRUE), 3))
      }
      if (fit_qgam) {
        error_df <- tibble('error' = c(prophet_error, qgam_error, naive_error, meanf_error, medf_error),
                           'obs' = rep(obs, times = 5),
                           'mean_diff' = rep(mean_diff, times = 5),
                           'med_diff' = rep(med_diff, times = 5),
                           'horizon' = rep(seq(1, 3), times = 5*length(cutoff_dates)),
                           'model' = rep(c('prophet', 'qgam', 'naive', 'meanf', 'medf'), each = 3*length(cutoff_dates)))
      } else {
        error_df <- tibble('error' = c(prophet_error, naive_error, meanf_error, medf_error),
                           'obs' = rep(obs, times = 4),
                           'mean_diff' = rep(mean_diff, times = 4),
                           'med_diff' = rep(med_diff, times = 4),
                           'horizon' = rep(seq(1, 3), times = 4*length(cutoff_dates)),
                           'model' = rep(c('prophet', 'naive', 'meanf', 'medf'), each = 3*length(cutoff_dates)))
      }
      return(error_df)
    },
    error=function(cond) {
      # print(cond)
      return(NA)
    },
    warning=function(cond) {
      # print(cond)
      return(NA)
    }
  )
}

#' Compute common forecasting performance metrics.
#'
#' Computes rmse, mae, mape, mase, and mamse from an
#' error data frame returned by ts_cv.
#'
#' @param error_df The data frame returned by ts_cv (see
#' ?ts_cv)
#'
#' @return A data frame with the following columns: model,
#' horizon, metric, and value. The model column gives the
#' name of the forecasting model being measured. The
#' horizon column gives the number of weeks each prediciton
#' is from the last date in the training data. The metric
#' column gives the name of the computed performance metric.
#' It can be any of: rmse, mae, mape, mase, or mamse. The
#' value column gives the estimated value of the performance
#' metric.
gather_metrics <- function(error_df) {
  tryCatch(
    {
      metric_df <- error_df %>%
        group_by(model, horizon) %>%
        summarise(rmse = sqrt(mean(error^2, na.rm = TRUE)),
                  mae = mean(abs(error), na.rm = TRUE),
                  mape = 100*mean(abs(error)/obs, na.rm = TRUE),
                  mase = mean(abs(error)/mean_diff, na.rm = TRUE),
                  mamse = mean(abs(error)/med_diff, na.rm = TRUE),
                  .groups = 'drop') %>%
        pivot_longer(cols = c('rmse', 'mae', 'mape', 'mase', 'mamse'),
                     names_to = 'metric',
                     values_to = 'value')
    },
    error=function(cond) {
      return(NA)
    },
    warning=function(cond) {
      return(NA)
    }
  )
}

#' Count the number of weeks with sales reported.
#'
#' Returns the number of rows with non-NA values for the
#' Weekly_Sales column.
#'
#' @param df A data frame of weekly sales data. Must
#' contain a Weekly_Sales column.
#'
#' @return An int giving the number of rows with non-NA
#' values for the Weekly_Sales column.
#'
#' @examples
#' test_df <- data.frame(Date = c('12/01/2020', '12/07/2020'),
#'                       Primary_Key = c('1.1', '1.1'),
#'                       Weekly_Sales = c(1000, NA))
#' n <- num_obs(test_df)
#' print(n)
num_obs <- function(df){
  sum(as.numeric(!is.na(df$Weekly_Sales)))
}

#' Get the smallest reported sales.
#'
#' Returns the min of the Weekly_Sales column.
#'
#' @param df A data frame of weekly sales data. Must
#' contain a Weekly_Sales column.
#'
#' @return An int giving the min of the Weekly_Sales
#' column.
#'
#' @examples
#' test_df <- data.frame(Date = c('12/01/2020', '12/07/2020'),
#'                       Primary_Key = c('1.1', '1.1'),
#'                       Weekly_Sales = c(1000, NA))
#' n <- min_sales(test_df)
#' print(n)
min_sales <- function(df){
  min(df$Weekly_Sales, na.rm = TRUE)
}
