#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(prophet)
library(tidyverse)
library(forecast)
source('/home/brian/R/salesforecast/R/helpers.R')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # read, preprocess and split data
    processed <- read.csv(file = '/home/brian/R/salesforecast/data/train (1).csv') %>%
        preprocess %>%
        train_test_split(weeks = 3)
    train <- processed[[1]]
    test <- processed[[2]]
    rm(processed)

    #get test dates for plotting
    test_dates <- data.frame('Date' = unique(test$Date))

    #get holidays into prophet format
    holiday_df <- get_holiday_df(train,
                                 test,
                                 holidays = c('Christmas',
                                              'Thanksgiving',
                                              'LaborDay',
                                              'SuperBowl',
                                              'Easter'),
                                 lower_window = -14,
                                 upper_window = 14)

    get_prophet_result <- eventReactive(input$get_forecast, {
        #get user-selected data
        key_train <- train %>%
            filter(Store == input$store_selection,
                   Dept == input$dept_selection) %>%
            rename(ds = Date) %>%
            mutate(y = BoxCox(Weekly_Sales, lambda = input$lambda))

        key_test <- test %>%
            filter(Store == input$store_selection,
                   Dept == input$dept_selection)

        if (nrow(key_train) > 85){
            if (input$yearly.seasonality == 0) {
                input$yearly.seasonality = FALSE
            }

            #fit model
            prophet_model <- prophet(growth = 'linear',
                                     yearly.seasonality = input$yearly.seasonality,
                                     weekly.seasonality = FALSE,
                                     daily.seasonality = FALSE,
                                     seasonality.mode = input$seasonality.mode,
                                     holidays = holiday_df,
                                     seasonality.prior.scale = input$seasonality.prior.scale,
                                     holidays.prior.scale = input$holidays.prior.scale,
                                     changepoint.prior.scale = input$changepoint.prior.scale,
                                     n.changepoints = input$n.changepoints,
                                     changepoint.range = input$changepoint.range,
                                     fit = FALSE)
            if (input$monthly.seasonality > 0) {
                prophet_model <- add_seasonality(prophet_model, name='monthly', period=30.5, fourier.order=1)
            }
            prophet_model <- fit.prophet(prophet_model, df = key_train)

            #get forecast
            future <- make_future_dataframe(prophet_model,
                                            periods = 3,
                                            freq = 'week')
            forecast <- predict(prophet_model, future)
            forecast$yhat <- InvBoxCox(forecast$yhat, lambda = input$lambda)

        } else {
            prophet_model <- NA
            forecast <- NA
        }
        return(list(key_train, key_test, prophet_model, forecast))
    })

    output$forecastPlot <- renderPlot({
        prophet_result <- get_prophet_result()
        key_train <- prophet_result[[1]]
        key_test <- prophet_result[[2]]
        forecast <- prophet_result[[4]]
        if (length(forecast) > 1){
            forecast$ds <- as.Date(forecast$ds)
        }
        ggplot(data = key_train, aes(x = ds, y = Weekly_Sales)) +
            {
                if (nrow(key_train) >= 1){
                    geom_point()
                }
            } +
            {
                if (nrow(key_test) >= 1){
                    geom_point(data = key_test,
                               mapping = aes(x = Date, y = Weekly_Sales),
                               color = 'red')
                }
            } +
            {
                if (length(forecast) > 1){
                    geom_line(data = forecast,
                              mapping = aes(x = ds,
                                            y = yhat,
                                            color = '#619CFF'))
                }
            } +
            {
                if (nrow(key_train) >= 1){
                    geom_line(data = test_dates,
                              mapping = aes(x = Date,
                                            y = key_train$Weekly_Sales[key_train$ds == max(key_train$ds)],
                                            color = 'orange'))
                }
            } +
            {
                if (nrow(key_train) >= 1){
                    geom_line(data = test_dates,
                              mapping = aes(x = Date,
                                            y = median(key_train$Weekly_Sales, na.rm = TRUE),
                                            color = 'purple'))
                }
            } +
            geom_vline(xintercept = max(train$Date),
                       linetype="dotted") +
            {
                if (length(forecast) > 1){
                    scale_colour_manual(name = 'model',
                                        values =c('#619CFF'='#619CFF',
                                                  'orange'='orange',
                                                  'purple'='purple'),
                                        labels = c('prophet','naive','median'))
                } else{
                    scale_colour_manual(name = 'model',
                                        values =c('orange'='orange',
                                                  'purple'='purple'),
                                        labels = c('naive','median'))
                }
            } +
            xlab('date') + ylab('weekly sales')
    })

    output$componentsPlot <- renderPlot({
        prophet_result <- get_prophet_result()
        if (length(prophet_result[[3]]) > 1){
            prophet_plot_components(prophet_result[[3]], prophet_result[[4]])
        }
    })

})
