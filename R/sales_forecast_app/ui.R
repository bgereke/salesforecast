#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Weekly Sales Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId = "store_selection", 
                         label = "Store:", 
                         value = 1, 
                         min = 1, 
                         max = 45,
                         step = 1),
            
            numericInput(inputId = "dept_selection", 
                         label = "Department:", 
                         value = 1, 
                         min = 1, 
                         max = 81,
                         step = 1),
            
            numericInput(inputId = "lambda", 
                         label = "Box Cox:", 
                         value = 1, 
                         min = 0, 
                         max = 1,
                         step = 0.1),
            
            sliderInput(inputId = "yearly.seasonality", 
                        label = "Yearly Seasonality:",
                        value = 6,
                        min = 0, 
                        max = 25, 
                        step = 1),
            
            sliderInput(inputId = "monthly.seasonality", 
                        label = "Monthly Seasonality:",
                        value = 1,
                        min = 0, 
                        max = 5, 
                        step = 1),

            selectInput(inputId = "seasonality.mode",
                        label = "Seasonality Mode:",
                        choices = c("additive" = "additive",
                                    "multiplicative" = "multiplicative"),
                        selected = "additive"),
            
            numericInput(inputId = "seasonality.prior.scale", 
                         label = "Seasonality Prior Scale:", 
                         value = 10, 
                         min = 0, 
                         max = 100),
            
            numericInput(inputId = "holidays.prior.scale", 
                         label = "Holidays Prior Scale:", 
                         value = 10, 
                         min = 0, 
                         max = 100),
            
            numericInput(inputId = "changepoint.prior.scale", 
                         label = "Changepoint Prior Scale:", 
                         value = 0.05, 
                         min = 0, 
                         max = 100),
            
            sliderInput(inputId = "n.changepoints", 
                        label = "# Changepoints:",
                        value = 25,
                        min = 0, 
                        max = 25, 
                        step = 1),
            
            sliderInput(inputId = "changepoint.range", 
                        label = "Changepoint Range:",
                        value = 0.8,
                        min = 0, 
                        max = 1, 
                        step = 0.1),
            
            actionButton("get_forecast", "Get Forecast!"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Forecasts", plotOutput("forecastPlot")), 
                tabPanel("Prophet Components", plotOutput("componentsPlot"))
            )
        )
    )
))
