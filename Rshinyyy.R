library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  skin = "green",  
  
  dashboardHeader(title = "Time Series Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "upload", icon = icon("file-upload")),
      menuItem("EDA", tabName = "eda", icon = icon("table")),
      menuItem("Time Series Plots", tabName = "tsplots", icon = icon("chart-line")),
      menuItem("Modeling", tabName = "modeling", icon = icon("cogs")),
      menuItem("Forecasting", tabName = "forecast", icon = icon("chart-bar")),
      menuItem("Diagnostics", tabName = "diagnostics", icon = icon("wrench"))
    )
  ),
  
  dashboardBody(
    # Optional custom CSS for purple headers
    tags$head(
      tags$style(HTML("
        .box.box-solid.box-primary > .box-header {
          background: #6f42c1;
          color: white;
        }
        .box.box-solid.box-primary {
          border: 1px solid #6f42c1;
        }
      "))
    ),
    
    tabItems(
      # Upload Tab
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "Upload CSV File", width = 12, status = "primary", solidHeader = TRUE,
                    fileInput("file", "Choose CSV File", accept = ".csv"),
                    selectInput("column", "Select Time Series Column", choices = NULL)
                )
              )
      ),
      
      # EDA Tab
      tabItem(tabName = "eda",
              fluidRow(
                box(title = "Exploratory Data Analysis", width = 12, status = "primary", solidHeader = TRUE,
                    verbatimTextOutput("edaSummary")
                )
              )
      ),
      
      # Time Series Plots Tab
      tabItem(tabName = "tsplots",
              fluidRow(
                box(title = "Original Time Series", width = 12, status = "primary", solidHeader = TRUE, plotOutput("originalPlot"))
              ),
              fluidRow(
                box(title = "Stationary Time Series", width = 12, status = "primary", solidHeader = TRUE, plotOutput("stationaryPlot"))
              ),
              fluidRow(
                box(title = "Original ACF", width = 12, status = "primary", solidHeader = TRUE, plotOutput("originalACF"))
              ),
              fluidRow(
                box(title = "Original PACF", width = 12, status = "primary", solidHeader = TRUE, plotOutput("originalPACF"))
              ),
              fluidRow(
                box(title = "Stationary ACF", width = 12, status = "primary", solidHeader = TRUE, plotOutput("acfPlot"))
              ),
              fluidRow(
                box(title = "Stationary PACF", width = 12, status = "primary", solidHeader = TRUE, plotOutput("pacfPlot"))
              )
      ),
      
      # Modeling Tab
      tabItem(tabName = "modeling",
              fluidRow(
                box(title = "Model Selection", width = 12, status = "primary", solidHeader = TRUE,
                    selectInput("modelType", "Select Model Type", 
                                choices = c("Auto ARIMA" = "auto", "ARIMA" = "arima", 
                                            "SARIMA" = "sarima", "ARCH" = "arch", "GARCH" = "garch")),
                    
                    numericInput("p", "AR Order (p)", value = 1, min = 0),
                    numericInput("d", "Differencing Order (d)", value = 0, min = 0),
                    numericInput("q", "MA Order (q)", value = 1, min = 0),
                    
                    conditionalPanel(
                      condition = "input.modelType == 'sarima'",
                      numericInput("P", "Seasonal AR Order (P)", value = 1, min = 0),
                      numericInput("D", "Seasonal Differencing Order (D)", value = 0, min = 0),
                      numericInput("Q", "Seasonal MA Order (Q)", value = 1, min = 0),
                      numericInput("s", "Seasonal Period (s)", value = 12, min = 1)
                    ),
                    
                    actionButton("fit", "Fit Model"),
                    verbatimTextOutput("modelSuggestion")
                )
              ),
              fluidRow(
                box(title = "Model Summary", width = 12, status = "primary", solidHeader = TRUE,
                    verbatimTextOutput("modelSummary")
                )
              )
      ),
      
      # Forecasting Tab
      tabItem(tabName = "forecast",
              fluidRow(
                box(title = "Forecast Settings", width = 12, status = "primary", solidHeader = TRUE,
                    numericInput("forecastHorizon", "Forecast Horizon", value = 10, min = 1),
                    conditionalPanel(
                      condition = "input.modelType == 'garch'",
                      selectInput("garchForecastType", "GARCH Forecast Type", 
                                  choices = c("Unconditional" = "uncond", "Rolling" = "roll"))
                    )
                )
              ),
              fluidRow(
                box(title = "Forecast Plot", width = 12, status = "primary", solidHeader = TRUE, plotOutput("forecastPlot"))
              )
      ),
      
      # Diagnostics Tab
      tabItem(tabName = "diagnostics",
              fluidRow(
                box(title = "Residual Analysis", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("residualPlot"),
                    plotOutput("residualACF")
                )
              ),
              fluidRow(
                box(title = "Volatility Plot (only for GARCH)", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("volatilityPlot")
                )
              )
      )
    )
  )
)

library(shiny)
library(forecast)
library(tseries)
library(rugarch)
library(ggplot2)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    df <- df[complete.cases(df), ]
    df <- df[!duplicated(df), ]
    output$edaSummary <- renderText({ summary(df) })
    return(df)
  })
  
  observe({ updateSelectInput(session, "column", choices = names(data())) })
  
  tsData <- reactive({ 
    req(input$column) 
    ts(data()[[input$column]]) 
  })
  
  output$originalPlot <- renderPlot({ 
    autoplot(tsData()) + ggtitle("Original Time Series") 
  })
  
  output$originalACF <- renderPlot({ 
    Acf(tsData(), main = "Original ACF") 
  })
  
  output$originalPACF <- renderPlot({ 
    Pacf(tsData(), main = "Original PACF") 
  })
  
  stationaryData <- reactive({
    diff_order <- ndiffs(tsData())
    transformed_ts <- if (diff_order > 0) diff(tsData(), differences = diff_order) else tsData()
    
    if (adf.test(transformed_ts)$p.value > 0.05) {
      transformed_ts <- diff(log(tsData() + 1), differences = diff_order)
    }
    ts(transformed_ts)  
  })
  
  output$stationaryPlot <- renderPlot({ 
    autoplot(stationaryData()) + ggtitle("Stationary Time Series") 
  })
  
  output$acfPlot <- renderPlot({ 
    Acf(stationaryData(), main = "Stationary ACF") 
  })
  
  output$pacfPlot <- renderPlot({ 
    Pacf(stationaryData(), main = "Stationary PACF") 
  })
  
  modelFit <- eventReactive(input$fit, {
    req(stationaryData())
    
    if (input$modelType == "auto") {
      return(auto.arima(stationaryData()))
      
    } else if (input$modelType == "arima") {
      return(Arima(stationaryData(), order = c(input$p, input$d, input$q)))
      
    } else if (input$modelType == "sarima") {
      return(Arima(stationaryData(), 
                   order = c(input$p, input$d, input$q),
                   seasonal = list(order = c(input$P, input$D, input$Q), period = input$s)))
      
    } else if (input$modelType == "arch" || input$modelType == "garch") {
      spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(input$p, input$q)))
      return(ugarchfit(spec, data = stationaryData(), out.sample = input$forecastHorizon))
    }
  })
  
  output$modelSummary <- renderPrint({ 
    req(modelFit()) 
    summary(modelFit()) 
  })
  
  output$forecastPlot <- renderPlot({
    req(modelFit())
    
    if (input$modelType %in% c("arima", "auto", "sarima")) {
      forecasted <- forecast(modelFit(), h = input$forecastHorizon)
      autoplot(forecasted) + ggtitle("Forecasted Values")
      
    } else if (input$modelType == "garch") {
      n_roll_value <- min(input$forecastHorizon - 1, input$forecastHorizon)
      spec <- ugarchforecast(modelFit(), n.ahead = input$forecastHorizon, 
                             n.roll = n_roll_value,  
                             method = input$garchForecastType)
      plot(spec, which = "all")
    }
  })
  
  output$residualPlot <- renderPlot({ 
    req(modelFit()) 
    ggplot(data.frame(residuals = residuals(modelFit())), aes(x = residuals)) +
      geom_histogram(binwidth = 0.5, fill = "blue", alpha = 0.7) +
      ggtitle("Residual Histogram") + theme_minimal()
  })
  
  output$residualACF <- renderPlot({ 
    req(modelFit()) 
    Acf(residuals(modelFit()), main = "Residual ACF") 
  })
  
  output$volatilityPlot <- renderPlot({
    req(modelFit())
    
    if (input$modelType == "garch") {
      sigma_vals <- sigma(modelFit())
      plot(sigma_vals, type = "l", col = "blue", main = "Volatility Clustering", ylab = "Sigma", xlab = "Time")
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)


# Run the application 
shinyApp(ui = ui, server = server)
