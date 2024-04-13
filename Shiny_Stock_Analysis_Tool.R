library(shiny)
library(quantmod)

ui = fluidPage(
  titlePanel("Tabsets"),
  
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "stockid", label = "Enter the Stock Symbol", value = "MSFT"),
      numericInput("lag", label = "Lag", value = 1, min = 1, max = 365),
      numericInput("confidence", label = "Confidence Level", value = 95, min = 1, max = 99),
      numericInput('window', label = 'Window Size', value = 100, min = 1, max = 1000),
      selectInput('metric', label = 'Select Metric', 
                  choices = c('Min' = 'min', 'Max' = 'max', 
                              'Standard Deviation' = 'sd', 'Variance' = 'var')),
      actionButton(inputId = "getdata", label = "Get Data")
    ),
    mainPanel(
      tabsetPanel(type = 'tabs',
                  tabPanel('Returns Plot', plotOutput('returnPlot')),
                  tabPanel('Rolling Metric Plot', plotOutput('metricPlot'))
      )
    )
  )
)

server = function(input, output) {
  
  stock_data = eventReactive(input$getdata, {
    req(input$stockid)
    getSymbols(input$stockid, src = "yahoo", auto.assign = FALSE)
  })
  
  stock_returns = reactive({
    req(stock_data())
    dailyReturn(stock_data(), lag = input$lag)
  })
  
  output$returnPlot = renderPlot({
    returns = stock_returns()
    hist(returns, breaks = 40, main = 'Returns Histogram',
         xlab = 'Returns', col = 'lightblue')
    
    confidence_level = input$confidence / 100
    VaR = -quantile(returns, 1 - confidence_level)
    ES = mean(returns[returns < VaR])
    
    abline(v = VaR, col = 'red', lwd = 2)
    abline(v = ES, col = 'blue', lwd = 2)
    legend('topright', legend = c(paste('VaR:', round(VaR, 4)), 
                                  paste('ES:', round(ES, 4))),
           col = c('red', 'blue'), lwd = 2)
  })
  
  output$metricPlot = renderPlot({
    req(stock_returns(), input$window, input$metric) 
    data = stock_returns()
    
    rolling_metric = switch(input$metric,
                            'min' = runMin(data, n = input$window),
                            'max' = runMax(data, n = input$window),
                            'sd' = runSD(data, n = input$window),
                            'var' = runVar(data, n = input$window))
    
    plot(rolling_metric, type = "l", main = paste('Rolling', input$metric, 'Plot'),
         ylab = input$metric, xlab = 'Time')
  })
}

shinyApp(ui = ui, server = server)