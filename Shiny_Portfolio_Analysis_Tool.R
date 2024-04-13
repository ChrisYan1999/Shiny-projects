library(shiny)
library(quantmod)

ui = fluidPage(
  titlePanel('Portfolio Analysis'),
  sidebarLayout(
    sidebarPanel(
      textInput('stock1', label = 'First Stock Ticker', value = 'AAPL'),
      numericInput('weight1', label = 'Weight for First Stock', 
                   value = 0.33, min = 0, max = 1, step = 0.01),
      textInput('stock2', label = 'Second Stock Ticker', value = 'GOOG'),
      numericInput('weight2', label = 'Weight for Second Stock', 
                   value = 0.33, min = 0, max = 1, step = 0.01),
      textInput('stock3', label = 'Third Stock Ticker', value = 'MSFT'),
      numericInput('weight3', label = 'Weight for Third Stock', 
                   value = 0.34, min = 0, max = 1, step = 0.01),
      dateRangeInput('dateRange', 'Date Range', 
                     start = Sys.Date() - 365*2, 
                     end = Sys.Date()), 
      actionButton('runAnalysis', 'Run')
    ),
    mainPanel(
      tabsetPanel(type = 'tabs',
                  tabPanel('Histogram of Portfolio Returns', 
                           plotOutput('portfolioReturns')),
                  tabPanel('Portfolio Statistics', 
                           verbatimTextOutput('portfolioStats'))
      )
    )
  )
)

PortfolioVol <- function(S, w = NULL) {
  if(is.null(w)) {
    n = NCOL(S)
    w = rep(1 / n, n)
  }
  w = matrix(w, length(w), 1)  
  return(sqrt(t(w) %*% S %*% w))
}

server = function(input, output) {
  
  portfolio_data = eventReactive(input$runAnalysis, {
    req(input$stock1, input$stock2, input$stock3)
    
    symbols = c(input$stock1, input$stock2, input$stock3)
    weights_input = c(input$weight1, input$weight2, input$weight3)
    weights = weights_input / sum(weights_input)
    
    stock1 = getSymbols(input$stock1, from = input$dateRange[1], to = input$dateRange[2], auto.assign = FALSE)
    stock2 = getSymbols(input$stock2, from = input$dateRange[1], to = input$dateRange[2], auto.assign = FALSE)
    stock3 = getSymbols(input$stock3, from = input$dateRange[1], to = input$dateRange[2], auto.assign = FALSE)
    
    stock1_prices = Cl(stock1)
    stock2_prices = Cl(stock2)
    stock3_prices = Cl(stock3)
    
    stock_prices = merge.xts(stock1_prices, stock2_prices, stock3_prices)
    stock_prices = na.omit(stock_prices)
    stock_returns = lapply(stock_prices, dailyReturn)
    returns_merged = do.call(merge.xts, stock_returns)
    
    S = cov(as.matrix(returns_merged))
    
    vol = PortfolioVol(S, weights)
    
    avg_daily_returns = colMeans(returns_merged)
    
    portfolio_return = sum(weights * avg_daily_returns)
    
    sharpe_ratio = portfolio_return / vol
    
    list(returns_merged = returns_merged,
         volatility = vol,
         avg_daily_returns = avg_daily_returns,
         portfolio_return = portfolio_return,
         sharpe_ratio = sharpe_ratio)
  })
  
  output$portfolioReturns = renderPlot({
    data = portfolio_data()
    req(data)
    hist(as.vector(as.matrix(data$returns_merged)), breaks = 50, col = 'blue', 
         main = 'Histogram of Portfolio Returns', xlab = 'Returns')
  })
  
  output$portfolioStats = renderText({
    data = portfolio_data()
    req(data)
    
    paste('Average Daily Returns:', toString(round(data$avg_daily_returns, 6)), '\n',
          'Portfolio Volatility:', round(data$volatility, 6), '\n',
          'Portfolio Return:', round(data$portfolio_return, 6), '\n',
          'Sharpe Ratio:', round(data$sharpe_ratio, 6))
  })
}

shinyApp(ui, server)
