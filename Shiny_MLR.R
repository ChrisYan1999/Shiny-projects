library(shiny)
library(car)

ui = fluidPage(
  titlePanel('MLR'),
  sidebarLayout(
    sidebarPanel(
      fileInput('file', 'Upload your .csv file'),
      textInput('formula', 'Enter formula', value = 'y ~ variables'),
      actionButton('action', 'Run Regression')
    ),
    mainPanel(
      tabsetPanel(type = 'tabs',
                  tabPanel('Data', tableOutput('table')),
                  tabPanel('AVPlots', plotOutput('avplots')),
                  tabPanel('VIF', verbatimTextOutput('vif')),
                  tabPanel('Influence Plots', plotOutput('inf_plots'))
      )
    )
  )
)

server = function(input, output) {
  data = reactive({
    inFile = input$file
    if (is.null(inFile)) {
      return(NULL)
    }
    read.csv(inFile$datapath, header = TRUE)
  })
  
  output$table = renderTable({
    req(data())
    data()
  })
  
  regression = eventReactive(input$action, {
    req(data())
    req(input$formula)
    lm(as.formula(input$formula), data = data())
  })
  
  output$avplots = renderPlot({
    req(regression())
    avPlots(regression())
  })
  
  output$vif = renderPrint({
    req(regression())
    vif(regression())
  })
  
  output$inf_plots = renderPlot({
    req(regression())
    influencePlot(regression(), id.method = "identify", main = "Influence Plot")
  })
}

shinyApp(ui = ui, server = server)