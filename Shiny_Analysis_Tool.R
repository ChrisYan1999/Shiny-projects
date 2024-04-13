library(shiny)
library(dplyr)

ui <- fluidPage(
  titlePanel('Data Analysis Tool'),
  sidebarLayout(
    sidebarPanel(
      fileInput('data_upload', 'Choose CSV File', accept = '.csv'),
      uiOutput('column_selector_ui'),
      selectInput('filter_condition', 
                  'Select Condition', 
                  choices = c('Less Than' = 'less', 
                              'Equal To' = 'equal', 
                              'Greater Than' = 'greater')),
      numericInput('comparison_value', 'Comparison Value', 0),
      uiOutput('display_fields_ui'), 
      uiOutput('group_fields_ui'),      
      uiOutput('stats_selector_ui'),       
      actionButton('analyze_btn', 'Run!')
    ),
    mainPanel(
      tabsetPanel(type = 'tabs',
                  tabPanel('Filtered Data', tableOutput('filtered_data_table')),
                  tabPanel('Grouped Summary', verbatimTextOutput('grouped_summary_output'))
      )
    )
  )
)

server <- function(input, output, session) {
  
  the_data <- reactive({
    req(input$data_upload)
    read.csv(input$data_upload$datapath)
  })
  
  output$column_selector_ui <- renderUI({
    req(the_data())
    selectInput('selected_column', 
                'Select Field for Comparison',
                choices = names(the_data()))
  })
  
  output$display_fields_ui <- renderUI({
    req(the_data())
    checkboxGroupInput('fields_to_display', 
                       'Fields to Display in the Data', 
                       names(the_data()))
  })
  
  output$group_fields_ui <- renderUI({
    req(the_data())
    checkboxGroupInput('fields_to_group_by', 
                       'Fields to Group By', 
                       names(the_data()))
  })
  
  output$stats_selector_ui <- renderUI({
    selectInput('statistic_to_compute', 
                'Statistic to Compute', 
                c('None', 'Min', 'Mean', 'Max', 'SD'))
  })
  
  filtered_and_selected_data <- reactive({
    req(the_data(), input$selected_column, input$filter_condition, input$comparison_value)
    data <- the_data()
    
    if(input$filter_condition == 'less') {
      data <- data %>% filter(get(input$selected_column) < input$comparison_value)
    } else if(input$filter_condition == 'equal') {
      data <- data %>% filter(get(input$selected_column) == input$comparison_value)
    } else if(input$filter_condition == 'greater') {
      data <- data %>% filter(get(input$selected_column) > input$comparison_value)
    }
    
    if (!is.null(input$fields_to_display) && !identical(input$fields_to_display, "")) {
      data <- data %>% select(all_of(input$fields_to_display))
    }
    
    data
  })
  
  output$filtered_data_table <- renderTable({
    req(filtered_and_selected_data())
    filtered_and_selected_data()
  })
  
  output$grouped_summary_output <- renderPrint({
    req(filtered_and_selected_data(), input$statistic_to_compute, input$fields_to_group_by)
    data <- filtered_and_selected_data()
    
    if (!is.null(input$fields_to_group_by) && input$fields_to_group_by != '') {
      data <- data %>% group_by(across(all_of(input$fields_to_group_by)))
    }
    
    statistic <- input$statistic_to_compute
    if (statistic != 'None') {
      summarized <- switch(statistic,
                           'Min' = data %>% summarise(across(all_of(input$selected_column), min, na.rm = TRUE)),
                           'Mean' = data %>% summarise(across(all_of(input$selected_column), mean, na.rm = TRUE)),
                           'Max' = data %>% summarise(across(all_of(input$selected_column), max, na.rm = TRUE)),
                           'SD' = data %>% summarise(across(all_of(input$selected_column), sd, na.rm = TRUE)),
                           data)
      return(summarized)
    }
    
    data
  })
}

shinyApp(ui, server)
