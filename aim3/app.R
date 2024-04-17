#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)


ui <- fluidPage(
  titlePanel("Variable Plotter for Multiple Datasets"),
  sidebarLayout(
    sidebarPanel(
      selectInput("datasetSelect", "Choose a Dataset:", choices = NULL),
      selectInput("varSelect", "Choose a Variable:", choices = NULL)
    ),
    mainPanel(
      plotOutput("varPlot")
    )
  )
)



server <- function(input, output, session) {
  # Load the list of datasets
  df_lst <- readRDS("../cleaned_categorized_dfsubset.RDS")
  dataset_names <- names(df_lst)
  
  # Update dataset selection choices
  updateSelectInput(session, "datasetSelect", choices = dataset_names)
  
  # Observe changes in selected dataset and update variable selection choices
  observeEvent(input$datasetSelect, {
    df_selected <- df_lst[[input$datasetSelect]]
    updateSelectInput(session, "varSelect", choices = names(df_selected))
  })
  
  # Generate and render plots based on selected variable
  output$varPlot <- renderPlot({
    req(input$datasetSelect, input$varSelect)  # Ensure both inputs are selected
    df_selected <- df_lst[[input$datasetSelect]]
    var_selected <- input$varSelect
    
    # You might adapt the logic here depending on the data structure and needs
    if (is.numeric(df_selected[[var_selected]])) {
      ggplot(df_selected, aes_string(x = var_selected)) +
        geom_density() +
        xlab(var_selected)
    } else {
      df_grouped <- df_selected %>%
        group_by(!!sym(var_selected)) %>%
        summarize(n = n(), .groups = 'drop')
      
      ggplot(df_grouped, aes_string(x = var_selected, y = 'n')) +
        geom_col() +
        geom_text(aes(label = n), vjust = -0.3) +
        xlab(var_selected) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}



# Run the application 
shinyApp(ui = ui, server = server)