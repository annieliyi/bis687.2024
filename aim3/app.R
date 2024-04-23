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
library(DT)
library(gt)

# Define the summarize_data function directly within the app file
summarize_data <- function(df, id_cols, factor_cols, 
                           label_vec, tbl_name){
  num_vars <- purrr::map_lgl(df %>% select(-all_of(id_cols),
                                           -all_of(factor_cols)),
                             is.numeric)
  cont_vars <- names(num_vars)[num_vars]
  cat_vars <- colnames(
    df %>%
      select(-all_of(id_cols)))[which(
        !colnames(df %>%
                    select(-all_of(id_cols))) %in% cont_vars)]
  
  if(length(cont_vars) == 0){
    ret1 <- "No Continuous variables in dataset"
  } else{
    cont_sum <- df %>%
      select(all_of(cont_vars))%>%
      mutate(across(all_of(cont_vars), 
                    list(`mean (sd)` = ~ paste0(round(mean(.x, na.rm = T),2), 
                                                " (", 
                                                round(sd(.x, na.rm = T), 2),  
                                                ")")
                    ),
                    .names = "{.col}")) %>%
      distinct() %>%
      t(.) %>%
      data.frame(.)
    
    colnames(cont_sum) <- "value"
    
    ret1 <- cont_sum %>% 
      mutate(var = rownames(.)) %>%
      mutate(var_lbl = label_vec[tolower(var)]) %>%
      select(-var) %>%
      gt::gt(rowname_col = "var_lbl", 
             caption = paste0(tbl_name, " : Continuous Variable Summary"))
  }
  
  if(length(cat_vars)== 0){
    ret2 <- "No Continuous variables in dataset"
  } else{
    cat_sum <- df %>%
      select(all_of(cat_vars)) %>%
      mutate(across(all_of(cat_vars), as.factor))
    
    cat_sum_wide <- cat_sum %>%
      mutate(id = row_number())%>%
      select(id)
    
    purrr::walk(seq_len(length(cat_vars)), function(i){
      covar_i <- cat_vars[i]
      temp_df_cat <- cat_sum[ , covar_i, drop = FALSE]
      temp_df_wide <- temp_df_cat %>%
        dplyr::mutate(value = 1, id = row_number()) %>%
        pivot_wider(id_cols = c(id),
                    values_from = value, 
                    names_from = !!sym(covar_i), 
                    names_prefix = paste0(covar_i, ":"),
                    values_fill = 0)
      
      cat_sum_wide <<- cat_sum_wide %>%
        left_join(temp_df_wide, by = c("id" = "id")) 
    })
    
    #cat_sum_wide
    cat_sum <- cat_sum_wide %>%
      select(-id) %>%
      mutate(across(everything(), 
                    list(`n (%)` = ~paste0(sum(.x) , 
                                           " (", 
                                           round(mean(.x) * 100, 2),
                                           "%)")
                    ), 
                    .names = "{.col}")) %>%
      distinct() %>%
      t(.) %>%
      data.frame(.)
    
    colnames(cat_sum) <- "value"
    
    ret2 <- cat_sum %>%
      mutate(cat = rownames(.), 
             group_cat = str_split(cat, ":", simplify = T)[, 1], 
             cat_label = str_split(cat, ":", simplify = T)[, 2]) %>%
      mutate(group_lbl = label_vec[tolower(group_cat)]) %>%
      select(-cat, -group_cat) %>%
      gt::gt(rowname_col = "cat_label", 
             groupname_col = "group_lbl", 
             caption = paste0(tbl_name, " : Categorical Variable Summary"))
    
  }
  return(list(ret1, ret2))
}






ui <- fluidPage(
  titlePanel("Variable Plotter and Data Summaries"),
  sidebarLayout(
    sidebarPanel(
      selectInput("datasetSelect", "Choose a Dataset:", choices = NULL),
      selectInput("varSelect", "Choose a Variable:", choices = NULL)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("varPlot")),
        tabPanel("Continuous Summary", uiOutput("summaryContTable")),
        tabPanel("Categorical Summary", uiOutput("summaryCatTable"))
      )
    )
  )
)




server <- function(input, output, session) {
  # Load the list of datasets
  df_lst <- readRDS("../cleaned_categorized_dfsubset.RDS")
  naming_vec <- readRDS("../var_labels.RDS")
  dataset_names <- names(df_lst)
  
  # Update dataset selection choices
  observe({
    updateSelectInput(session, "datasetSelect", choices = dataset_names, selected = dataset_names[1])
  })
  
  # Reactively update the variable selection based on the dataset
  observeEvent(input$datasetSelect, {
    df_selected <- df_lst[[input$datasetSelect]]
    updateSelectInput(session, "varSelect", choices = names(df_selected), selected = names(df_selected)[1])
  })
  
  # Generate and render plots based on selected variable
  output$varPlot <- renderPlot({
    req(input$datasetSelect, input$varSelect)  # Ensure both inputs are selected
    df_selected <- df_lst[[input$datasetSelect]]
    var_selected <- input$varSelect
    
    # Check variable type and plot accordingly
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
  
  # Directly render tables without using reactive
  output$summaryContTable <- renderTable({
    req(input$datasetSelect)  # Ensure a dataset is selected
    df_selected <- df_lst[[input$datasetSelect]]
    
    exclude = c("RACEGP", "YEARTX") %in% colnames(df_selected)
    id = "DUMMYID" %in% colnames(df_selected)
    
    exclude <- colnames(df_selected)[exclude]
    id <- colnames(df_selected)[id]
    
    # Direct call to the summarize function
    summaries <- summarize_data(df_selected, id, exclude, naming_vec, names(df_lst)[input$datasetSelect])
    
    if(is.character(summaries[[1]])) {  # Assuming ret1 is Continuous
      data.frame(Message = "No continuous variables to display.")
    } else {
      summaries[[1]]  # Display the Continuous data
    }
  })
  
  output$summaryCatTable <- renderTable({
    req(input$datasetSelect)  # Ensure a dataset is selected
    df_selected <- df_lst[[input$datasetSelect]]
    
    exclude = c("RACEGP", "YEARTX") %in% colnames(df_selected)
    id = "DUMMYID" %in% colnames(df_selected)
    
    exclude <- colnames(df_selected)[exclude]
    id <- colnames(df_selected)[id]
    
    # Direct call to the summarize function
    summaries <- summarize_data(df_selected, id, exclude, naming_vec, names(df_lst)[input$datasetSelect])
    
    if(is.character(summaries[[2]])) {  # Assuming ret2 is Categorical
      data.frame(Message = "No categorical variables to display.")
    } else {
      summaries[[2]]  # Display the Categorical data
    }
  })
}




# Run the application 
shinyApp(ui = ui, server = server)













