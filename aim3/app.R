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

library(haven)
library(tidyr)
library(readr)
library(readxl)
library(gt)
library(kableExtra)
library(survival)
library(glmnet)
library(boot)
library(survminer)
library(knitr)
library(htmltools)




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

variable_descriptions <- data.frame(
  Category = c(
    "Disease-related", "Patient-related", "Patient-related", 
    "Patient-related", "Patient-related", "Patient-related", 
    "Patient-related", "Patient-related", "Patient-related", 
    "Patient-related", "Transplant-related", "Transplant-related", 
    "Transplant-related", "Transplant-related", "Transplant-related", 
    "Transplant-related", "Transplant-related", "Transplant-related", 
    "Transplant-related", "Transplant-related", "Transplant-related", 
    "Outcomes", "Outcomes", "Outcomes", "Outcomes", "Outcomes", 
    "Outcomes", "Outcomes", "Outcomes", "Outcomes", "Outcomes"
  ),
  Variable.name = c(
    "subdis1f", "Dummyid", "flag_lancet", 
    "flag_blood", "flag_0601", "age", 
    "agegpff", "sex", "ethnicit", 
    "kps", "hctcigpf", "donorf", 
    "graftype", "condgrp", "condgrp_final", 
    "atgf", "gvhd_final", "hla_final", 
    "rcmvpr", "yeargp", "yeartx", 
    "intxsurv", "intxgf", "intxanc", 
    "intxplatelet", "intxagvhd", "intxcgvhd", 
    "intxptld", "intxsxcdmal", "dead", 
    "efs"
  ),
  Description = c(
    "Disease genotype", "Unique patient identifier", "Cases from 2019 Lancet Haematology publication", 
    "Cases from 2016 Blood publication", "Cases from BMT CTN 0601", "Patient age at transplant, years", 
    "Patient age at transplant, years", "Sex", "Ethnicity", 
    "Karnofsky/Lansky score at HCT", "HCT-comorbidity index", "Donor type", 
    "Graft type", "Conditioning intensity", "Conditioning regimen", 
    "ATG/Alemtuzumab given as conditioning regimen/GVHD prophylaxis", "GVHD prophylaxis", "Donor-recipient HLA matching", 
    "Recipient CMV serostatus", "Year of transplant", "Year of transplant", 
    "Time from HCT to date of last contact or death, months", "Time from HCT to graft failure, months", "Time from HCT to neutrophil engraftment, months", 
    "Time from HCT to platelet recovery, months", "Time from HCT to acute graft-vs-host disease, months", "Time from HCT to chronic graft-vs-host disease, months", 
    "Time from HCT to PTLD, months", "Time from HCT to second malignancy, months", "Survival status at last contact", 
    "Event-free survival (Graft failure or death are the events)"
  )
)






events <- c('DEAD', 'GF', 'EFS', 'ANC', 'PLATELET', 'AGVHD', 'CGVHD', 'SCDMAL_FINAL')
features <- c('RCMVPR', 'SEX', 'ETHNICIT', 'DONORF', 'GRAFTYPE', 
              'YEARTX', 'AGE', 'AGEGPFF', 'KPS', 'HCTCIGPF', 'SUBDIS1F', 
              'ATGF', 'YEARGPF', 'GVHD_FINAL', 'CONDGRPF', 'CONDGRP_FINAL', 
              'HLA_FINAL', 'FLAG_LANCET', 'FLAG_BLOOD')










ui <- fluidPage(
  titlePanel("Data Visualization Platform on Post-HCT Outcomes for SCD Patients"),
  sidebarLayout(
    sidebarPanel(
      selectInput("datasetSelect", "Choose a Dataset:", choices = NULL),
      selectInput("varSelect", "Choose a Variable:", choices = NULL),
      selectInput("eventSelect", "Select Events:", choices = events, multiple = TRUE),
      selectInput("featureSelect", "Select Feature:", choices = features, multiple = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Variable Distribution Plots", 
                 helpText("Use the 1st and 2nd inputs."),
                 plotOutput("varPlot")),
        tabPanel("Variable Descriptions", 
                 helpText("No input required."),
                 DTOutput("varDescriptions")),
        tabPanel("Outcome Summary", 
                 helpText("Use the 1st input; utilize the search bar to filter results."),
                 uiOutput("summaryContTable")),
        tabPanel("Categorical Variables Summary", 
                 helpText("Use the 1st and 2nd inputs."),
                 uiOutput("summaryCatTable")),
        tabPanel("Event Coefficients", 
                 helpText("Use the 3rd input only."),
                 uiOutput("eventCoefficients")),
        tabPanel("Event Features Summary", 
                 helpText("Use the 3rd input only."),
                 uiOutput("eventFeaturesSummary")),
        tabPanel("Survival Plots", 
                 helpText("Select an event using the 3rd input, then select a feature with the 4th input."),
                 uiOutput("kmPlots"))
      )
    )
  )
)




server <- function(input, output, session) {
  # Load the list of datasets
  df_lst <- readRDS("../cleaned_categorized_dfsubset.RDS")
  naming_vec <- readRDS("../var_labels.RDS")
  dataset_names <- names(df_lst)
  source("../R/survival.R")
  
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
  
  
  output$varDescriptions <- renderDT({
    datatable(variable_descriptions, options = list(pageLength = 10))
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
  
  output$eventCoefficients <- renderUI({
    req(input$eventSelect)  # Ensure at least one event is selected
    
    # Create a table output for each selected event
    tables <- lapply(input$eventSelect, function(event) {
      # Generate the coefficient table for the event
      coeff_table <- cox_func(event, scd_data_yr3_label)
      
      # Create the kable table
      kable_table <- kable(coeff_table, caption = paste("Coefficients of features for", event)) %>%
        kable_classic(full_width = F, html_font = "Cambria") %>%
        as.character()  # Convert to character to ensure it's HTML
      
      # Wrap the kable table output in an HTML container with padding
      htmltools::tags$div(HTML(kable_table), style = "padding-bottom: 20px;")
    })
    
    # Return all the tables
    do.call(htmltools::tagList, tables)
  })
  
  # Render the expanded features summary table
  output$eventFeaturesSummary <- renderUI({
    req(input$eventSelect)  # Ensure at least one event is selected
    
    # Generate the expanded features summary table for selected events
    expanded_df <- summary_table_wide(input$eventSelect, scd_data_yr3_label)
    
    # Create the kable table for the expanded features summary
    kable_table <- kable(expanded_df, caption = "Selected features for different events") %>%
      kable_classic(full_width = F, html_font = "Cambria") %>%
      as.character()  # Convert to character to ensure it's HTML
    
    # Return the kable table
    HTML(kable_table)
  })
  
  
  
  
  output$kmPlots <- renderUI({
    req(input$eventSelect, input$featureSelect)  # Ensure event and feature are selected
    
    # Create a plot output for each combination of selected event and feature
    plot_outputs <- lapply(input$eventSelect, function(event) {
      lapply(input$featureSelect, function(feature) {
        # Generate the plot for the event-feature combination
        plot_output <- renderPlot({ plot_km(event, feature, scd_data_yr3_label) })

        
        # Wrap the plot output in an HTML container
        htmltools::tags$div(plot_output, style = "padding-bottom: 20px;")
      })
    })
    
    # Flatten the list of lists and combine all plot outputs into one UI output
    do.call(htmltools::tagList, unlist(plot_outputs, recursive = FALSE))
  })
  
  # Update the variable selection choices based on dataset
  observeEvent(input$datasetSelect, {
    df_selected <- df_lst[[input$datasetSelect]]
    updateSelectInput(session, "varSelect", choices = names(df_selected), selected = names(df_selected)[1])
    updateSelectInput(session, "featureSelect", choices = features, selected = features[1])
  })
  

  
  
  
}




# Run the application 
shinyApp(ui = ui, server = server)























