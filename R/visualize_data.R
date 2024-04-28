library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)


visualize_data <- function(df, id_cols, factor_cols, 
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
  
  
  
  
  
  
  cont_plts <- lapply(cont_vars, function(var_i){
    df_i <- df[ , var_i, drop = F]
    
    ggplot(df_i) + 
      geom_density(aes(x = !!sym(var_i))) + 
      xlab(label_vec[tolower(var_i)])
  })
  
  
  
  cat_plts <- lapply(cat_vars, function(var_i){
    df_i <- df[ , var_i, drop = F]
    
    df_i <- df_i |>
      group_by(!!sym(var_i)) |>
      summarize(n = n())
    
    ggplot(df_i) + 
      geom_col(aes(x = !!sym(var_i), y = n))+
      geom_text(aes(x = !!sym(var_i), y = n, 
                    label = n),
                position = position_dodge(0.5), 
                vjust = 0) +
      xlab(label_vec[tolower(var_i)]) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  list(cont_plts, cat_plts)
}