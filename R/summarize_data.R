library(purrr)
library(dplyr)
library(tidyr)
library(stringr)

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
    
    gt::gtsave(ret1,  file.path("tables", 
                                paste0(tbl_name, "_Cont_Sum.png")))
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
    
    gt::gtsave(ret2, file.path("tables", 
                               paste0(tbl_name,
                                      "_Cat_Sum.png")),
               expand = 40)
    
  }
  return(list(ret1, ret2))
}