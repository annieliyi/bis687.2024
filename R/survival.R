library(ggplot2)
library(dplyr)
library(haven)
library(tidyr)
library(readr)
library(stringr)
library(readxl)
library(gt)
library(kableExtra)
library(survival)
library(glmnet)
library(boot)
library(survminer)
library(knitr)
library(purrr)

# var_full <- readRDS("processed_meta.rds")
scd_data_yr3_label <- readRDS(file.path(getwd(),"processed_scd_data.rds"))
# naming_vec <- readRDS("var_labels.RDS")
#df_lst <- readRDS(file.path(getwd(),"../categorized_dfsubset.RDS"))

#time to events variables:
#INTXAGVHD, INTXCGVHD, INTXSCDMAL, INTXANC, INTXPLA_TELET, INTXGF, INTXSURV,death

# event && event occurance
# convert DEAD, GF, EFS, ANC, PLATELET, AGVHD, CGVHD, SCDMAL_FINAL to 0/1
#time to events variables:
#INTXAGVHD, INTXCGVHD, INTXSCDMAL, INTXANC, INTXPLA_TELET, INTXGF, INTXSURV, INTXPYLD
scd_data_yr3_label$DEAD <- ifelse(scd_data_yr3_label$DEAD=='Alive', 0,1)
scd_data_yr3_label$GF <- ifelse(scd_data_yr3_label$GF=='No', 0,1)
scd_data_yr3_label$PTLD <- ifelse(scd_data_yr3_label$PTLD=='No event', 0,1)
scd_data_yr3_label$ANC <- ifelse(scd_data_yr3_label$ANC=='Yes', 1,0)
scd_data_yr3_label$PLATELET <- ifelse(scd_data_yr3_label$PLATELET=='Yes', 1,0)
scd_data_yr3_label$AGVHD <- ifelse(scd_data_yr3_label$AGVHD=='No', 0,1)
scd_data_yr3_label$CGVHD <- ifelse(scd_data_yr3_label$CGVHD=='No', 0,1)
scd_data_yr3_label$SCDMAL_FINAL <- ifelse((scd_data_yr3_label$SCDMAL_FINAL=='None' & scd_data_yr3_label$SCDMAL_FINAL=='Not Reported'), 0,1)

#total features: RCMVPR, SEX, ETHNICIT, DONORF, GRAFTYPE, YEARTX, AGE, AGEGPFF, KPS, HCTCIGPF, SUBDIS1F, ATGF, YEARGPF, GVHD_FINAL, CONDGRPF, CONDGRP_FINAL, HLA_FINAL, FLAG_LANCET, FLAG_BLOOD

# 8 tables for a endpoint including selected features as row and coefficients as column
scd_data_yr3_label <- na.omit(scd_data_yr3_label)

cox_func <- function(event,scd_data_yr3_label){
  scd_data_yr3_label <- na.omit(scd_data_yr3_label)
  time_to_event <- paste0('INTX', event)
  time_to_event <- ifelse(time_to_event =='INTXDEAD', 'INTXSURV',time_to_event)
  time_to_event <- ifelse(time_to_event =='INTXEFS', 'INTXPTLD',time_to_event)
  time_to_event <- ifelse(time_to_event =='INTXSCDMAL_FINAL', 'INTXSCDMAL',time_to_event)
  x <- model.matrix(~., data = scd_data_yr3_label[, 2:20])  # excluding time and event columns
  y <- Surv(scd_data_yr3_label[[time_to_event]], scd_data_yr3_label[[event]])
  
  # Fit lasso model
  #fit_lasso <- glmnet(x, y, family = "cox")
  #plot(fit_lasso)
  cv_fit <- cv.glmnet(x, y, family = "cox")
  # plot(cv_fit)
  best_lambda <- cv_fit$lambda.min
  fit_best_lambda <- glmnet(x, y, family = "cox", alpha = 1, lambda = best_lambda)
  
  # Look at the coefficients
  coef_best_lambda <- coef(fit_best_lambda)
  coef_best_lambda <- data.frame(feature = rownames(coef_best_lambda), value = as.numeric(coef_best_lambda[, 1]))
  names(coef_best_lambda) <- c('feature', "value")
  coef_best_lambda <- coef_best_lambda[coef_best_lambda$value != 0, ]
  rownames(coef_best_lambda) <- NULL
  return(coef_best_lambda)
}



plot_km <- function(event,i, scd_data_yr3_label){
  coefficients_df <- cox_func(event,scd_data_yr3_label)
  selected_features <- coefficients_df$feature
  scd_data_yr3_label <- na.omit(scd_data_yr3_label)
  time_to_event <- paste0('INTX', event)
  time_to_event <- ifelse(time_to_event =='INTXDEAD', 'INTXSURV',time_to_event)
  time_to_event <- ifelse(time_to_event =='INTXEFS', 'INTXPTLD',time_to_event)
  time_to_event <- ifelse(time_to_event =='INTXSCDMAL_FINAL', 'INTXSCDMAL',time_to_event)
  extract_column_name_and_value <- function(feature) {
    # RCMVPR, SEX, ETHNICIT, DONORF, GRAFTYPE, YEARTX, AGE, AGEGPFF, KPS, HCTCIGPF, SUBDIS1F, ATGF, YEARGPF, GVHD_FINAL, CONDGRPF, CONDGRP_FINAL, HLA_FINAL, FLAG_LANCET, FLAG_BLOOD
    if (any(grepl("RCMVPR|SEX|ETHNICIT|DONORF|GRAFTYPE|AGEGPFF|KPS|HCTCIGPF|ATGF|SUBDIS1F|YEARGPF|GVHD_FINAL|CONDGRPF|CONDGRP_FINAL|HLA_FINAL||FLAG_LANCET|FLAG_BLOOD", feature, ignore.case = TRUE))) {
      parts <- strsplit(feature, "(?<=[A-Z])(?=[A-Z][a-z])|(?<=[a-z])(?=[A-Z])", perl=TRUE)[[1]]
      column_name <- parts[1]
      value <- paste(parts[-1], collapse = "")
      return(list(column_name = column_name, value = value))
    } else {
      return(list(column_name = NA, value = NA))
    }
  }

  km_features <- c()
  for(feature in selected_features) {
    result <- extract_column_name_and_value(feature)
    column_name <- result$column_name
    value <- result$value
    if(!is.na(column_name)) {
      if(column_name %in% names(scd_data_yr3_label)) {
        if(value %in% unique(scd_data_yr3_label[[column_name]])) {
          #print(paste("Match found for", column_name, "with value", value))
          km_features = c(km_features,column_name )
        } else {
          #print(paste("Column", column_name, "found, but value", value, "not found"))
          km_features = c(km_features,column_name )
        }
      } else {
        #print(paste("Column", column_name, "not found"))
      }
    }
  }
  p<- NA
  if(i %in% km_features){
    if(i %in% c('YEARTX', 'AGE')){
      p<- paste(i, 'is not catagorical')
    }
    else{
      scd_data_yr3_label <- scd_data_yr3_label %>%
        mutate(across(c('RCMVPR', 'SEX', 'ETHNICIT', 'DONORF', 'GRAFTYPE', 'AGEGPFF', 'KPS', 'HCTCIGPF', 'SUBDIS1F', 'ATGF', 'YEARGPF', 'GVHD_FINAL', 'CONDGRPF', 'CONDGRP_FINAL', 'HLA_FINAL', 'FLAG_LANCET', 'FLAG_BLOOD'), as.factor))
      value_counts <- sapply(scd_data_yr3_label[, sapply(scd_data_yr3_label, is.factor)], table)
      rows_to_keep <- apply(scd_data_yr3_label, 1, function(row) {
        all(sapply(names(row), function(var) {
          if (is.factor(scd_data_yr3_label[[var]])) {
            return(value_counts[[var]][as.character(row[var])] > 5)
          } else {
            return(TRUE)
          }
        }))
      })
      km_featuresdata <- scd_data_yr3_label[rows_to_keep, ]
    p <-ggsurvplot(
        survminer::surv_fit(as.formula(paste("Surv(", time_to_event,',',event,") ~", i, collapse = " ")), 
                data = km_featuresdata), 
        pval = FALSE,
        #onf.int = 0.95,
        risk.table = TRUE, 
        risk.table.height = 0.25,
        ggtheme = theme_survminer(
          base_size = 10
        ),
        tables.theme = theme_cleantable(),
        legend.title = "")
      #t <- ggsurvtable(survfit(as.formula(paste("Surv(", time_to_event,',',event,") ~", i)), 
      #        data = scd_data_yr3_label))
    }
    
  }
  else{
    p <- paste(i, 'not been selected', 'for', event)
  }
  return(p)
}



# summary table showing all selected endpoints and there selected features 
summary_table <- function(events,scd_data_yr3_label){
  lstfeatures <- c()
  for(event in events){
    coefficients_df <- cox_func(event,scd_data_yr3_label)
    selected_features <- coefficients_df$feature
    scd_data_yr3_label <- na.omit(scd_data_yr3_label)
    time_to_event <- paste0('INTX', event)
    time_to_event <- ifelse(time_to_event =='INTXDEAD', 'INTXSURV',time_to_event)
    time_to_event <- ifelse(time_to_event =='INTXEFS', 'INTXPTLD',time_to_event)
    time_to_event <- ifelse(time_to_event =='INTXSCDMAL_FINAL', 'INTXSCDMAL',time_to_event)
    extract_column_name_and_value <- function(feature){
      # RCMVPR, SEX, ETHNICIT, DONORF, GRAFTYPE, YEARTX, AGE, AGEGPFF, KPS, HCTCIGPF, SUBDIS1F, ATGF, YEARGPF, GVHD_FINAL, CONDGRPF, CONDGRP_FINAL, HLA_FINAL, FLAG_LANCET, FLAG_BLOOD
      if (any(grepl("RCMVPR|SEX|ETHNICIT|DONORF|GRAFTYPE|AGEGPFF|KPS|HCTCIGPF|ATGF|SUBDIS1F|YEARGPF|GVHD_FINAL|CONDGRPF|CONDGRP_FINAL|HLA_FINAL||FLAG_LANCET|FLAG_BLOOD", feature, ignore.case = TRUE))) {
        parts <- strsplit(feature, "(?<=[A-Z])(?=[A-Z][a-z])|(?<=[a-z])(?=[A-Z])", perl=TRUE)[[1]]
        column_name <- parts[1]
        value <- paste(parts[-1], collapse = "")
        return(list(column_name = column_name, value = value))
      } else {
        return(list(column_name = NA, value = NA))
      }
    }
    km_features <- c()
    
    for(feature in selected_features) {
      result <- extract_column_name_and_value(feature)
      column_name <- result$column_name
      value <- result$value
      if(!is.na(column_name)) {
        if(column_name %in% names(scd_data_yr3_label)) {
          if(value %in% unique(scd_data_yr3_label[[column_name]])) {
            #print(paste("Match found for", column_name, "with value", value))
            km_features = c(km_features,column_name )
          } else {
            #print(paste("Column", column_name, "found, but value", value, "not found"))
            km_features = c(km_features,column_name )
          }
        } else {
          #print(paste("Column", column_name, "not found"))
        }
      }
    }
    
    features <- paste(unlist(km_features), collapse = ", ")
    lstfeatures = c(lstfeatures, features) 
  }

  summarytbl<- data.frame( Event = events, Features = lstfeatures)
  return(summarytbl)
}

summary_table_wide <- function(events, scd_data_yr3_label){
  original_data <-summary_table(events, scd_data_yr3_label)
  unique_features <- c('RCMVPR', 'SEX', 'ETHNICIT', 'DONORF', 'GRAFTYPE', 'YEARTX', 'AGE', 'AGEGPFF', 'KPS', 'HCTCIGPF', 'SUBDIS1F', 'ATGF', 'YEARGPF', 'GVHD_FINAL', 'CONDGRPF', 'CONDGRP_FINAL', 'HLA_FINAL', 'FLAG_LANCET', 'FLAG_BLOOD')
  expanded_df <- data.frame(matrix(ncol = length(unique_features), nrow = nrow(original_data)))
  names(expanded_df) <- unique_features
  expanded_df$event <- original_data$Event
  for (i in 1:nrow(original_data)) {
    features_in_event <- trimws(strsplit(original_data$Features, split = ",")[[i]])
    for (feature in unique_features) {
      expanded_df[i, feature] <- ifelse(feature %in% features_in_event, "Y", " ")
    }
  }
  
  rownames(expanded_df) <- expanded_df$event
  expanded_df$event <- NULL
  return(expanded_df)
}


