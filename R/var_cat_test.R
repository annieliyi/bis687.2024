cat_test <- function(out_df, exp_df){
  ret2 <- purrr::map(colnames(out_df), function(out_i){
    ret1 <- purrr::map(colnames(exp_df), function(disease_i){
      tbl <- table(out_df[, out_i], 
                   exp_df[, disease_i])
      
      pre_chisq_check <- all(apply(tbl, 2, function(x) x > 5))
      
      if(pre_chisq_check){
        test <- chisq.test(tbl, correct = F)
        return(list(tbl, 
                    c(out_i, disease_i, "Chisq", round(test$p.val, 3))))
      } else{
        test <- fisher.test(tbl, workspace = 2e7, simulate.p.value=TRUE)
        return(
          list(tbl, 
               c(out_i, disease_i, "Fisher" , round(test$p.val, 3))))
        # return(sprintf("Low freq category detected for outcome %s. and disease %s.", 
        #                out_i, disease_i))
      }
    })
    tbl <- purrr::map(ret1, function(i){
      i[[1]]
    })
    
    stat <- purrr::map(ret1, function(i){
      i[[2]]
    })
    
    list(tbl, 
         do.call(rbind, stat))
  })
  stat_info <- purrr::map(ret2, function(i){
    i[[2]]
  })
  
  tbls <- purrr::map(ret2, function(i){
    i[[1]]
  })
  
  ret <- do.call(rbind, stat_info) |> as.data.frame()
  colnames(ret) <- c("Outcome", "Explanatory", "Method", "Pvalue")
  list(tbls, ret)
}