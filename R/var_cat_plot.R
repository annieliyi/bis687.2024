library(randomcoloR)

cat_plot <- function(out_df, exp_df, label_vec){
  ret2 <- purrr::map(colnames(out_df), function(out_i){
    ret1 <- purrr::map(colnames(exp_df), function(disease_i){
      plot_df <- data.frame(
        out_df[, out_i], 
        exp_df[, disease_i]
        )
      
      colnames(plot_df) <- c(out_i, disease_i)
      
      plot_df <- plot_df |>
        group_by(!!sym(out_i), !!sym(disease_i)) |>
        summarize(n = n(), .groups = "drop")
        
      colors <- randomcoloR::distinctColorPalette(
        length(
        unique(
          plot_df[, out_i, drop = T])
        )
        )
      
      plt <- ggplot(plot_df) + 
        geom_col(aes(x =!!sym(disease_i), y = n, 
                     fill =  !!sym(out_i)), 
                 position = "dodge") +
        geom_text(aes(x = !!sym(disease_i), 
                      y = n, col =  !!sym(out_i),
                      label = n), vjust = 0, 
                  position = position_dodge(width = 1)) +
        scale_fill_manual(values = colors) + 
        scale_color_manual(values = colors) + 
        scale_x_discrete(drop = F) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        ylab(label_vec[tolower(out_i)]) + 
        xlab(label_vec[tolower(disease_i)])+
        labs(fill = label_vec[tolower(out_i)])
      
      return(plt)
    })
  })
  
  ret2
}