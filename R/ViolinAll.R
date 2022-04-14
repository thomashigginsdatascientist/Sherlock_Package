

#' violin_all
#'
#' Create violin plots for each categorical variable in a data frame against a given numerical variable
#' @param df The input data frame
#' @param numericcolumn The numeric variable to be compared against each categorical variable
#' @param colstoremove Any columns specified to be removed from the original data frame before running analysis
#' @print A ggplot violin plot visualization for each categorical variable against the specified numeric variable in the data frame


violin_all <- function(df, numericcolumn, colstoremove = NULL){
  
  library(dplyr)
  library(ggplot2)
  library(scales)
  
  data <- df %>%
    select_if(is.character)
  
  data <- data[ , !colnames(data) %in% colstoremove]
  string <- colnames(data)
  
  numbers <- df[,numericcolumn]
  colnames(numbers) <- c("Val")
  
  for (i in string) {
    
    df <- data[,i]
    df$Val <- numbers$Val
    colnames(df) <- c("Var1", "Value")
    
    print(ggplot(data = df, aes(x=Var1, y=Value, fill = Var1)) +
            geom_violin() +
            #geom_boxplot(width=0.1, color="grey", alpha=0.2) +
            #geom_jitter(color="black", size=0.4, alpha=0.9) +
            labs(
              x = i,
              y = numericcolumn,
            ) +
            theme(
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_line(colour="grey", size = (0.1)),
              panel.grid.minor.y = element_line(colour="grey", size = (0.1)),
              axis.text.x= element_text(size = 15),
              axis.text.y= element_text(size = 15)
            ) +
            theme_minimal() +
            guides(fill=guide_legend(i)) +
            scale_y_continuous(labels = scales::comma_format(big.mark = ',', decimal.mark = '.'))
    )
  }
}

