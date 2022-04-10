

#' Categorical Proportional Summary
#'
#' Visualize and understand the proportion of each category within variables defined as character for a given dataset
#' @param df The input dataframe
#' @param colstoremove Any columns specified to be removed from the original dataframe before running analysis
#' @print The resulting proportions as printed dataframes and ggplot visualizations of the resulting proportional dataframes
#' @export

categoricalsummary <- function(df, colstoremove = NULL){
  
  library(tidyverse)
  library(ggplot2)
  library(scales)
  
  data <- df %>%
    select_if(is.character)
  
  data <- data[ , !colnames(data) %in% colstoremove]
  string <- colnames(data)
  
  for (i in string) {
    
    df <- table(data[,i])
    df <- round(prop.table(df), 3)
    df <- as.data.frame(df)
    
    print(ggplot(data = df, aes(x=Var1, y=Freq, fill = Var1)) +
            geom_col() +
            geom_text(data=df, aes(x=Var1, y=Freq, label = percent(Freq)), vjust = 1.5, size = 5) +
            labs(
              x = i,
              y = "Percentage of Dataset",
            ) +
            theme(
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_line(colour="grey", size = (0.1)),
              panel.grid.minor.y = element_line(colour="grey", size = (0.1)),
              axis.text.x= element_text(size = 15),
              axis.text.y= element_text(size = 15)
            ) +
            guides(fill=guide_legend(i)) +
            scale_y_continuous(labels = scales::percent_format(big.mark = ',', decimal.mark = '.'))
    )
    
    df$Freq <- percent(df$Freq)
    print(df)
  }
}