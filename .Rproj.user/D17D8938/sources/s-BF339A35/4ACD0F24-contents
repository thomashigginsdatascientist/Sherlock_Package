

#' NA Finder Summary
#'
#' Find the percentage of values that are NA/missing for each column in a dataframe
#' @param df The input dataframe
#' @param colstoremove Any columns specified to be removed from the original dataframe before running analysis
#' @print A single dataframe containing the percentage of NA values for each column in df and a ggplot visualization of this resulting dataframe
#' @export

nafinder <- function(df, colstoremove=NULL){
  
  start <- Sys.time()
  
  library(tidyverse)
  library(ggplot2)
  library(scales)
  
  df <- df[ , !colnames(df) %in% colstoremove]
  string <- colnames(df)
  
  rows <- nrow(df)
  
  empty <- data.frame("Column.Name", "Perc.NA")
  colnames(empty) <- empty[1,]
  empty <- empty[-1,]
  
  for (i in string) {
    empty[nrow(empty)+1,"Column.Name"] <- i
    
    nadf <- df[,i]
    nadf <- as.data.frame(is.na(nadf))
    
    if(is.na(table(nadf)["TRUE"])){
      
      empty[nrow(empty), "Perc.NA"] <- 0
      
    }else{
      
      empty[nrow(empty), "Perc.NA"] <- as.numeric(table(nadf)["TRUE"])/rows
      
    }
    
  }
  
  empty$Perc.NA <- as.numeric(empty$Perc.NA)
  empty$Perc.NA <- round(empty$Perc.NA, 4)
  print(empty)
  print(ggplot(data = empty, aes(x=Column.Name, y=Perc.NA, fill = Column.Name)) +
          geom_col() +
          geom_text(data=empty, aes(x=Column.Name, y=Perc.NA, label = percent(Perc.NA)), hjust = -0.25, size = 5) +
          coord_flip()+
          labs(
            x = "Column Name",
            y = "Percentage NA's",
          ) +
          theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(colour="grey", size = (0.1)),
            panel.grid.minor.x = element_line(colour="grey", size = (0.1)),
            axis.text.x= element_text(size = 15),
            axis.text.y= element_text(size = 15)
          ) +
          guides(fill=guide_legend(empty$Column.Name)) +
          scale_y_continuous(labels = scales::percent_format(big.mark = ',', decimal.mark = '.'))
  )
  end <- Sys.time()
  time <- end-start
  time <- round(time, 3)
  print(paste0("This function took ", time, " to run."))
}