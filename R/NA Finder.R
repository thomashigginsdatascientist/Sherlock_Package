

#' na_finder
#'
#' Find the percentage of values that are NA/missing for each column in a data frame
#' @param df The input data frame
#' @param colstoremove Any columns specified to be removed from the original data frame before running analysis
#' @param Nulls If TRUE, will also look for strings equal to "NULL" and treat those rows as missing values, along with rows containing NA's
#' @print A single data frame containing the percentage of NA values for each column in df and a ggplot visualization of this resulting data frame
#' @export

na_finder <- function(df, colstoremove=NULL, Nulls = FALSE){
  
  if(Nulls == FALSE){
  
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
          theme_minimal() +
          guides(fill=guide_legend(empty$Column.Name)) +
          scale_y_continuous(labels = scales::percent_format(big.mark = ',', decimal.mark = '.'))
  )
  end <- Sys.time()
  time <- end-start
  time <- round(time, 3)
  print(paste0("This function took ", time, " to run."))
  
  }else if(Nulls == TRUE){
    
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
      
      nulldf <- df[,i]
      nulldf <- as.data.frame(nulldf)
      
      if( (is.na(table(nadf)["TRUE"])) & (is.na(table(nulldf)["NULL"])) ){
        
        empty[nrow(empty), "Perc.NA"] <- 0
        
      }else if( (is.na(table(nadf)["TRUE"])) &  (!is.na(table(nulldf)["NULL"])) ){
      
        empty[nrow(empty), "Perc.NA"] <- (0+as.numeric(table(nulldf)["NULL"]))/rows
      
      }else if( (!is.na(table(nadf)["TRUE"])) &  (is.na(table(nulldf)["NULL"])) ){
        
        empty[nrow(empty), "Perc.NA"] <- (as.numeric(table(nadf)["TRUE"])+0)/rows
        
      }else{
        
        empty[nrow(empty), "Perc.NA"] <- (as.numeric(table(nadf)["TRUE"])+as.numeric(table(nulldf)["NULL"]))/rows
        
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
}