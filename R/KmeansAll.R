
#' kmeans_all
#'
#' Apply the kmeans clustering algorithm to any data set and visualize commonalities between different categorical variables within each cluster. This function will automatically select the best number of clusters to apply to the data set by measuring the difference between each additional clsuter added to the analysis.
#' @param df The input data frame
#' @print First, the total number of clusters found and the number of rows per cluster will be displayed in the console. Then, the proportion for each categorical variable within each cluster will be displayed both graphically and visually in the console.  



kmeans_all <- function(df){
  
  library(tidyverse)
  
  if(anyNA(df)){
    print("Warning message: This data frame contains missing values. Stopping processing.")
  }else{
    print("Running kmeans processing!")
  
    df <- na.omit(df)
    
    df1 <- df %>%
      mutate_if(is_character, function(x) as.factor(x)) %>%
      mutate_if(is_integer, function(x) as.numeric(x)) %>%
      mutate_if(is.factor, function(x) as.numeric(x)) %>%
      select_if(is.numeric)
    
    df1 <- na.omit(df1)
    
    dfNorm <- scale(df1[,1:length(df1)])
    dfNorm <- as.data.frame(dfNorm)
    dfNorm <- na.omit(dfNorm)
    wss <- (nrow(dfNorm)-1)*sum(apply(dfNorm,2,var))
    for(i in 2:8){
      wss[i] <- sum(kmeans(dfNorm, centers=i)$withinss)
    }
    wssdf <- data.frame(x=wss)
    wssdf <- wssdf %>%
      mutate(jump = ifelse(x > lag(x), 1, 0)) %>%
      mutate(measure = ifelse(jump == 1, 1, NA)) %>%
      mutate(measure = ifelse(is.na(jump), 0, measure)) %>% 
      fill(measure) %>%
      filter(measure == 0) %>%
      mutate(name = row.names(.)) %>%
      mutate(y=((x-lag(x))/lag(x))*100) %>%
      mutate(z=y-lag(y)) %>%
      mutate(z = ifelse(is.na(z), 0, z)) %>%
      filter(z == min(z))
    numberclus <- as.numeric(wssdf$name)
    mydataCluster <-kmeans(dfNorm, numberclus, nstart = 25)
    df$cluster <- mydataCluster$cluster
    
    
    clustersfound <- unique(df$cluster)
    clustersfound <- sort(clustersfound)
    
    print(paste0(max(clustersfound), " clusters found for your data set!"))
    
    print(df %>% 
            group_by(cluster) %>%
            summarise(number.in.cluster = n()))
    
    cat ("Press [enter] to continue")
    line <- readline()
    
    for(i in 1:length(clustersfound)){
      print(paste0("Printing cluster ", i, " results:"))
      df %>%
        filter(cluster == i) %>%
        categorical_summary()
      print(paste0("This is the end of cluster ", i, " results. There are ", max(clustersfound), " clusters total."))
      cat ("Press [enter] to continue")
      line <- readline()
      dev.off(dev.list()["RStudioGD"])
    }
  }
}
  