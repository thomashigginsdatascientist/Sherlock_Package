% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KmeansAll.R
\name{kmeans_finder}
\alias{kmeans_finder}
\title{kmeans_finder}
\usage{
kmeans_finder(df)
}
\arguments{
\item{df}{The input data frame}
}
\value{
First, the total number of clusters found and the number of rows per cluster will be displayed in the console. Then, the proportion for each categorical variable within each cluster will be displayed both graphically and visually in the console.
}
\description{
Apply the kmeans clustering algorithm to any data set and visualize commonalities between different categorical variables within each cluster. This function will automatically select the best number of clusters to apply to the data set by measuring the difference between each additional clsuter added to the analysis.
}
\examples{
library(Sherlock)
data("flights")
kmeans_finder(df = flights)
}
