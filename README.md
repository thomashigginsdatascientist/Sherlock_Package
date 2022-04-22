# Sherlock_Package
A simple R package designed to make EDA and data investigation easier!

Sherlock was written by a graduate student who needed an easier way to do ad hoc EDA for larger data sets for his school assignments. The functions in this package are intended to make EDA fast, visual and easier to disect patterns to serve as a launch pad for further analysis.

### Current functions include:
categorical_summary : visualize and list out the proportion of each categorical for each categorical variable in a data set
na_finder : find the number of missing values for each column in a data set
violin_all : create violin plots for each categorical variable against a specified numeric variable in a data set
kmeans_finder : apply the kmeans clustering algorithm to a data set and visualize the proprotion of each categorical variable in the data set for each cluster

### Use these R commands to access the package:
#install.packages("devtools")
library(devtools)
install_github("thomashigginsdatascientist/Sherlock_Package")
library(Sherlock)