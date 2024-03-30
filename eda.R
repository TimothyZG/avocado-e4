# Import Dataset
df = read.csv("avocado.csv", header = TRUE)
drops <- c("X") # Dropped unnecessary column
df = df[ , !(names(df) %in% drops)]
na_count = sum(is.na(df))
if(na_count==0){
  print("The dataset contains no na value.")
} else{
  sprintf("The dataset contains %d na values", na_count)
}
head(df)

# EDA
# import ggplot library
install.packages("tidyverse")
library(ggplot2)
