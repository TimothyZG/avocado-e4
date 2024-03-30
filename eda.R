# Import Dataset
df = read.csv("avocado.csv", header = TRUE)

# Dropped unnecessary column
drops <- c("X","X4046","X4225","X4770","Total.Bags",
           "Small.Bags","Large.Bags", "XLarge.Bags")
df = df[ , !(names(df) %in% drops)]
na_count = sum(is.na(df))
if(na_count==0){
  print("The dataset contains no na value.")
} else{
  sprintf("The dataset contains %d na values", na_count)
}
df$type = factor(df$type)
df$region = factor(df$region)
df$year = factor(df$year)

# Feature transformation (date -> month) 
install.packages("lubridate")
library(lubridate)
df$Date <- ymd(df$Date)
# Extract the month number
df$Month <- factor(month(df$Date))

#Data Wrangling and Transformation done, here's a preview of cleaned data:
head(df)

# EDA
# import ggplot library
install.packages("tidyverse")
library(ggplot2)
ggplot(df, aes(AveragePrice, Total.Volume, colour = type)) + 
  geom_point()+
  ggtitle("Volume against price and type")+
  xlab("Average Price")+
  ylab("Total Volume Sold")
