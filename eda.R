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

ggplot(df, aes(year, Total.Volume, colour = type)) +
  geom_point()+
  ggtitle("Volume against year and type")+
  xlab("Year sold")+
  ylab("Total Volume Sold")

# From these two plots, we see that there is a great difference between the
# volume of conventional and organic avocados sold, as such, I believe that it 
# would make sense to analyze both individually

ggplot(subset(df, type == "conventional"), aes(year, Total.Volume)) +
  geom_point() +
  ggtitle("Volume against year for Type_A") +
  xlab("Year sold") +
  ylab("Total Volume Sold")

ggplot(subset(df, type == "organic"), aes(year, Total.Volume)) +
  geom_point() +
  ggtitle("Volume against year for Type_A") +
  xlab("Year sold") +
  ylab("Total Volume Sold")

# In both the conventional and the organic avocado plots, it can be seen that
# there are two distinct groups, to see why there's this difference, we will
# group by the other variables to see if we can identify the reason

# Inspecting the data more closely, it can be seen that many of the top results 
# are for total US/ regions of the US, aka west, east, and central. In order to
# account for this, we can remove these points and analyze them separately

regions_to_exclude <- c("TotalUS", "West", "SouthCentral", "SouthEast", 
                        "Midsouth", "GreatLakes", "Southeast", "Northeast",
                        "Plains")

# Filter the dataframe to exclude the specified regions
df_filtered <- subset(df, !(region %in% regions_to_exclude))

# Once removing these results, the results become more consistent

ggplot(subset(df_filtered, type == "organic"), aes(year, Total.Volume, colour = season)) +
  geom_point() +
  ggtitle("Volume against year for filtered (organic)") +
  xlab("Year sold") +
  ylab("Total Volume Sold")

ggplot(subset(df_filtered, type == "conventional"), aes(year, Total.Volume)) +
  geom_point() +
  ggtitle("Volume against year for filtered (conventional") +
  xlab("Year sold") +
  ylab("Total Volume Sold")

df_totals <- subset(df, (region %in% regions_to_exclude))
