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
# while doing this, we will also make two dataframes, one for conventional, and
# one for organic

regions_to_exclude <- c("TotalUS", "West", "SouthCentral", "SouthEast", 
                        "Midsouth", "GreatLakes", "Southeast", "Northeast",
                        "Plains")


# Filter the dataframe to exclude the specified regions
df_filtered <- subset(df, !(region %in% regions_to_exclude))
df_totals <- subset(df, (region %in% regions_to_exclude))

# Filter the dataframe to make one for organic and one for conventional

df_filtered_c <- subset(df_filtered, (type %in% "conventional"))
df_filtered_o <- subset(df_filtered, type %in% "organic")

# Once removing these results, the results become more consistent

ggplot(subset(df_filtered, type == "organic"), aes(year, Total.Volume, colour = season)) +
  geom_point() +
  ggtitle("Volume against year for filtered (organic)") +
  xlab("Year sold") +
  ylab("Total Volume Sold")

ggplot(subset(df_filtered, type == "conventional"), aes(year, Total.Volume, colour = season)) +
  geom_point() +
  ggtitle("Volume against year for filtered (conventional)") +
  xlab("Year sold") +
  ylab("Total Volume Sold")

# Now that the data is more uniform, let's perform some basic analysis

model <- lm(Total.Volume ~ type + region + Month + year + AveragePrice, data = df_filtered)

# Summarize the model
summary(model)

# Lets create some plots to analyze the fit of the model

ggplot(model, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals") +
  ggtitle("Residuals vs Fitted")

# The residual vs fitted plot shows patterns, but there does seem to be two
# distinct-ish groups

ggplot(model, aes(sample = .stdresid)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", y = "Standardized Residuals") +
  ggtitle("Normal Q-Q Plot")

# The qq plot does seem to be roughly normal, however, does seem slightly
# over-dispersed, shown by the two ends, meaning that there may be many outlier

# as the residual vs fitted plot does seem to show two sperate patterns, lets
# perform the analysis on organic and conventional data seperately

model_c <- lm(Total.Volume ~ region + Month + year + AveragePrice, data = df_filtered_c)

summary(model_c)

model_o <- lm(Total.Volume ~ region + Month + year + AveragePrice, data = df_filtered_o)

summary(model_o)

# Analysis for model with conventional

ggplot(model_c, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals") +
  ggtitle("Residuals vs Fitted")

ggplot(model_c , aes(sample = .stdresid)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", y = "Standardized Residuals") +
  ggtitle("Normal Q-Q Plot")

# Analysis for model with organic

ggplot(model_o, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals") +
  ggtitle("Residuals vs Fitted")

ggplot(model_o, aes(sample = .stdresid)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", y = "Standardized Residuals") +
  ggtitle("Normal Q-Q Plot")

# seperating the data makes both the conventional, and organic data sets look
# much more normal for both the residual vs fitted and the qq plots


