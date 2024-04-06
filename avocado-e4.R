# Importing libraries
library(lubridate)
library(ggplot2)
# Import Data:
df = read.csv("avocado.csv", header = TRUE)
# Removing unused columns:
drops <- c("X","X4046","X4225","X4770","Total.Bags","Small.Bags","Large.Bags", "XLarge.Bags")
df = df[ , !(names(df) %in% drops)]
# Preview first 2 rows of data
head(df,2)
# Check for null values in dataframe
na_count = sum(is.na(df))
sprintf("The dataset contains %d na values", na_count)
df$type = factor(df$type)
df$region = factor(df$region)
df$year = factor(df$year)
head(df,2)

#Feature Transformation
df$Date <- ymd(df$Date)
# Extract the month number
df$Month <- factor(month(df$Date))
head(df,2)

#EDA
ggplot(df, aes(AveragePrice, Total.Volume, colour = type))+ 
  geom_point()+
  ggtitle("Volume against price and type")+
  xlab("Average Price")+
  ylab("Total Volume Sold")

ggplot(df, aes(year, Total.Volume, colour = type)) +
  geom_point()+
  ggtitle("Volume against year and type")+
  xlab("Year sold")+
  ylab("Total Volume Sold")

ggplot(subset(df, type == "conventional"), aes(AveragePrice, Total.Volume, colour = year)) +
  geom_point() +
  ggtitle("Volume against price for conventional") +
  xlab("Average Price") +
  ylab("Total Volume Sold")

ggplot(subset(df, type == "organic"), aes(AveragePrice, Total.Volume, colour = year)) +
  geom_point() +
  ggtitle("Volume against price for organic") +
  xlab("Average Price") +
  ylab("Total Volume Sold")

regions_to_include <- c("West", "SouthCentral", "SouthEast", 
                        "Midsouth", "GreatLakes", "Southeast", "Northeast",
                        "Plains")


# # Filter the dataframe to include the specified regions
df_filtered <- subset(df, (region %in% regions_to_include))

# Filter the dataframe to make one for organic and one for conventional

df_filtered_c <- subset(df_filtered, (type %in% "conventional"))
df_filtered_o <- subset(df_filtered, type %in% "organic")

# Once removing these results, the results become more consistent

ggplot(subset(df_filtered, type == "organic"), aes(AveragePrice, Total.Volume, colour = year)) +
  geom_point() +
  ggtitle("Volume against price for filtered (organic)") +
  xlab("Average Price") +
  ylab("Total Volume Sold")

ggplot(subset(df_filtered, type == "conventional"), aes(AveragePrice, Total.Volume, colour = year)) +
  geom_point() +
  ggtitle("Volume against price for filtered (conventional)") +
  xlab("Average Price") +
  ylab("Total Volume Sold")


#model fitting
full_model_wo_int <- lm(Total.Volume ~ type + AveragePrice + year + Month + region , data = df_filtered)
ggplot(full_model_wo_int, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals") +
  ggtitle("Residuals vs Fitted")
ggplot(full_model_wo_int, aes(sample = .stdresid)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", y = "Standardized Residuals") +
  ggtitle("Normal Q-Q Plot")
full_model_w_int <- lm(Total.Volume ~ type * AveragePrice * year * Month * region , data = df_filtered)
ggplot(full_model_w_int, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals") +
  ggtitle("Residuals vs Fitted")
ggplot(full_model_w_int, aes(sample = .stdresid)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", y = "Standardized Residuals") +
  ggtitle("Normal Q-Q Plot")
full_model_w_int_log <- lm(log(Total.Volume) ~ type * AveragePrice * year * Month * region , data = df_filtered)
ggplot(full_model_w_int_log, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals") +
  ggtitle("Residuals vs Fitted")
ggplot(full_model_w_int_log, aes(sample = .stdresid)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", y = "Standardized Residuals") +
  ggtitle("Normal Q-Q Plot")

#model selection
partial_model_w_int_log1 <- lm(log(Total.Volume) ~ type + AveragePrice * region * year * Month, data = df_filtered)
partial_model_w_int_log2 <- lm(log(Total.Volume) ~ AveragePrice + type * region * year * Month, data = df_filtered)
partial_model_w_int_log3 <- lm(log(Total.Volume) ~ region + type * AveragePrice * year * Month, data = df_filtered)
partial_model_w_int_log4 <- lm(log(Total.Volume) ~ year + type * AveragePrice * region * Month, data = df_filtered)
partial_model_w_int_log5 <- lm(log(Total.Volume) ~ Month + type * AveragePrice * region * year, data = df_filtered)

cat("\n AIC for model without type interection = ", AIC(partial_model_w_int_log1))
cat("\n AIC for model without AveragePrice interection = ", AIC(partial_model_w_int_log2))
cat("\n AIC for model without region interection = ", AIC(partial_model_w_int_log3))
cat("\n AIC for model without year interection = ", AIC(partial_model_w_int_log4))
cat("\n AIC for model without Month interection = ", AIC(partial_model_w_int_log5))
cat("\n AIC for full interactive model = ", AIC(full_model_w_int_log))

